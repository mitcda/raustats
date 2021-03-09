#' @name abs_get_old_site_tables
#' @title Navigates ABS AUSSTATS style pages to return list of tables for specified ABS publication
#' @description Return list of data tables available for specified ABS data series.
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom tidyr fill everything
#' @importFrom rvest follow_link html_attr html_nodes html_session html_text
#' @param url ABS statistical release URL.
#' @return Returns a data frame listing the data collection tables and URLs.
#' @export
#' @family ABS catalogue helper functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_get_old_site_tables <- function(url)
{
  ## if (FALSE) {
  ##   url <- "https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6345.0Main+Features1Jun 2019?OpenDocument="
  ## }
  ## Replace all whitespace in URLs - 
  url <- gsub("\\s", "%20", url)
  ## -- END DEPRECATED --
  ## Check for HTTP errors
  raustats_check_url_available(url);
  ## Open html session
  suppressWarnings(s <- html_session(url));
  ## Navigate to Downloads page
  r <- follow_link(s, abs_urls()$downloads_regex);
  ## Return list of all downloadable files, for specified catalogue tables ('cat_tables')
  ht <- html_nodes(html_nodes(r, "table"), "table");
  ## Return data table
  ## The ABS data catalogue lists the data inside a HTML table within a table, i.e.
  ##  <table>
  ##    <table> </table>
  ##  </table>
  ## The following nested apply functions, exploits this structure to extract the
  ## list of available publication types and associated links.
  abs_tables <-
    lapply(sapply(ht,
                  ## Filter 'tr' nodes where attribute class='listentry'
                  function(y) x <- html_nodes(y, xpath="//tr[@class='listentry']")), 
           function(x) {
             ## Extract ABS table information
             z <- list(table_name = gsub(abs_expressions()$regex_nbsp, "",
                                         html_text(html_nodes(x, "td"))), ## Remove non-breaking spaces
                       file_url = 
                         sapply(paste0(abs_urls()$base_url,
                                       html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")),
                                function(x) gsub("\\s", "%20", x)),
                       series_type = 
                         sub(sprintf("^.*(%s|%s|%s).*$",
                                     "publications", "data\\s*cubes",
                                     "time\\s*series\\s*spreadsheets?"),
                             "\\1",
                             html_attr(html_nodes(html_nodes(x, "td"), "a"), "href"),
                             ignore.case=TRUE),
                       file_type = 
                         sub("Download\\s*(.+File).*", "\\1",
                             html_attr(html_nodes(html_nodes(html_nodes(x, "td"), "a"), "img"), "alt")),
                       file_size = 
                         gsub(abs_expressions()$regex_nbsp, "",       ## Remove non-breaking spaces
                              sub(".*\\((.+)\\s*(.+)\\)", "\\1\\2",
                                  html_attr(html_nodes(html_nodes(html_nodes(x, "td"),
                                                                  "a"),
                                                       "img"),
                                            "alt"))),
                       file_name = 
                         sapply(html_attr(html_nodes(html_nodes(x, "td"), "a"), "href"),
                                function(x) sub(".+&(.+\\.(xlsx*|zip))&.+", "\\1", x))
                       );
             ## file_url = paste0(abs_urls()$base_url,
             ##                   x %>% html_nodes("td") %>%
             ##                   html_nodes("a") %>%
             ##                   html_attr("href")) %>%
             ##   sapply(function(x) gsub("\\s", "%20", x)),
             ## series_type = x %>% html_nodes("td") %>%
             ##   html_nodes("a") %>%
             ##   html_attr("href") %>%
             ##   sub(sprintf("^.*(%s|%s|%s).*$",
             ##               "publications", "data\\s*cubes",
             ##               "time\\s*series\\s*spreadsheets?"),
             ##       "\\1", ., ignore.case=TRUE),
             ## file_type = x %>% html_nodes("td") %>%
             ##   html_nodes("a") %>% html_nodes("img") %>%
             ##   html_attr("alt") %>%
             ##   sub("Download\\s*(.+File).*", "\\1", .),
             ## file_size = x %>% html_nodes("td") %>%
             ##   html_nodes("a") %>% html_nodes("img") %>%
             ##   html_attr("alt") %>%
             ##   sub(".*\\((.+)\\s*(.+)\\)", "\\1\\2", .) %>%
             ##   gsub(abs_expressions()$regex_nbsp, "", .), ## Remove non-breaking spaces
             ## file_name = x %>% html_nodes("td") %>%
             ##   html_nodes("a") %>%
                       ##   html_attr("href") %>%
             ##   sapply(function(x) sub(".+&(.+\\.(xlsx*|zip))&.+", "\\1", x)));
             ## Balance number of elements returned
             z <- lapply(z,
                         function(x) {
                           x <- x[x != ""];
                           ## Convert a list of different length vectors to a data frame:
                           ## (See: https://stackoverflow.com/questions/15201305/how-to-convert-a-list-consisting-of-vector-of-different-lengths-to-a-usable-data)
                           x <- sapply(x, '[', seq(max(sapply(x, length))));
                           return(x)
                         });
             z <- suppressWarnings(as.data.frame(z, optional=TRUE));
             return(z);
           });
  abs_tables <- do.call(rbind, abs_tables);
  ## Return results
  return(abs_tables[,c("table_name", "file_url", "file_name", "file_type", "file_size")]);
}

## ----------------------------------- EOF ---------------------------------- ##
