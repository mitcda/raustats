#' @name abs_get_old_site_tables
#' @title Navigates ABS AUSSTATS style pages to return list of tables for specified ABS publication
#' @description Return list of data tables available for specified ABS data series.
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom tidyr fill everything
#' @importFrom rvest follow_link html_attr html_nodes html_session html_text
#' @param url ABS statistical release URL.
#' @return Returns a data frame listing the data collection tables and URLs for Excel (column:
#'   \code{path_xls}) and, if available, Zip (column: \code{path_zip}) files.
#' @export
#' @family ABS catalogue helper functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_get_old_site_tables <- function(url) {
  if (FALSE) {
    url <- "https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6345.0Main+Features1Jun 2019?OpenDocument="
  }
  ## -- DEPRECATED - INCLUDED IN abs_cat_releases() --
  ## Replace all whitespace in URLs - 
  url <- gsub("\\s", "%20", url)
  ## -- END DEPRECATED --
  ## Check for HTTP errors
  raustats_check_url_available(url);
  ## Open html session
  suppressWarnings(s <- html_session(url));
  ## Navigate to Downloads page
  r <- s %>% follow_link(abs_urls()$downloads_regex);
  ## Return list of all downloadable files, for specified catalogue tables ('cat_tables')
  ht <- r %>% html_nodes("table") %>% html_nodes("table");
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
                  function(x) html_nodes(x, xpath="//tr[@class='listentry']")), 
           function(x) {
             ## Extract ABS table information
             z <- list(table_name = x %>%
                         html_nodes("td") %>% html_text %>%
                         gsub(abs_expressions()$regex_nbsp, "", .), ## Remove non-breaking spaces
                       file_url = paste0(abs_urls()$base_url,
                                         x %>% html_nodes("td") %>%
                                         html_nodes("a") %>%
                                         html_attr("href")) %>%
                         sapply(function(x) gsub("\\s", "%20", x)),
                       series_type = x %>% html_nodes("td") %>%
                         html_nodes("a") %>%
                         html_attr("href") %>%
                         sub(sprintf("^.*(%s|%s|%s).*$",
                                     "publications", "data\\s*cubes",
                                     "time\\s*series\\s*spreadsheets?"),
                             "\\1", ., ignore.case=TRUE),
                       file_type = x %>% html_nodes("td") %>%
                         html_nodes("a") %>% html_nodes("img") %>%
                         html_attr("alt") %>%
                         sub("Download\\s*(.+File).*", "\\1", .),
                       file_size = x %>% html_nodes("td") %>%
                         html_nodes("a") %>% html_nodes("img") %>%
                         html_attr("alt") %>%
                         sub(".*\\((.+)\\s*(.+)\\)", "\\1\\2", .) %>%
                         gsub(abs_expressions()$regex_nbsp, "", .), ## Remove non-breaking spaces
                       file_name = x %>% html_nodes("td") %>%
                         html_nodes("a") %>%
                         html_attr("href") %>%
                         sapply(function(x) sub(".+&(.+\\.(xlsx*|zip))&.+", "\\1", x)));
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
           }) %>%
    do.call(rbind, .);
  ## Return results
  return(abs_tables[,c("table_name", "file_url", "file_name", "file_type", "file_size")]);
}

## ----------------------------------- EOF ---------------------------------- ##

## ## Remove ABS data download section heading from all_nodes
## ##   Where ABS data download section titles that include links, are included
## ##   in the node set, but are not conformant with publication information.
## ##   The following code block, removes these entries.
## data_nodes <- lapply(all_nodes,
##                      function(x) {
##                        if (grepl(paste(c("(^\\W{0,1}$)",
##                                          "(^publications\\W*$)",
##                                          "(^data\\s*cubes\\W*$)",
##                                          "(^time\\s*series\\s*spreadsheets?\\W*$)"),
##                                        collapse="|"),
##                                  x[1], ignore.case=TRUE)) {
##                          NULL
##                          ## Also drop SuperTABLE (srd) format files
##                        } else if (any(grepl("\\.srd", x, ignore.case=TRUE))) {
##                          NULL
##                        } else {
##                          x
##                        }
##                      })
##
