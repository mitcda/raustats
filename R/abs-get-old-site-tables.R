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
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_get_old_site_tables <- function(url) {
  if (FALSE) {
    url <- "https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6345.0Main+Features1Jun 2019?OpenDocument="
  }
  url <- gsub("\\s", "%20", url)
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
  all_nodes <-
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
                         gsub(abs_expressions()$regex_nbsp, "", .)); ## Remove non-breaking spaces
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

 
  ## Return results
  return(all_nodes[,c("table_name", "file_url", "file_type", "file_size")]);
  
  ## ## Tidy and return data set names and urls
  ## nodes <- data_nodes[unlist(lapply(data_nodes,
  ##                                   function(x) {
  ##                                     any(
  ##                                       grepl(sprintf("(%s)",
  ##                                                     paste(types, collapse="|")),
  ##                                             x, ignore.case=TRUE) &
  ##                                       grepl("ausstats", x, ignore.case=TRUE))
  ##                                   }))];
  ## ## Remove non-breaking spaces (&nbsp;), and blank entries
  ## nodes <- lapply(nodes,
  ##                 function(x) {
  ##                   z <- trimws(gsub("\u00a0", "", x));      ## Remove non-breaking spaces
  ##                   z <- replace(z, z == "", NA_character_); ## Replace blank objects with NA
  ##                   ## Set entries not starting with 'https*' with 'NA_character_'
  ##                   z[-1] <- replace(z[-1],                          
  ##                                    !grepl("^https*.+", z[-1], ignore.case=TRUE),
  ##                                    NA_character_);
  ##                   ## Set entries containing 'INotes' with 'NA_character_'
  ##                   z <- replace(z,                          
  ##                                grepl("INotes", z, ignore.case=TRUE),
  ##                                NA_character_);
  ##                   z <- z[!is.na(z)];                       ## Remove NA objects
  ##                   ## Set object names: First element = 'item_name'
  ##                   names(z)[1] <- "item_name";
  ##                   names(z)[-1] <- case_when(
  ##                     ## !grepl("(^https*|^Releases|INotes)", z, ignore.case=TRUE) ~ "item_name",
  ##                     grepl("\\.xlsx*", z[-1], ignore.case=TRUE) ~ "path_xls",
  ##                     grepl("\\.zip", z[-1], ignore.case=TRUE) ~ "path_zip",
  ##                     grepl("\\.pdf", z[-1], ignore.case=TRUE) ~ "path_pdf",
  ##                     TRUE ~ NA_character_)
  ##                   z <- as.data.frame(t(cbind.data.frame(z, deparse.level=1)),
  ##                                      stringsAsFactors=FALSE);
  ##                   return(z);
  ##                 });
  ## ## Tidy nodes into data.frame (using dplyr::bind_rows)
  ## dt <- suppressWarnings(bind_rows(nodes))
  ## ## Lastly replace spaces in all URL paths with '%20' string
  ## for(name in grep("^path_", names(dt), ignore.case=TRUE, value=TRUE)) # names(dt)[-1]
  ##   dt[,name] <- gsub("\\s+", "%20", dt[,name]);
  ## return(dt);
  
  ## v <- lapply(.paths,
  ##             function(x) {
  ##               ## Check for HTTP errors
  ##               y <- jump_to(s, x)
  ##               l <- follow_link(y, abs_urls()$downloads_regex)
  ##               ht <- html_nodes(html_nodes(l, "table"), "table")
  ##               ## Return data table
  ##               ## The ABS data catalogue lists the data inside a HTML table within a table, i.e.
  ##               ##  <table>
  ##               ##    <table> </table>
  ##               ##  </table>
  ##               ## The following nested apply functions, exploits this structure to extract the
  ##               ## list of available publication types and associated links.
  ##               all_nodes <- lapply(sapply(ht, function(x) html_nodes(x, "tr")),
  ##                                   function(x)
  ##                                     c(html_text(html_nodes(x, "td")),
  ##                                       ## html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")));
  ##                                       paste0(abs_urls()$base_url,
  ##                                              html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")))
  ##                                   );
  ##               ## Remove ABS data download section heading from all_nodes
  ##               ##   Where ABS data download section titles that include links, are included
  ##               ##   in the node set, but are not conformant with publication information.
  ##               ##   The following code block, removes these entries.
  ##               data_nodes <- lapply(all_nodes,
  ##                                    function(x) {
  ##                                      if (grepl(paste(c("(^\\W{0,1}$)",
  ##                                                        "(^publications\\W*$)",
  ##                                                        "(^data\\s*cubes\\W*$)",
  ##                                                        "(^time\\s*series\\s*spreadsheets?\\W*$)"),
  ##                                                      collapse="|"),
  ##                                                x[1], ignore.case=TRUE)) {
  ##                                        NULL
  ##                                        ## Also drop SuperTABLE (srd) format files
  ##                                      } else if (any(grepl("\\.srd", x, ignore.case=TRUE))) {
  ##                                        NULL
  ##                                      } else {
  ##                                        x
  ##                                      }
  ##                                    })
  ##               data_nodes <- data_nodes[-which(sapply(data_nodes, is.null))];
  ##               ## Tidy and return data set names and urls
  ##               nodes <- data_nodes[unlist(lapply(data_nodes,
  ##                                                 function(x) {
  ##                                                   any(
  ##                                                     grepl(sprintf("(%s)",
  ##                                                                   paste(types, collapse="|")),
  ##                                                           x, ignore.case=TRUE) &
  ##                                                     grepl("ausstats", x, ignore.case=TRUE))
  ##                                                 }))];
  ##               ## Remove non-breaking spaces (&nbsp;), and blank entries
  ##               nodes <- lapply(nodes,
  ##                               function(x) {
  ##                                 z <- trimws(gsub("\u00a0", "", x));      ## Remove non-breaking spaces
  ##                                 z <- replace(z, z == "", NA_character_); ## Replace blank objects with NA
  ##                                 ## Set entries not starting with 'https*' with 'NA_character_'
  ##                                 z[-1] <- replace(z[-1],                          
  ##                                                  !grepl("^https*.+", z[-1], ignore.case=TRUE),
  ##                                                  NA_character_);
  ##                                 ## Set entries containing 'INotes' with 'NA_character_'
  ##                                 z <- replace(z,                          
  ##                                              grepl("INotes", z, ignore.case=TRUE),
  ##                                              NA_character_);
  ##                                 z <- z[!is.na(z)];                       ## Remove NA objects
  ##                                 ## Set object names: First element = 'item_name'
  ##                                 names(z)[1] <- "item_name";
  ##                                 names(z)[-1] <- case_when(
  ##                                   ## !grepl("(^https*|^Releases|INotes)", z, ignore.case=TRUE) ~ "item_name",
  ##                                   grepl("\\.xlsx*", z[-1], ignore.case=TRUE) ~ "path_xls",
  ##                                   grepl("\\.zip", z[-1], ignore.case=TRUE) ~ "path_zip",
  ##                                   grepl("\\.pdf", z[-1], ignore.case=TRUE) ~ "path_pdf",
  ##                                   TRUE ~ NA_character_)
  ##                                 z <- as.data.frame(t(cbind.data.frame(z, deparse.level=1)),
  ##                                                    stringsAsFactors=FALSE);
  ##                                 return(z);
  ##                               });
  ##               ## Tidy nodes into data.frame (using dplyr::bind_rows)
  ##               dt <- suppressWarnings(bind_rows(nodes))
  ##               ## Lastly replace spaces in all URL paths with '%20' string
  ##               for(name in grep("^path_", names(dt), ignore.case=TRUE, value=TRUE)) # names(dt)[-1]
  ##                 dt[,name] <- gsub("\\s+", "%20", dt[,name]);
  ##               return(dt);
  ##             });
  ## ## Add catalogue number and release information to table
  ## v <- lapply(seq_along(v),
  ##             function(i) {
  ##               v[[i]]$release <- sub("^$", "Latest", releases[i]);
  ##               v[[i]]$cat_no <- cat_no;
  ##               as.data.frame(v)
  ##             });
  ## ## Bind all results together
  ## z <- do.call(rbind, v);
}

## ----------------------------------- EOF ---------------------------------- ##
