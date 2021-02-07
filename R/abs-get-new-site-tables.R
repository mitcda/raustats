#' @name abs_get_new_site_tables
#' @title Returns list of tables for specified ABS title and release.
#' @description Return list of data tables available for specified ABS data series.
#' @importFrom dplyr bind_rows
#' @importFrom rvest follow_link html_attr html_nodes html_session html_text
#' @param url ABS statistical release URL.
#' @return Returns a data frame listing the statistical collection tables, URLs, file type and file
#'   size.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_get_new_site_tables <- function(url) {
  if (FALSE) {
    url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/sep-2020"
  }
  ## Check for HTTP errors
  raustats_check_url_available(url);
  ## Open html session
  suppressWarnings(s <- html_session(url));
  ## -- Statistical tables --
  table_nodes <- s %>% html_nodes(xpath = ".//div[contains(@class, 'abs-data-download-content')]");
  table_data <-
    lapply(table_nodes,
           function(x) {
             z <- list(table_name = x %>%
                         html_nodes(xpath = ".//div[contains(@class, 'abs-data-download-left')]") %>%
                         html_nodes("h3") %>% html_text,
                       file_url = x %>%
                         html_nodes(xpath = ".//div[contains(@class, 'abs-data-download-right')]") %>%
                         html_nodes("a") %>% html_attr("href"),
                       file_type = x %>%
                         html_nodes(xpath = ".//div[contains(@class, 'abs-data-download-right')]") %>%
                         html_nodes("a") %>% html_attr("type") %>%
                         sub("(^.+);\\s+length=\\d+", "\\1", .),
                       file_size = x %>%
                         html_nodes(xpath = ".//div[contains(@class, 'abs-data-download-right')]") %>%
                         html_nodes(xpath = ".//span[contains(@class, 'size')]") %>%
                         html_text %>%
                         sub("\\[(.+)\\]", "\\1", .)
                       ) %>% as.data.frame
             return(z)
           }) %>%
    bind_rows;
  ## Return data frame
  return(table_data);
}

## ----------------------------------- EOF ---------------------------------- ##
