#' @name abs_get_new_site_tables
#' @title Returns list of tables for specified ABS title and release.
#' @description Return list of data tables available for specified ABS data series.
#' @importFrom dplyr bind_rows
#' @importFrom rvest follow_link html_attr html_nodes html_session html_text
#' @param url ABS statistical release URL.
#' @return Returns a data frame listing the statistical collection tables, URLs, file type and file
#'   size.
#' @export
#' @family ABS catalogue helper functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_get_new_site_tables <- function(url) {
  if (FALSE) {
    url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/sep-2020"
    url <- "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/latest-release"
    xx <- abs_get_new_site_tables(url)
  }
  ## Check for HTTP errors
  raustats_check_url_available(url);
  ## Open html session
  suppressWarnings(s <- html_session(url));
  ## -- Statistical tables --
  table_nodes <- s %>% html_nodes(xpath = ".//span[contains(@class, 'download_link')]");
  abs_tables <-
    lapply(table_nodes,
           function(x) {
             z <- list(table_name = x %>%
                         html_nodes("a") %>% html_attr("aria-label") %>%
                         sub("(\\s*xls|zip)", "", ., ignore.case=TRUE) %>%
                         sub("(\\s\\d+\\.*\\d*\\s\\w{2})$", "", ., ignore.case=TRUE),
                       file_url = x %>%
                         html_nodes("a") %>% html_attr("href"),
                       file_type = x %>%
                         html_nodes("a") %>% html_attr("type") %>%
                         sub("(.+);\\s*length=(.+)", "\\1", .),
                       file_size = x %>%
                         html_nodes("a") %>% html_attr("type") %>%
                         sub("(.+);\\s*length=(.+)", "\\2", .) %>%
                         as.numeric %>% `/`(1024),
                       file_name = x %>%
                         html_nodes("a") %>% html_attr("title")
                       ) %>% as.data.frame
             return(z)
           }) %>%
    bind_rows;
  ## Return data frame
  return(abs_tables[,c("table_name","file_url","file_name","file_type","file_size")]);
}

## ----------------------------------- EOF ---------------------------------- ##
