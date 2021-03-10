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
abs_get_new_site_tables <- function(url)
{
  ## if (FALSE) {
  ##   url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/sep-2020"
  ##   url <- "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/latest-release"
  ##   url <- "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/sep-2020"
  ##   xx <- abs_get_new_site_tables(url)
  ## }
  ## Check for HTTP errors
  raustats_check_url_available(url);
  ## Open html session
  suppressWarnings(s <- html_session(url));
  ## -- Statistical tables --
  table_nodes <- html_nodes(s, xpath = ".//span[contains(@class, 'download-link')]");
  abs_tables <-
    lapply(table_nodes,
           function(x) {
             z <- data.frame(table_name = sub("(\\s\\d+\\.*\\d*\\s\\w{2})$", "",
                                              sub("(\\s*xls|zip)", "",
                                                  html_attr(html_nodes(x, "a"), "aria-label"),
                                                  ignore.case=TRUE),
                                              ignore.case=TRUE),
                             file_url = html_attr(html_nodes(x, "a"), "href"),
                             file_type = sub("(.+);\\s*length=(.+)", "\\1",
                                             html_attr(html_nodes(x, "a"), "type")),
                             file_size = as.numeric(
                               sub("(.+);\\s*length=(.+)", "\\2",
                                   html_attr(html_nodes(x, "a"), "type")))/1024,
                             file_name = html_attr(html_nodes(x, "a"), "title"),
                             stringsAsFactors=FALSE)
             return(z)
           });
    abs_tables <- do.call(rbind, abs_tables);
  ## Return data frame
  return(abs_tables[,c("table_name","file_url","file_name","file_type","file_size")]);
}

## ----------------------------------- EOF ---------------------------------- ##
