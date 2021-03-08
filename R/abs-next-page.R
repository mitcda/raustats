### ABS website scraping function
#' @name abs_next_page
#' @title Parse ABS search function results
#' @description
#'   `r lifecycle::badge("experimental")`
#'   This function parses the results of the ABS search results and returns as a data
#'   frame.
#' @importFrom magrittr %>%
#' @importFrom rvest html_attr html_nodes is.session
#' @param s a valid session object (see \link[rvest]{html_session}).
#' @return a url to linking to the next page returned by an ABS search results
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS catalogue helper functions
abs_next_page <- function(s) {
  if (!is.session(s))
    stop("s is not a valid session object");
  ## Return search result next page address
  z <- s %>%
    html_nodes(xpath = "//li[@class='pager__item']") %>%
    html_nodes(xpath = ".//*[starts-with(.,'Next')]") %>%
    html_attr("href") %>%
    sprintf("%s/%s/%s", abs_urls()$search_url, "s", .);
  ## Check for HTTP errors
  if (!is.null(raustats_check_url_available(z)))
    stop("Next page is not a valid URL.");
  ## Return results
  return(z)
}

## ----------------------------------- EOF ---------------------------------- ##
