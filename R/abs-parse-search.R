### ABS Catalogue functions ' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}:
#' @name abs_parse_search
#' @title Parse ABS search function results
#' @description This function parses the results of the ABS search results and returns as a data
#'   frame.
#' @importFrom magrittr %>%
#' @importFrom rvest follow_link html_attr html_nodes html_node html_session html_text is.session
#'   jump_to
#' @param s a session object
#' @param resource One or more of \code{'Statistical analysis and data'} (the default) or
#'   \code{'Article'}.
#' @return a data frame containg the ABS Search function results
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS search helper functions
abs_parse_search <- function(s, resource = c("Statistical analysis and data", "Article")) {
  if (!is.session(s))
    stop("s should be a valid session object")
  ## Return all 'search-results'
  search_results <- html_nodes(s, xpath = "//ul[@id='search-results']");
  ## Include only selected 'resource' nodes
  stat_results <- search_results %>%
    html_nodes(xpath = sapply(resource,
                              function(x) sprintf(".//li[contains(.,%s)]", shQuote(x))) %>%
                 paste(., collapse="|")); ## Return search list
  .search_list = stat_results %>%
    html_node(xpath = ".//*[contains(@href,'redirect')]");
  ## Return data frame of search results
  z <- list(
    ## Return release dates (Note: xpath expression './/*' returns from current root node)
    release_date = stat_results %>%
      html_nodes(xpath = ".//*[starts-with(., 'Released:')]") %>%
      html_text %>% sub("^Released:\\s*(.+)$", "\\1", ., ignore.case=TRUE) %>% trimws,
    ## Return redirect address
    redirect_addr = .search_list %>%
      html_attr("href") %>% sprintf("%s%s", abs_urls()$search_url, .),
    ## Return series name
    series_name = .search_list %>% html_text %>% trimws,
    ## Return url path
    url_path = .search_list %>% html_attr("title"),
    ## Reference period
    reference_period = stat_results %>%
      lapply(. %>% html_nodes(xpath = ".//*[starts-with(., 'Reference period:')]") %>%
             html_text %>%
             ifelse(identical(., character(0)), NA, .)) %>% unlist %>%
      sub("^Reference period:\\s*(.+)\\s*$", "\\1", ., ignore.case=TRUE),
    ## Resource type
    resource_type = stat_results %>%
      html_nodes(xpath = sapply(resource,
                                function(x) sprintf(".//*[starts-with(., %s)]", shQuote(x))) %>%
                   paste(., collapse="|")) %>%
      html_text) %>%
    as.data.frame;
  ## Return results
  return(z);
}

## ----------------------------------- EOF ---------------------------------- ##
