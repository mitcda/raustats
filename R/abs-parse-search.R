### ABS Catalogue functions
#' @name abs_parse_search
#' @title Parse ABS search function results
#' @description `r lifecycle::badge("experimental")` This function parses the
#'     results of the ABS search results and returns as a data frame.
#' @importFrom magrittr %>%
#' @importFrom rvest follow_link html_attr html_nodes html_node html_session
#'     html_text is.session
#' @param s a session object
#' @param resource One or more of \code{'Statistical analysis and data'} (the
#'     default) or \code{'Article'}.
#' @return a data frame containg the ABS Search function results
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS search helper functions
abs_parse_search <- function(s, resource = c("Statistical analysis and data", "Article"))
{
## abs_cat_search("gross domestic product", date_range="Past 3 months");
  if (!is.session(s))
    stop("s should be a valid session object")
  ## Return all 'search-results'
  search_results <- html_nodes(s, xpath = "//ul[@id='search-results']");
  ## Include only selected 'resource' nodes
  stat_results <- #paste(
    html_nodes(search_results,
               xpath = sapply(resource,
                              function(x) sprintf(".//li[contains(.,%s)]", shQuote(x))))
#, collapse="|"); ## Return search list
  .search_list <- html_node(stat_results, xpath = ".//*[contains(@href,'redirect')]");
  ## Return data frame of search results
  z <- data.frame(
    ## Return release dates (Note: xpath expression './/*' returns from current root node)
    release_date = 
      trimws(
        sub("^Released:\\s*(.+)$", "\\1",
            html_text(
              html_nodes(stat_results, xpath = ".//*[starts-with(., 'Released:')]")),
            ignore.case=TRUE)),
    ## Return redirect address
    redirect_addr = sprintf("%s%s", abs_urls()$search_url, 
                            html_attr(.search_list, "href")),
    ## Return series name
    title = trimws(html_text(.search_list)),
    ## Return url path
    url = html_attr(.search_list, "title"),
    ## Reference period
    reference_period =
      sub("^Reference period:\\s*(.+)\\s*$", "\\1",
          unlist(
            lapply(stat_results,
                   function(x) {
                     z <- html_text(html_nodes(x, xpath = ".//*[starts-with(., 'Reference period:')]"))
                     z <- ifelse(identical(z, character(0)), NA, z)
                     return(z)
                   })),
          ignore.case=TRUE),
    ## -- pipe-based method --
    ## reference_period = stat_results %>%
    ##   lapply(. %>% html_nodes(xpath = ".//*[starts-with(., 'Reference period:')]") %>%
    ##          html_text %>%
    ##          ifelse(identical(., character(0)), NA, .)) %>%
    ##   unlist %>%
    ##   sub("^Reference period:\\s*(.+)\\s*$", "\\1", ., ignore.case=TRUE),
    ## -- END pipe-based method --
    ## Resource type
    resource =
      html_text(
        ## paste(
          html_nodes(stat_results,
                     xpath = sapply(resource,
                                    function(x) sprintf(".//*[starts-with(., %s)]", shQuote(x))))
        ## , collapse="|")));
      ),
    stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
  ## Return results
  return(z[,c("title","reference_period","release_date","url","resource")]); # "redirect_addr"
}

## ----------------------------------- EOF ---------------------------------- ##
