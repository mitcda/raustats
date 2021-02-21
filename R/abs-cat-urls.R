### ABS Catalogue functions
#' @name abs_urls
#' @title ABS URL addresses and paths used in accessing ABS Catalogue data calls
#' @description This function returns a list of URLs and data paths used to construct ABS Catalogue
#'   data access calls. It is used in other functions in this package and need not be called
#'   directly.
#' @return a list with a base url and a url section for formatting ABS Catalogue statistics calls
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS catalogue functions
#' @keywords internal
abs_urls <- function()
{
  list(base_url = "https://www.abs.gov.au",
       ## Old site paths
       ausstats_path = "ausstats/abs@.nsf",
       mf_path = "mf",
       ## New site paths
       statistics_path = "statistics",
       ## Old site regular expressions
       downloads_regex = "Downloads",
       releases_regex = "Past.*Future.*Releases",
       ## New site search paths
       search_url = "https://search.abs.gov.au",
       search_path = ""
       );
}


### ABS expression functions
#' @name abs_expressions
#' @title Common expressions used in scraping the ABS website
#' @description This function returns a common set of expressions used in scraping the ABS website.
#' @return a list with a set of XPath expressions.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS catalogue functions
#' @keywords internal
abs_expressions <- function()
{
  list(
    ## New site XPath expressions
    xpath_release_links_class = "abs-all-releases-link",
    xpath_release_views_row = "views-row",
    ## Old site release type regular expression
    regex_release_type = ".*(Archive\\s*release|Release\\s*date|Latest\\s*release).*",
    regex_nbsp = "\u00a0"
  );
}
