
#' @name abs_cat_search
#' @title Use ABS website search function
#' @description `r lifecycle::badge("experimental")`
#'   This function submits a search string to the ABS website search facility and returns
#'   the search results in a data frame.
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom rvest html_session
#' @importFrom stringi stri_replace_last_fixed
#' @param pattern A character string or named list specifying one or more search strings. If
#'   character string, the function invokes a \emph{simple} search. If named list, \code{abs_search}
#'   invoked an \emph{advanced} search. See \link{Details}.
#' @param resource One or more of \code{'Statistical analysis and data'} (the default) or
#'   \code{'Article'}.
#' @param refine_date Filter search results by named date range -- one of: 'Today', 'Past week',
#'   'Past fortnight', 'Past month', 'Past 3 months', 'Past 6 months', 'Past year', or single
#'   calendar year (e.g. '2021' or '2020'). Default is NULL. If either \code{start_date} or
#'   \code{end_date} is specified, then this argument will be ignored.
#' @param start_date Filter search results by start date. Specify as string '\%Y-\%m-\%d', Default is
#'   NULL.
#' @param end_date Filter search results by end date. Specify as string '\%Y-\%m-\%d', Default is NULL,
#' @param sort_by Sort by either 'Relevance' (default), 'Newest', 'Oldest', 'A-Z' or 'Z-A' (is not
#'   case senstive).
#' @param n_results The number of results to return on each page. Default is 10.
#' @param follow_links Specify integer number of sequential links to include in search
#'   results. Default is 5. (The maximum number of links the function will follow is 25.)
#' @return data frame in long format
#' @export
#' @family ABS search functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @details The ABS search facility supports multiple search string fields encompassing either 'Any'
#'   words, 'All' words, exact 'Phrase' and terms to exclude ('Not'). Advanced queries can be
#'   specified by supplying a named list of strings to argument \code{pattern}. List names should be
#'   one or more of more of 'any', 'all', 'phrase' and/or 'not'.
#' @examples
#'   \donttest{
#'     ## Example ABS search queries 
#'     abs_cat_search("gross domestic product", refine_date="Past 3 months");
#'     abs_cat_search(pattern=list(phrase="consumer price index"),
#'                    refine_date="Past 3 months");
#'     abs_cat_search(pattern=list(phrase="consumer price index"),
#'                    refine_date="Past 3 months", resource="Statistical analysis and data");
#'     abs_cat_search(pattern=list(phrase="consumer price index"),
#'                    refine_date="Past 3 months", resource="Statistical analysis and data");
#'     abs_cat_search(pattern=list(any="consumer price index"),
#'                    start_date='2001-10-15', end_date='2015-06-30');
#'     abs_cat_search(pattern=list(any="consumer price index"), sort_by='A-Z');
#'   }
abs_cat_search <- function(pattern,
                           resource = c("Statistical analysis and data", "Article"),
                           refine_date = NULL,
                           start_date = NULL, end_date = NULL,
                           sort_by = c('Relevance', 'Newest', 'Oldest', 'A-Z', 'Z-A'),
                           n_results = 10, follow_links = 5,
                           return_url = FALSE) {
  max_links <- 25L;
  valid_pattern_names <- c('any','all','phrase','not');
  valid_sort_list <- c('relevance', 'newest', 'oldest', 'a-z', 'z-a');
  ## DEBUGGING CODE
  ## if (FALSE) {
  ##   follow_links <- 5L;
  ##   pattern <- "gross domestic product";
  ##   pattern <- list(any="gross domestic product", all="gross domestic", phrase=NULL, not=NULL);
  ##   resource <- c("Statistical analysis and data", "Article");
  ##   resource <- c("Statistical analysis and data");
  ##   sort_by <- "Relevance";
  ##   start_date <- '2001-10-15';
  ##   end_date <- '2015-06-30';
  ##   n_results <- 10;
  ##   follow_links <- 5;
  ## }
  ## Check pattern provided
  if (is.null(pattern))
    stop("No pattern provided")
  if (!any(class(pattern) %in% c("character", "list")))
    stop("pattern must be of type 'character' or 'list'")
  if (class(pattern) == "list") {
    ## If no valid pattern list names, return error
    if (!all(names(pattern) %in% valid_pattern_names))
      stop(sprintf("When list, pattern must contain at least one named string from: %s",
                   stri_replace_last_fixed(paste(shQuote(valid_pattern_names), collapse=", "),
                                           ",", " or")));
    ## Warn of invalid pattern list names
    if (any(!names(pattern) %in% valid_pattern_names) )
      warning(sprintf("Invalid pattern list names: '%s' ignored.",
                      valid_pattern_names[which(!names(pattern) %in%
                                                valid_pattern_names)] %>%
                      { stri_replace_last_fixed(paste(., collapse=", "), ",", " and") }));
  }
  ## Check arguments: resource, sort_by, sort_order
  resource <- match.arg(resource, several.ok=TRUE);
  sort_by <- match.arg(tolower(sort_by),
                       choices=valid_sort_list);
  sort_by <- switch(sort_by,
                    relevance = "",
                    newest = 'date',
                    oldest = 'adate',
                    `a-z` = 'title',
                    `z-a` = 'dtitle');
  ## Check start_date & end_date format
  if (!is.null(start_date) & is.null(refine_date)) {
    d_start_date <- try(as.Date(start_date, format="%Y-%m-%d"))
    ## Warn if d_start_date is not valid date class
    if ("try-error" %in% class(d_start_date) || is.na(d_start_date)) {
      stop("Incorrect start_date date format")
    }
  }
  if (!is.null(end_date) & is.null(refine_date)) { 
    d_end_date <- try(as.Date(end_date, format="%Y-%m-%d"))
    ## Warn if d_end_date is not valid date class
    if ("try-error" %in% class(d_end_date) || is.na(d_end_date)) {
      stop("Incorrect end_date date format")
    }
  }
  ## Check follow_links
  if (!class(follow_links) %in% c("numeric", "integer"))
    stop("follow_links must be an integer or numeric")
  if (follow_links > 25) {
    warning("follow_links limited to 25 links")
    follow_links <- max_links;
  }
  ##
  ## Create ABS query string
  if (class(pattern) == "character") {
    ## 'Simple' query string 
    url <- paste0(abs_urls()$search_url,
                  "/s/search.html?",
                  "form=simple",
                  "&collection=abs-search",
                  sprintf("&query=%s", gsub("\\s+", "+", pattern)),
                  if(!is.null(refine_date)) {
                    sprintf("&%s", abs_search_date_filter(refine_date))
                  } else {
                    ""
                  });
  } else {
    ## 'Advanced' query string
    url <- paste0(abs_urls()$search_url,
                  "/s",
                  "/search.html?collection=abs-search",
                  "&from-advanced=true",
                  "&facetScope=",
                  "&form=simple",
                  "&profile=_default",
                  sprintf("&query=%s",        ## Pattern: any 
                          gsub("\\s+", "+", ifelse(is.null(pattern$any), "", pattern$any))), 
                  sprintf("&query_and=%s",    ## Pattern: all
                          gsub("\\s+", "+", ifelse(is.null(pattern$all), "", pattern$all))),      
                  sprintf("&query_phrase=%s", ## Pattern: phrase
                          gsub("\\s+", "+", ifelse(is.null(pattern$phrase), "", pattern$phrase))),
                  sprintf("&query_not=%s",    ## Pattern: not
                          gsub("\\s+", "+", ifelse(is.null(pattern$not), "", pattern$not))),       
                  sprintf("&meta_d1year=%s",  ## Start date: year
                          ifelse(is.null(start_date), "", format(d_start_date, "%Y"))), 
                  sprintf("&meta_d1month=%s", ## Start date: month
                          ifelse(is.null(start_date), "", format(d_start_date, "%b"))),
                  sprintf("&meta_d1day=%s",   ## Start date: day
                          ifelse(is.null(start_date), "", format(d_start_date, "%d"))),     
                  sprintf("&meta_d2year=%s",  ## End date: year
                          ifelse(is.null(end_date), "", format(d_end_date, "%Y"))),
                  sprintf("&meta_d2month=%s", ## End date: month
                          ifelse(is.null(end_date), "", format(d_end_date, "%b"))),
                  sprintf("&meta_d2day=%s",   ## End date: day
                          ifelse(is.null(end_date), "", format(d_end_date, "%d"))), 
                  if(!is.null(refine_date)) {
                    sprintf("&%s", abs_search_date_filter(refine_date))
                  } else {
                    ""
                  },
                  sprintf("&sort=%s", sort_by),
                  sprintf("&num_ranks=%s", n_results)
                  );
  }
  if (return_url) {
    return(url)
  } else {
    ## Check for HTTP errors
    raustats_check_url_available(url);
    ## Open html session
    suppressWarnings(s <- html_session(url));
    ## Return search results
    z <- abs_parse_search(s, resource=resource);
    N <- 1;
    ## 
    if (length(follow_links) > 1) {
      while(N < follow_links) {
        suppressWarnings(s <- html_session(abs_next_page(s)))
        w <- abs_parse_search(s, resource=resource)
        if (dim(w) > 0)
          z <- bind_rows(z, w);
        N <- N + 1;
      }
    }
    ## Return results
    return(z)
  }
}


## ---------------------------------- EOF ----------------------------------- ##

## -- ABS search query example statements --

## https://search.abs.gov.au/s/search.html?meta_d2month=&profile=_default&query=consumer+price+index&num_ranks=13&from-advanced=true&meta_d1year=2010&meta_d1day=1&collection=abs-search&meta_d2day=&sort=date&query_not=&query_and=&f.Date%7Cd=d%3D25Jan2021+%3A%3A+Today&form=simple&meta_d1month=Jul&query_phrase=&meta_d2year=

## https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=consumer+price+index&query_and=&query_phrase=&query_not=&meta_d1year=2010&meta_d1month=Jul&meta_d1day=1&meta_d2year=&meta_d2month=&meta_d2day=&sort=date&num_ranks=13
                   
## https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=consumer+price+index&query_and=&query_phrase=&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10
                   
## https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=consumer+price+index&query_and=&query_phrase=&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10
    
##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=gross+domestic+product&query_and=&query_phrase=&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=date&num_ranks=10

##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=gross+domestic+product&query_and=&query_phrase=&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=date&num_ranks=10
  
## https://search.abs.gov.au/s/search.html?collection=abs-search&form=simple&profile=_default&query=gross+domestic+product

##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=&query_and=gross+domestic+product&query_phrase=&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10

##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=&query_and=&query_phrase=gross+domestic+product&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10

##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=&query_and=&query_phrase=&query_not=gross+domestic+product&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10

##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=gross+domestic+product&query_and=&query_phrase=&query_not=&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10

##   https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=gross+domestic+product&query_and=&query_phrase=&query_not=&meta_d1year=2010&meta_d1month=Jun&meta_d1day=15&meta_d2year=&meta_d2month=&meta_d2day=&sort=&num_ranks=10

## https://search.abs.gov.au/s/search.html?collection=abs-search&from-advanced=true&facetScope=&form=simple&profile=_default&query=gross+domestic+product&query_and=&query_phrase=&query_not=expenditure&meta_d1year=&meta_d1month=&meta_d1day=&meta_d2year=&meta_d2month=&meta_d2day=&sort=date&num_ranks=10

## https://search.abs.gov.au/s/search.html?f.Date%7Cd=d%3E10Jan2021%3C26Jan2021+%3A%3A+Past+fortnight&form=simple&profile=_default&query=consumer+price+index&collection=abs-search

## https://search.abs.gov.au/s/search.html?f.Date%7Cd=d%3E17Jan2021%3C26Jan2021+%3A%3A+Past+week&form=simple&profile=_default&query=consumer+price+index&collection=abs-search
