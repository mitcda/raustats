#' @name abs_search
#' @title Search dataset information from the ABS.Stat API
#' @description This function finds datasets or dimensions within a specific that match a specified
#'   regular expresion and returns matching results.
#' @param pattern Character string or regular expression to be matched.
#' @param dataset Character vector of ABS.Stat dataset codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}. If NULL (default), then function
#'   undertakes a dataset mode search. If not NULL, function searches all dimensions of specified
#'   dataset.
#' @param ignore.case Case senstive pattern match or not.
#' @param code_only If FALSE (default), all column/fields are returned. If TRUE, only the dataset
#'   identifier or indicator code are returned.
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available
#'   ABS.Stat datasets, if TRUE, update the list of available datasets.
#' @return A data frame with datasets and data items that match the search pattern.
#' @export
#' @family ABS.Stat API search functions
#' @note With acknowledgements to \code{wb_search} function.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'  ## ABS dataset search
#'  x <- abs_search(pattern = "consumer price index")
#'  x <- abs_search(pattern = "census")
#'  x <- abs_search(pattern = "labour force")
#'
#'  ## ABS indicator search
#'  x <- abs_search(pattern = "all groups", dataset="CPI")
#'  x <- abs_search(pattern = c("all groups", "capital cities"), dataset="CPI")
#' 
abs_search <- function(pattern, dataset=NULL, ignore.case=TRUE, code_only=FALSE, update_cache=FALSE)
{
  if (missing(pattern))
    stop("No regular expression provided.")
  if (!exists("abs_stat_list", envir=.raustats_cache, inherits=FALSE)) {
    message("Scanning ABS.Stat catalogue (called on first use each session).")
    assign("abs_stat_list", abs_datasets(), envir=.raustats_cache);
  }
  cache <- get("abs_stat_list", envir=.raustats_cache);
  ## 
  if (is.null(dataset)) {
    ## 1. If dataset not specified, search through list of datasets
    ## Return list of matching ABS.Stat datasets
    match_index <- sapply(names(cache),
                          function(i) grep(pattern, cache[, i], ignore.case=ignore.case),
                          USE.NAMES = FALSE);
    match_index <- sort(unique(unlist(match_index)));
    if (length(match_index) == 0)
      warning(sprintf("No matches were found for the search term %s. Returning an empty data frame.", 
                      pattern));
    match_df <- unique(cache[match_index, ])  ## unique(cache_table[match_index, ])
    rownames(match_df) <- seq_len(nrow(match_df));
    if (code_only)
      match_df <- as.character(match_df[,"id"]);
    return(match_df);
  } else {
    ## 2. If dataset specified, search through list of datasets
    if (!dataset %in% cache$id)
      stop(sprintf("Dataset: %s not available on ABS.Stat", dataset))
    .cachelist <- abs_metadata(dataset);
    names(.cachelist) <- attr(.cachelist, "concept");
    ## Return list of all dataset dimensions with matching elements
    filter_index <- lapply(.cachelist,
                           function(x) {
                             i <- grep(sprintf("(%s)", paste(pattern, collapse="|")),
                                       x$Description, ignore.case=ignore.case);
                             z <- x[i,];
                             return(z);
                           });
    filter <- filter_index[sapply(filter_index, nrow) > 0]
    if (code_only)
      filter <- lapply(filter, function(x) as.character(x$Code));
    return(filter)
  }
}

## ----------------------------------- EOF ---------------------------------- ##
