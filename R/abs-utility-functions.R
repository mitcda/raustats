### ABS catalogue utility functions

#' @name is_abs_tss
#' @title Check if file is a valid ABS time series spreadsheet
#' @description This function checks if the specified returns a list of URLs and data paths used to
#'   construct ABS Catalogue data access calls. It is used in other functions in this package and
#'   need not be called directly.
#' @importFrom readxl read_excel excel_sheets
#' @param file An ABS data filename
#' @return logical
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS catalogue helper functions
is_abs_tss <- function(file)
{
  if (!grepl("\\.xlsx*", file, ignore.case=TRUE))
    stop(sprintf("File: %s is not an Excel spreadsheet.", basename(file)));
  ## Get sheet names
  sheet_names <- tolower(excel_sheets(file));
  ## Check if 'file' contains valid ABS time series spreadsheet sheet names
  if (!all(c("index", "data1")  %in% sheet_names)) {
    ## stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' @name split_absstat_query
#' @title Split long ABS Stat query into several smaller chunks
#' @description
#'   `r lifecycle::badge("experimental")`
#'   This function checks if a draft ABS.Stat query call exceeds the string length limit
#'   or would return more than one million cells and if so, splits the query into smaller chunks
#'   for calling the ABS.Stat site. This function is called by the \code{\link{abs_stats}} function,
#'   it need not be called directly.
#' @importFrom purrr cross
#' @param url An ABS Stat JSON SDMX query string
#' @return string or list
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS.Stat helper functions
split_absstat_query <- function(url)
{
  if (nchar(url) > 1000) {
    ## Extract data filter terms
    filter_text <- sub(sprintf("^(%s/%s/.+/)(.+)(/.+)$",
                               abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
                       "\\2", url);
    url_head <- sub(sprintf("^(%s/%s/.+/)(.+)(/.+)$",
                            abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
                    "\\1", url);
    url_tail <- sub(sprintf("^(%s/%s/.+/)(.+)(/.+)$",
                            abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
                    "\\3", url);
    ## Total filter string length
    len_filter_text <- nchar(filter_text);
    avail_filter_text <- 1000 - round(nchar(url) - len_filter_text, -1);
    ## Split filter into different components
    filter_list <- lapply(unlist(strsplit(filter_text, "\\.")),
                          function(x) {
                            z <- unlist(strsplit(x, "\\+"))
                          });
    ## Derive component string lengths
    len_filter_list <- lapply(filter_list, function(x) sum(nchar(x)+1))
    maxlen_filter_list <- lapply(filter_list, function(x) max(nchar(x)+1))
    n_filter_list <- lapply(filter_list, length)
    ## Find longest 
    i <- which.max(len_filter_list)
    n_groups <- floor((avail_filter_text - sum(unlist(len_filter_list[-i]))) /
                      maxlen_filter_list[[i]]);
    ## Split filter Shrink the number of 
    split_filter <- lapply(filter_list,
                           function(x) split(x, ceiling(seq_along(x) / n_groups)));
    split_filter <- cross(split_filter);
    ## Re-create URL filter strings
    ## split_url <- lapply(split_filter,
    ##                     function(x)
    ##                       paste(lapply(x,
    ##                                    function(y) paste(y, collapse="+")),
    ##                             collapse="."));
    split_url <- paste0(url_head,
                            lapply(split_filter,
                                   function(x)
                                     paste(lapply(x,
                                                  function(y) paste(y, collapse="+")),
                                           collapse=".")),
                            url_tail);
    return(split_url);
  }
}

## ----------------------------------- EOF ---------------------------------- ##
