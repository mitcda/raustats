#' @name abs_dimensions
#' @title Return available dimensions of ABS series
#' @description This function returns the available dimeninsions for a specified ABS API dataset.
#' @param dataset Character vector of dataset codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}.
#' @return a data frame with available dataset dimensions.
#' @family ABS.Stat functions
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## CPI - Consumer Price Index
#'     x <- abs_dimensions("CPI");
#'     str(x)
#'     ## LF - Labour Force
#'     x <- abs_dimensions("LF");
#'     str(x)
#'   }
abs_dimensions <- function(dataset)
{
  ## Check dataset present and valid 
  if (missing(dataset))
    stop("No dataset name supplied.");
  if (!exists("abs_stat_list", envir=.raustats_cache, inherits=FALSE)) {
    message("Scanning ABS.Stat catalogue (called on first use each session).")
    assign("abs_stat_list", abs_datasets(), envir=.raustats_cache);
  }
  ## Get latest ABS statistics list
  cache <- get("abs_stat_list", envir=.raustats_cache);
  ##   if (update_cache) {
  ##   cache <- abs_datasets();
  ## } else {
  ##   cache <- raustats::abs_cachelist;
  ## }
  ## Stop if invalid ABS dataset identifier
  if (!dataset %in% cache$id)
    stop(sprintf("%s not valid dataset name.", dataset));
  metadata <- abs_metadata(dataset)
  ## Return data frame of dataset dimensions:
  z <- data.frame(name = attr(metadata, "concept"),
                  type = attr(metadata, "type"),
                  stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
  return(z)
}

## ----------------------------------- EOF ---------------------------------- ##
