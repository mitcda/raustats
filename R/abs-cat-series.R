#' @name abs_cat_series
#' @title Return list of ABS statistics' series titles
#' @description `r lifecycle::badge("experimental")` Return list of ABS statistical series matching
#'   selection condition
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom dplyr bind_cols
#' @importFrom magrittr set_names
#' @importFrom vctrs vec_as_names
#' @param pattern character string containing a regular expression to be matched in the given
#'   character vector. See \code{\link[base]{grep}} for further details.
#' @param level one or more of 'group', 'view' and/or 'title'.
#' @param ignore.case if \code{TRUE} (default) pattern matching is not case sensitive and if
#'   \code{FALSE}, matching is case sensitive.
#' @param include_urls if \code{TRUE}, the function includes the ABS website URL for the returned
#'   series in the results table, but not if \code{FALSE} (default)..
#' @param ... other arguments to \code{\link[base]{grep}}.
#' @return Returns a data frame listing available ABS statistical titles.
#' @family ABS catalogue functions
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## List ABS series matching regular expression: "national.*income.*expenditure.*product"
#'     abs_series <- abs_cat_series(pattern="national.*income.*expenditure.*product",
#'                                  level="title");
#' 
#'     ## List ABS series matching regular expression: "consumer.*price.*index" and include URLs
#'     abs_series <- abs_cat_series(pattern="consumer.*price.*index.*product",
#'                                  include_urls=TRUE);
#'   }
abs_cat_series <- function(pattern, level = c('group', 'view', 'title'),
                           ignore.case = TRUE, include_urls = FALSE, ...) {
  if (missing(pattern))
    stop("No pattern supplied.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  level <- match.arg(level, several.ok=TRUE);
  ## Get list of all ABS statistics
  if (!exists("abs_cat_list", envir=.raustats_cache, inherits=FALSE))
    abs_cat_list();
  z <- get("abs_cat_list", envir=.raustats_cache, inherits=FALSE);
  ## Return list ABS publications
  y <- lapply(level,
              function(x)
                w <- grepl(pattern, z[,sprintf("stat_%s", x)], ignore.case=ignore.case));
  y <- bind_cols(y, .name_repair = ~ vec_as_names(..., repair="unique", quiet=TRUE));
  i <- sapply(1:nrow(y), function(i) any(unlist(y[i,])));

  if (include_urls) {
    x <- z[i,]
  } else {
    x <- z[i,c("stat_group", "stat_view", "stat_title")];
  }
  names(x) <- sub("stat_", "", names(x));
  return(x)
}

## ----------------------------------- EOF ---------------------------------- ##
