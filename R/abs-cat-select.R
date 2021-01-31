#' @name abs_cat_select
#' @title Return list of ABS statistics series
#' @description Return list of ABS statistical series matching selection condition
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom dplyr bind_cols
#' @importFrom magrittr set_names
#' @param pattern character string containing a regular expression to be matched in the given
#'   character vector. See \code{\link[base]{grep}} for further details.
#' @param level one or more of 'group', 'view' and/or 'topic'.
#' @param ... other arguments to \code{grep}.
#' @return Returns a data frame listing available ABS catalogue releases.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## List all available quarterly National Accounts tables
#'     ana_releases <- abs_cat_releases("5206.0");
#'     ana_release_urls <- abs_cat_releases("5206.0", include_urls=TRUE);
#'   
#'     ## List latest available CPI Time Series Spreadsheet tables only
#'     cpi_releases <- abs_cat_releases("6401.0");
#'     cpi_release_urls <- abs_cat_releases("6401.0", include_urls=TRUE);
#'   }
abs_cat_select <- function(pattern, level = c('group', 'view', 'topic'),
                           ignore.case = TRUE, include_urls = FALSE, ...) {
  if (FALSE) {
    ## -- DEBUGGING CODE --
    pattern <- "wage price index"
    level <- 'topic'
    level <- c('group', 'view', 'topic')
  }
  if (missing(pattern))
    stop("No pattern supplied.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  level <- match.arg(level);
  ## Get list of all ABS statistics
  if (!exists("abs_stat_list", envir=raustats_env))
    abs_cat_list();
  z <- get("abs_stat_list", envir=raustats_env);
  ## Return list ABS publications
  y <- lapply(level,
              function(x)
                w <- grepl(pattern, z[,sprintf("stat_%s", x)], ignore.case=ignore.case)) %>% ## , ...
    bind_cols(.name_repair = ~ vctrs::vec_as_names(..., repair="unique", quiet=TRUE));
  i <- sapply(1:nrow(y), function(i) any(unlist(y[i,])));

  if (include_urls) {
    x <- z[i,]
  } else {
    x <- z[i,c("stat_group", "stat_view", "stat_topic")] %>%
      set_names(sub("stat_", "", names(.)))
  }

  return(x)
}
