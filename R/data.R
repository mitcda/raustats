### Data sets

#' @name rba_cachelist
#' @title Cached list of statistical tables provided by the RBA
#' @description This data is a cached result of the \code{\link{rba_table_cache}} function. By
#'   default functions \code{\link{rba_search}} and \code{\link{rba_stats}} use this data if the
#'   \code{update_cache} parameter is \code{TRUE}.
#' 
#' @format A data frame containing three columns:
#' \itemize{
#'   \item \code{table_code} RBA table code.
#'   \item \code{table_name} RBA table name.
#'   \item \code{table_type} One of either current statistical tables, historical data or discontinued data
#'   \item \code{url} RBA URL 
#' }
"rba_cachelist"


#' @name abs_cat_cachelist
#' @title List of ABS catalogue tables
#' @description This data set provides a list of the most common ABS catalogue tables.
#'
#' @format A data frame containing five columns:
#' \itemize{
#'   \item \code{publication_title} ABS publication title.
#'   \item \code{catalogue_no} ABS catalogue number.
#'   \item \code{abs_url} ABS URL.
#'   \item \code{last_updated} Publication last updated.
#'   \item \code{type} Publication type -- one of either 'time series', 'panel' or 'summary'.RBA URL 
#' }
"abs_cat_cachelist"


#' @name abs_cachelist
#' @title Datasets available through the ABS API
#' @description This data set provides a list of all datasets, and the associated metadata,
#'   available through the ABS API.
#' @format A data frame containing three columns:
#' \itemize{
#' \item \code{id} ABS dataset identifier.
#' \item \code{agencyID} Source agency identifier (ABS).
#' \item \code{name} ABS dataset name.
#' }
"abs_cachelist"


#' @name aus_state_codes
#' @title Table of Australian state and territory codes
#' @description A list of Australian state and territory codes.
#'
#' @format A data frame containing three columns:
#' \itemize{
#'   \item \code{state_code} One-digit state code.
#'   \item \code{state_abb} State/territory abbreviation.
#'   \item \code{state_name} State/territory name.
#' }
"aus_state_codes"

