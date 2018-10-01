### Data sets

#' @name rba_tablecache
#' @title Cached list of statistical tables provided by the RBA
#' @description 
#'
#' @format A data frame containing three columns:
#' \itemize{
#' \item \code{table_code} RBA table code.
#' \item \code{table_name} RBA table name.
#' \item \code{table_type} One of either current statistical tables, historical data or discontinued data
#' \item \code{path} RBA URL 
#' }

"rba_tablecache"


#' @name abs_tablecache
#' @title Cached list of ABS catalogue tables
#' @description 
#'
#' @format A data frame containing five columns:
#' \itemize{
#' \item \code{publication_title} ABS publication title.
#' \item \code{catalogue_no} ABS catalogue number.
#' \item \code{abs_url} ABS URL.
#' \item \code{last_updated} Publication last updated.
#' \item \code{type} Publication type -- one of either 'time series', 'panel' or 'summary'.RBA URL 
#' }

"abs_tablecache"


#' @name aus_state_codes
#' @title Table of Australian state and territory codes
#' @description 
#'
#' @format A data frame containing three columns:
#' \itemize{
#' \item \code{state_code} One-digit state code.
#' \item \code{state_abb} State/territory abbreviation.
#' \item \code{state_name} State/territory name.
#' }

"aus_state_codes"
