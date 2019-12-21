## httr settings
#' @name raustats_ua
#' @title raustats package user agent
#' @description This function specifies the package user agent, and is used inside
#'   GET/POST function calls
#' @importFrom httr user_agent
#' @return a list with a base url and a url section for formatting the JSON API calls
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
raustats_ua <- function()
  user_agent("http://github.com/mitcda/raustats")


## Check if the specified ABS/RBA URL is available
#' @name raustats_check_url_available
#' @title Check specified ABS/RBA URL available
#' @description Function to ensure URL calls fail gracefully with an informative message if the
#'   resource is not available (and not give a check warning nor error).
#' @importFrom httr GET status_code
#' @param url The base URL to check.
#' @return \code{TRUE} if the API is available, otherwise \code{stop()} is called.
#' @note Based on code in \code{opensensmapR} (\url{https://github.com/sensebox/opensensmapR/blob/f69cf62b2771d5b6ed605c04b7ddd618f5a272c2/R/api.R}{\code{api.R}}).
#' @keywords internal
raustats_check_url_available <- function(url) {
  code = FALSE
  try({ code = status_code(GET(url, raustats_ua())) }, silent = TRUE)
  
  if (code == 200)
    return(NULL)
  
  if (code != FALSE) {
    errtext = sprintf("The API at %s is currently not available. (HTTP code %s)", url, code)
    stop(paste(errtext, collapse='\n'), call. = FALSE)
  }
}
