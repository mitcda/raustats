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

