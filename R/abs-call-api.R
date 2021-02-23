### ABS API functions

#' @name abs_api_urls
#' @title ABS URL addresses and paths used in ABS.Stat API calls
#' @description This function returns a list of URLs and data paths used to construction ABS.Stat
#'   API call. It is used in other functions in this package and need not be called directly.
#' @return a list with a base url and a url section for formatting the JSON API calls
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS.Stat API call functions
#' @keywords internal
abs_api_urls <- function()
  list(base_url = "http://stat.data.abs.gov.au",
       datastr_path = "restsdmx/sdmx.ashx/GetDataStructure",
       sdmx_json_path = "SDMX-JSON/data")


#' @name abs_api_call
#' @title Create ABS.Stat API URL call
#' @description The function created the ABS.Stat API call URL
#' @param path Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param args Named list of arguments to supply to call.
#' @return data frame in long format
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS.Stat API call functions
#' @keywords internal
abs_api_call <- function(path, args)
{
    if (missing(path))
      stop("path missing.")

    if (missing(args))
      stop("Argument path missing.")

    url <- file.path(abs_api_urls()$base_url, path, args)

    return(url);
}


#' @name abs_call_api
#' @title Submit API call to ABS.Stat
#' @description This function submits the specified API call to ABS.Stat
#' @importFrom xml2 read_xml
#' @importFrom httr http_error
#' @param url Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @return data frame in long format
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS.Stat API call functions
#' @keywords internal
abs_call_api <- function(url)
{
  if (http_error(url))
    stop(sprintf("HTTP error returned by url: %s", url))
  x <- read_xml(url)
  return(x);
}

## ----------------------------------- EOF ---------------------------------- ##

