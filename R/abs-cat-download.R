#' @name abs_cat_download
#' @title Function to download files from the ABS website and store locally
#' @description Downloads specified ABS catalogue data files from the ABS website, using a valid ABS
#'   data set URL.
#' @importFrom httr GET http_type http_error progress status_code write_disk
#' @param data_url Character vector specifying an ABS data URLs.
#' @param exdir Target directory for downloaded files (defaults to \code{tempdir()}). Directory is
#'   created if it doesn't exist.
#' @return Downloads data from the ABS website and returns a character vector listing the location
#'   where files are saved.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
abs_cat_download <- function(data_url, exdir=tempdir()) {
  if (!dir.exists(exdir)) dir.create(exdir);
  local_filenames <-
    sapply(data_url[!is.na(data_url)],
           function(url) {
             local_filename <- abs_local_filename(url);
             ## -- DEPRECATED --
             ## ## Check if any data_urls are not ABS data URLs
             ## if (!grepl("^https*:\\/\\/www\\.abs\\.gov\\.au\\/.+",
             ##            url, ignore.case=TRUE))	
             ##   stop(sprintf("Invalid ABS url: %s", url));
             ## -- END DEPRECATED --
             ##
             ## -- Download files --
             cat(sprintf("Downloading: %s", local_filename));
             ## Check for errors
             raustats_check_url_available(url)
             resp <- GET(url, write_disk(file.path(exdir, local_filename), overwrite=TRUE),
                         raustats_ua(), progress());
             ## -- DEPRECATED --
             ## Check content-type is compliant
             ## if (!http_type(resp) %in% abs_filetypes()) {
             ##   stop("ABS file request did not return Excel, Zip or PDF file", call. = FALSE)
             ## }
             ## -- END DEPRECATED --
             return(file.path(exdir, local_filename));
           })
  ## Return results
  return(local_filenames);
}
