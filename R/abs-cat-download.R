#' @name abs_cat_download
#' @title Function to download files from the ABS website and store locally
#' @description Downloads specified ABS catalogue data files from the ABS website, using a valid ABS
#'   data set URL.
#' @importFrom httr GET http_type http_error progress status_code write_disk
#' @param x Either a character vector specifying one or more a valid ABS data URLs or a data frame
#'   returned by \link{\code{abs_cat_tables}} function.
#' @param exdir Target directory for downloaded files (defaults to \code{tempdir()}). Directory is
#'   created if it doesn't exist.
#' @return Downloads data from the ABS website and returns a character vector listing the location
#'   of locally saved files.
#' @family ABS catalogue functions
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
abs_cat_download <- function(x, exdir=tempdir()) {
  UseMethod("abs_cat_download", x)
}


#' @rdname abs_cat_download
#' @export
abs_cat_download.cat_table <- function(x, exdir=tempdir()) { 
  if (!dir.exists(exdir)) dir.create(exdir);
  local_filenames <-
    sapply(1:nrow(x),
           function(i) {
             ## Check for errors
             raustats_check_url_available(x$file_url[i])
             cat(sprintf("Downloading file: %s", x$file_name[i]));
             resp <- GET(x$file_url[i],
                         write_disk(file.path(exdir, x$file_name[i]), overwrite=TRUE),
                         raustats_ua(), progress());
             return(file.path(exdir, x$file_name[i]));
           })
  return(local_filenames);
}


#' @rdname abs_cat_download
#' @export
abs_cat_download.default <- function(x, exdir=tempdir()) {
  if (!dir.exists(exdir)) dir.create(exdir);
  local_filenames <-
    sapply(x[!is.na(x)],
           function(url) {
             local_filename <- abs_local_filename(url);
             ## -- Download files --
             cat(sprintf("Downloading: %s", local_filename));
             ## Check for errors
             raustats_check_url_available(url)
             resp <- GET(url, write_disk(file.path(exdir, local_filename), overwrite=TRUE),
                         raustats_ua(), progress());
             return(file.path(exdir, local_filename));
           })
  ## Return results
  return(local_filenames);
}

## ----------------------------------- EOF ---------------------------------- ##
