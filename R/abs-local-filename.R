
#' @name abs_local_filename
#' @title Create local file names for storing downloaded ABS data files
#' @description Function to create local filename from web-based file name.
#' @param url Character vector specifying one or more ABS data URLs.
#' @return Returns a local file names (character vector) in which downloaded files will be saved.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS catalogue helper functions
abs_local_filename <- function(url)
{
   ## Regular expression notes
   ## 1. Leading and trailing '&' (ampersand) demarcates filename from 'archive' ABS URLs
   ## 2. Leading (escaped) slash '\\/' and zero or more trailing characters demarcates filename in 'current' ABS URLs
   sub("^.+(&|\\/)(.+\\.(zip|xlsx*|pdf))&*.*$", "\\2", url)
}
