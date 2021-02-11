
#' @name abs_local_filename
#' @title Create local file names for storing downloaded ABS data files
#' @description Function to create local filename from web-based file name.
#' @param url Character vector specifying one or more ABS data URLs.
#' @return Returns a local file names (character vector) in which downloaded files will be saved.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_local_filename <- function(url)
{
  gsub("\\s+", "_", basename(url));
  ## sprintf("%s_%s.%s",
  ##            sub("^.+&(.+)\\.(zip|xlsx*|pdf)&.+$", "\\1", z),
  ##            sub("^.+(\\d{2}).(\\d{2}).(\\d{4}).+$", "\\3\\2\\1", z),
  ##            sub("^.+&(.+)\\.(zip|xlsx*|pdf)&.+$", "\\2", z));
}
