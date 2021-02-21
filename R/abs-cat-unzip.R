
#' @name abs_cat_unzip
#' @title Uncompress locally-stored ABS Catalogue data file archives
#' @description Function to uncompress locally-stored ABS Catalogue data file archives.
#' @importFrom utils unzip zip
#' @param files One or more local zip files.
#' @param exdir Target directory for extracted archive files. Directory is created if it doesn't
#'   exist. If missing, creates a new subdirectory in \code{tempdir()} using the respective zip
#'   files (specified in \code{files}.
#' @return Returns a character vector listing the names of all files extracted.
#' @family ABS helper functions
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
abs_cat_unzip <- function(files, exdir) {
  if (any(!file.exists(files)))
    stop(sprintf("Files %s do not exist",
                 paste(files[!file.exists(files)], collapse=", ")));
  if (missing(exdir))
    exdir <- tempdir();
  ## Only extract from zip files
  files <- files[grepl("\\.zip$", files, ignore.case=TRUE)];
  xl_files <- sapply(files,
                     function(x)
                       if (grepl("\\.zip$", x, ignore.case=TRUE)) {
                         ## If exdir NOT missing, then use it
                         if (exdir == tempdir()) {
                           exdir <- file.path(exdir, sub("\\.zip", "", basename(x)));
                         } else {
                           ## Else, use tempdir()
                           if (!dir.exists(exdir))
                             dir.create(exdir)
                         }
                         unzip(x, exdir=exdir);
                         file.path(exdir, unzip(x, list=TRUE)$Name);
                       } else {
                         x;
                       });
  return(xl_files);
}
