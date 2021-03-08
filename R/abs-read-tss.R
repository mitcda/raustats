### Function: abs_read_tss
#' @name abs_read_tss
#' @title Extract data from an ABS time series data file
#' @description This function extracts time series data from ABS data files.
#' @param files Names of one or more ABS data files
#' @param type
#'   `r lifecycle::badge("deprecated")`
#'   One of either 'tss' -- ABS Time Series Spreadsheet (the Default) or 'css' -- Data
#'   Cube.R
#' @param na.rm logical. If \code{TRUE} (default), remove observations containing missing values.
#' @return data frame in long format
#' @export
#' @family ABS catalogue functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## Read specified ABS Excel time series files
#'     tables <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
#'     downloaded_tables <- abs_cat_download(tables$path_zip, exdir=tempdir())
#'     extracted_files <- abs_cat_unzip(downloaded_tables)
#'     x <- abs_read_tss(extracted_files);
#'   }
abs_read_tss <- function(files, type="tss", na.rm=TRUE) {
  x <- lapply(files,
              function(file)
                abs_read_tss_(file, type=type, na.rm=na.rm));
  z <- do.call(rbind, x);
#  rownames(z) <- seq_len(nrow(z));
  return(z);
}


### Function: abs_read_tss_
#' @name abs_read_tss
#' @title Read ABS time series data file(s)
#' @description This is the internal function that extracts time series data from ABS data files.
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom stats complete.cases
#' @param files Names of one or more ABS data files
#' @param type DEPRECATED One of either 'tss' -- ABS Time Series Spreadsheet (the Default) or 'css' -- Data
#'   Cube.R
#' @param na.rm logical. If \code{TRUE} (default), remove observations containing missing values.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS catalogue helper functions
abs_read_tss_ <- function(file, type="tss", na.rm=na.rm) {
  ## if (FALSE) {
  ##   library(tidyr); library(magrittr)
  ##   tables <- abs_cat_tables("5206.0", include_urls=TRUE);
  ##   tables <- tables[grepl("^Table 1\\W", tables$item_name, ignore.case=TRUE),];
  ##   url <- tables$path_xls
  ##   files <- abs_cat_download(url)
  ##   file <- files[1]
  ## }
  ## Avoid 'No visible binding for global variables' note
  { series_start <- series_end <- no_obs <- collection_month <- series_id <- value <- NULL }
  ## Check if 'file' is a valid ABS time series file
  if (!is_abs_tss(file)) {
    ##  warning(sprintf("File: %s is not a valid ABS time series file.", basename(file)))
    ##  return(NULL)
    stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
  }
  ## sheet_names <- tolower(excel_sheets(file));
  ## ## Check if 'file' is a valid ABS time series file
  ## if (!all(c("index", "data1")  %in% sheet_names)) {
  ##   stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
  ## POSSIBLE ALTERNATIVE WAY OF HANDLING NON TIME SERIES FILES:
  ##  warning(sprintf("File: %s is not a valid ABS time series file.", basename(file)))
  ##  return(NULL)
  ## } else {
  ## -- Read metadata --
  .meta <- read_excel(file,
                      sheet = grep("index", excel_sheets(file), ignore.case=TRUE, value=TRUE),
                        .name_repair = "minimal");
    ## Return pre-header information from ABS files 
  header_row <- which(sapply(1:nrow(.meta),
                               function(i)
                                 grepl("series\\s*id", paste(.meta[i,], collapse=" "),
                                       ignore.case=TRUE)));
  metadata <- .meta;
  names(metadata) <- make.names(tolower(gsub("\\s","_",                 ## Rename columns
                                             gsub("\\.", "",
                                                  .meta[header_row,]))),
                                unique=TRUE);
  metadata <- metadata[-(1:header_row),                               ## Drop header rows & unnamed columns
                       !(is.na(names(metadata)) | grepl("^X.*", names(metadata)))];     
  metadata <- Filter(function(x) !all(is.na(x)), metadata);           ## Drop all-NA columns
  metadata <- metadata[complete.cases(metadata),];                    ## Drop NA rows
    metadata <- metadata[grepl("\\w\\d{4,7}\\w", metadata$series_id),]; ## Drop if Series ID invalid 
    metadata <- transform(metadata,
                          series_start     = excel2Date(as.integer(series_start)),
                          series_end       = excel2Date(as.integer(series_end)),
                          no_obs           = as.integer(no_obs),
                          collection_month = as.integer(collection_month),
                          stringsAsFactors=FALSE);
    ##
    ## Get publication details
    ## -- Catalogue number & name --
    regex_catno_name <- "^.*(\\d{4}\\.\\d+(\\.\\d+)*)\\s+(.+)$";
    catno_name <- sapply(1:header_row,
                         function(i)
                           grep(regex_catno_name, paste(.meta[i,], collapse=" "),
                                ignore.case=TRUE, value=TRUE));
    catno_name <- gsub("(\\s*NA)+", "",
                       sub(regex_catno_name, "\\1|\\3", unlist(catno_name), ignore.case=TRUE));
    catno_name <- trimws(unlist(strsplit(catno_name, split="\\|")));
    ##
    ## -- Table number & name --
    ## Note use of 'word' character    \/here               \/here for 13a, 6b, etc.
    regex_table_name <- "^.*Tables*\\s+(\\w+(\\s+\\w+\\s+\\w+)*)(\\.|:)*\\s+(.+)$";
    ## Note use of alternative separators: .|:                      ^here
    tableno_name <- sapply(1:header_row,
                           function(i)
                             grep(regex_table_name,
                                  paste(.meta[i,], collapse=" "),
                                  ignore.case=TRUE, value=TRUE));
    tableno_name <- gsub("(\\s*NA)+", "",
                         sub(regex_table_name, "\\1|\\4", unlist(tableno_name), ignore.case=TRUE));
    tableno_name <- trimws(unlist(strsplit(tableno_name, split="\\|")));
    ##
    ## Add publication details to metadata table
    metadata  <- transform(metadata,
                           catalogue_no      = catno_name[1],
                           publication_title = catno_name[2],
                           table_no          = tableno_name[1],
                           table_title       = tableno_name[2],
                           stringsAsFactors=FALSE);
    ## Extract data
    data <- lapply(grep("data", excel_sheets(file), ignore.case=TRUE, value=TRUE),
                   function(sheet_name) {
                     z <- read_excel(file, sheet=sheet_name, .name_repair = "minimal");
                     ## Return pre-header information from ABS files 
                     header_row <- which(sapply(1:nrow(z),
                                                function(i)
                                                  grepl("series\\s*id", paste(z[i,], collapse=" "), 
                                                        ignore.case=TRUE)));
                     names(z) <- gsub("\\s","_",
                                      gsub("\\.","", z[header_row,]));      ## Rename variables
                     names(z) <- sub("series_id", "date", names(z),         ## Rename Series_ID field
                                     ignore.case=TRUE); 
                     z <- z[-(1:header_row), !is.na(names(z))];             ## Drop header rows & unnamed columns
                     z <- Filter(function(x) !all(is.na(x)), z);            ## Drop all-NA columns
                     z <- gather(z, series_id, value, -date, convert=TRUE); ## Transform data to key:value pairs
                     z <- transform(z,
                                    date = excel2Date(as.integer(date)),
                                    value = as.numeric(value));
                     names(z) <- tolower(names(z));
                     return(z);
                   });
    data <- do.call(rbind, data);
    data <- left_join(data, metadata, by="series_id");
    if (na.rm)
      data <- data[complete.cases(data),]
    names(data) <- tolower(names(data));
    return(data);
}

## ----------------------------------- EOF ---------------------------------- ##
