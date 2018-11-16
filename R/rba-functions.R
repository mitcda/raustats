rba_urls <- function()
{
  list(base_url = "http://www.rba.gov.au",
       stats_path = "statistics",
       tables_path = "tables");
}

### Function: rba_table_cache
#' @name rba_table_cache
#' @title Return list of RBA tables
#' @description Function to return an updated list of data tables available from the RBA website.
#' @importFrom rvest html_session jump_to html_attr html_text html_nodes
#' @return data frame in long format
#' @export
#' @examples
#'   rba_tablecache <- rba_table_cache();
rba_table_cache <- function()
{
  ## Create RBA URL and open session 
  url <- file.path(rba_urls()$base_url, rba_urls()$stats_path);
  s <- html_session(url);
  ## Get statistical data paths
  .paths <- html_nodes(s, "a");
  path_statistical_data <- unique(html_attr(.paths, "href")[grepl("^statistical tables$",
                                                                  html_text(.paths), ignore.case=TRUE)]);
  path_historical_data <- unique(html_attr(.paths, "href")[grepl("^historical data$",
                                                                 html_text(.paths), ignore.case=TRUE)]);
  path_discontinued_data <- unique(html_attr(.paths, "href")[grepl("^discontinued data$",
                                                                   html_text(.paths), ignore.case=TRUE)]);
  ##
  ## Get list of current data tables
  rs <- jump_to(s, path_statistical_data);
  .paths <- html_nodes(rs, "a");
  statistical_tables <- data.frame(table_type = "statistical table",
                                   table = html_text(.paths[grepl("xls(x*)", .paths, ignore.case=TRUE)]),
                                   url = paste0(sub("/$", "", rba_urls()$base_url),
                                                 html_attr(.paths[grepl("xls(x*)", .paths, ignore.case=TRUE)],
                                                           "href")));
  ## Include only Excel spreadsheet tables
  statistical_tables <- statistical_tables[grepl("\\.xls(x*)$", statistical_tables$url, ignore.case=TRUE),];
  ##
  ## Get list of historical data tables
  rs <- jump_to(s, path_historical_data);
  .paths <- html_nodes(rs, "a");
  historical_tables <- data.frame(table_type = "historical data",
                                  table = html_text(.paths[grepl("xls(x*)", .paths, ignore.case=TRUE)]),
                                  url = paste0(sub("/$", "", rba_urls()$base_url),
                                                html_attr(.paths[grepl("xls(x*)", .paths, ignore.case=TRUE)],
                                                          "href")));
  ## Exclude: i) Occasional Paper 10
  historical_tables <- historical_tables[!grepl("Occasional Paper.+10", historical_tables$table,
                                                ignore.case=TRUE),];
  ##  and    ii) Survey of consumers use of payments
  historical_tables <- historical_tables[!grepl("survey.+of.+consumers.+use", historical_tables$url,
                                                ignore.case=TRUE),];
  ##
  ## Get list of discontinued data tables
  rs <- jump_to(s, path_discontinued_data);
  .paths <- html_nodes(rs, "a");
  discontinued_tables <- data.frame(table_type = "discontinued data",
                                    table = html_text(.paths[grepl("xls(x*)", .paths, ignore.case=TRUE)]),
                                    url = paste0(sub("/$", "", rba_urls()$base_url),
                                                  html_attr(.paths[grepl("xls(x*)", .paths, ignore.case=TRUE)],
                                                            "href")));
  z <- rbind(statistical_tables,
             historical_tables,
             discontinued_tables);
  z <- transform(z,
                 table_name = sub("(.+)\\s(-|\u2013|\u2014)\\s(\\w\\d+(\\.\\d+)*)$", "\\1", table),
                 table_no = sub("(.+)\\s(-|\u2013|\u2014)\\s(\\w\\d+(\\.\\d+)*)$", "\\3", table));
  ## Replace en-dash/em-dash with hyphen (Regular expressions: en-dash - \u2013, and em-dash - \u2014
  z <- transform(z,
                 table_name = gsub("\\s+"," ", gsub("(\u2013|\u2014)","-", table_name)));
  ## Re-order columns and return
  z <- z[,c("table_no", "table_name", "table_type", "url")];
  return(z);
}


### Function: rba_search
#' @name rba_search
#' @title Return list of data tables from RBA website
#' @description Function to return a list of all RBA data tables.
#' @export
#' @param pattern Character string or regular expression to be matched
#' @param fields Character vector of column names through which to search. By default, the function
#'   searches 'table_no' and 'table_name'.
#' @param ignore.case Case senstive pattern match or not.
#' @param cache RBA table cache, returned by \code{rba_table_cache} function. If omitted,
#'   \code{rba_tablecache} is used.
#' @return data frame in long format
#' @export
#' @examples
#'   rba_datasets <- rba_search(pattern = "Liabilities and Assets");
#'
rba_search <- function(pattern, fields=c("table_no", "table_name"), ignore.case=TRUE, cache)
{
    if (missing(pattern))
        stop("No pattern supplied")
    if (missing(cache)) {
        rba_cache <- rba_tablecache;
    } else {
        rba_cache <- rba_table_cache();
    }
    if (any(!fields %in% names(rba_cache)))
      stop(sprintf("Field names: %s not in cache", fields[!fields %in% names(rba_cache)]))
    ## Return list of matching ABS.Stat datasets
    match_index <- sapply(fields,
                          function(field) grep(pattern, rba_cache[, field], ignore.case=ignore.case));
    match_index <- sort(unique(unlist(match_index)));
    z <- rba_cache[match_index,];
    return(z);
}


#' @name rba_stats
#' @title Return data for a specified RBA time series
#' @description Function to download and return specified RBA time series data.
#' @importFrom utils download.file unzip
#' @param table_no Character vector specifying one or more RBA table numbers to download.
## @param series_type RBA series type, one of either 'statistical tables', 'historical data' or
##   'discontinued data'.
#' @param pattern Character string or regular expression to be matched.
#' @param url Valid URL for RBA dataset (Excel format only).
#' @param cache RBA table cache, returned by \code{rba_table_cache} function. If omitted,
#'   \code{rba_tablecache} is used.
#' @param ... Other arguments to \code{\link{rba_search}}
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'    x <- rba_stats("A1");
#'    y <- rba_stats("A1", update_cache=TRUE);
#'    
rba_stats <- function(table_no, pattern, url, series_type="statistical tables", cache, ...)
{
  ## DEBUG <- FALSE
  ## if (DEBUG) {
  ##   table_no <- "A1"
  ##   pattern <- "Liabilities and Assets"
  ##   url <- rba_tablecache[1,"url"]
  ## }
  ## Deprecate: series_type
  if (missing(table_no) & missing(pattern) & missing(url))
    stop("One of either table_no, pattern or url must be specified.")
  if (!missing(table_no) & !missing(pattern))
    warning("Both table_no and pattern supplied, using table_no.")
  if (!missing(table_no) & !missing(url))
    warning("Both table_no and url supplied, using table_no.")
  if (!missing(pattern) & !missing(url))
    warning("Both pattern and url supplied, using pattern.")
  ## Update RBA table list
  if (missing(cache)) {
    rba_cache <- rba_table_cache();
  } else {
    rba_cache <- rba_tablecache;
  }

  ## TO DO: Add table_type attribute to vector 'urls'
  if (!missing(table_no)) {
    if (!table_no %in% rba_cache$table_no)
      stop("table_no not valid RBA table code")
    urls <- as.character(rba_cache$url[which(table_no == rba_cache$table_no)]);
  }

  if (!missing(pattern))
    urls <- as.character(rba_search(pattern, cache=rba_cache, ...)$url)
  
  if (!missing(url)) {
    if (any(!url %in% rba_cache$url))
      stop(sprintf("Following urls invalid: %s",
                   paste(rba_cache$url[!url %in% rba_cache$url], collapse="|")));
    urls <- url
  }
  
  ## Select the relevant tables
  ## paths <- rba_table_list[grepl(series_type, rba_table_list$table_type, ignore.case=TRUE) &
  ##                         grepl(paste(table_code, collapse="|"), rba_table_list$table_code,
  ##                               ignore.case=TRUE),];
  ## Download RBA statistical data ..
  z <- lapply(urls, rba_file_download);
  ## ## Download files
  ## local_files <- file.path(tempdir(), basename(as.character(rba_cach$url)));
  ## ## cat("Downloading data\n");
  ## mapply(function(x, y) download.file(x, y, mode="wb"),
  ##        as.character(urls), local_files);
  ## Read data
  data <- lapply(z, rba_read_tss);
  data <- do.call(rbind, data);
  rownames(data) <- seq_len(nrow(data));
  return(data);
}


#' @name rba_file_download
#' @title Function to download statistics files from the RBA website and store locally
#' @description This function downloads one or more RBA data files at the specified by URLs and
#'   saves a local copy.
#' @importFrom utils download.file unzip
#' @param url Character vector specifying one or more RBA data set URLs.
#' @param exdir Target directory for downloaded files (defaults to \code{tempdir()}). Directory is
#'   created if it doesn't exist.
#' @return Downloads data from the ABS website and returns a character vector listing the location
#'   where files are saved.
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
rba_file_download <- function(url, exdir=tempdir()) {
  ## DEBUG <- FALSE
  ## if (DEBUG) {
  ##   url <- "http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls"
  ##   exdir <- tempdir()
  ## }
  if(!dir.exists(exdir))
    dir.create(exdir)
  local_filename <- basename(as.character(url));
  ## local_filenames <- abs_local_filename(url);
  ## -- Download files --
  mapply(function(x, y) download.file(x, y, mode="wb"),
         url, file.path(exdir, local_filename));
  ## Return results
  return(file.path(exdir, local_filename));
}



### Function: rba_read_tss
#' @name rba_read_tss
#' @title Read RBA statistical time series spreadsheet
#' @description Functions to extract data from a specified RBA time series spreadsheet.
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @param files Names of one or more ABS data file
#' @return data frame in long format
#' @export
#' @examples
#'   file <- file.path("data-raw", "a01whist-summary.xls");
#'   rba_a1_summ <- rba_read_tss(file);
rba_read_tss <- function(files)
{
  x <- lapply(files,
              function(file)
                rba_read_tss_(file)
              )
  z <- do.call(rbind, x);
  return(z)
}


rba_read_tss_ <- function(file)
{
  DEBUG <- FALSE
  if (DEBUG) {
    library(readxl)
    file <- file.path(tempdir(), "a01whist-summary.xls");
  }
  ## Only process sheets not named: 'Data' or 'Series breaks'
  sheet_names <- excel_sheets(file)[grepl("data|series breaks", excel_sheets(file), ignore.case=TRUE)];
  ## TO DO
  ## 1. Require method to import historical and supplementary RBA data tables
  ## Check validity
  ## if (!all(c("notes", "data") %in% tolower(sheet_names)))
  ##   stop(sprintf("File: %s is not a valid RBA time series file.", basename(file)));
  data <- lapply(sheet_names,
                 function(sheet_name) {
                   ## Read metadata
                   .data <- read_excel(file, sheet=sheet_name, col_names=FALSE, col_types="text");
                   ## Return pre-header information from RBA files 
                   header_row <- which(sapply(1:nrow(.data),
                                              function(i)
                                                grepl("series\\s*id", paste(.data[i,], collapse=" "),
                                                      ignore.case=TRUE)));
                   ## -- Extract table name & number --
                   ## Note use of 'word' character    /here                /here for 13a, 6b, etc.
                   regex_table_name <- "^(\\w+\\d+(\\.\\d+)*)(.+)$";
                   table_no <- trimws(sub(regex_table_name, "\\1",
                                         paste(replace(.data[1,], is.na(.data[1,]), ""), collapse="")));
                   ## Return table name/number details
                   table_name <- trimws(sub(regex_table_name, "\\3",
                                            paste(replace(.data[1,], is.na(.data[1,]), ""), collapse="")));
                   ## Extract metadata
                   metadata <- .data[1:header_row,];
                   metadata <- metadata[complete.cases(metadata),];            ## Drop NA rows
                   metadata <- as.data.frame(t(metadata), stringsAsFactors=FALSE);
                   rownames(metadata) <- seq_len(nrow(metadata));
                   names(metadata) <- tolower(gsub("\\s","_",
                                                   gsub("\\.", "",
                                                        metadata[1,])));       ## Rename variables
                   metadata <- metadata[-1,];
                   metadata$publication_date <- excel2Date(as.integer(metadata$publication_date));
                   ## Append to metadata table
                   metadata <- transform(metadata,
                                         table_no = table_no,
                                         table_name = table_name);

                   ## UP TO HERE 
                   z <- .data[-(1:header_row),];
                   ## Rename variables, including renaming `Series ID`
                   names(z) <- sub("series.*id", "date", .data[header_row,], ignore.case=TRUE);
                   z <- gather(z, series_id, value, -date, convert=TRUE); ## Transform to key:value pairs
                   z <- transform(z,
                                  date = excel2Date(as.integer(date)),
                                  value = as.numeric(value));

                   data <- left_join(z, metadata, by="series_id");
                   data <- data[complete.cases(data),];
                   names(data) <- tolower(names(data));
                   return(data)
                 });
  ## ## Read metadata
  ## .meta <- read_excel(file,
  ##                     sheet=excel_sheets(file)[grepl("data|series breaks", excel_sheets(file), ignore.case=TRUE),
  ##                     col_names=FALSE, col_types="text");
  ## Return pre-header information from RBA files 
  ## header_row <- which(sapply(1:nrow(.meta),
  ##                            function(i)
  ##                                grepl("series\\s*id", paste(.meta[i,], collapse=" "), ignore.case=TRUE)));
  ## metadata <- .meta[1:header_row,];
  ## metadata <- metadata[complete.cases(metadata),];            ## Drop NA rows
  ## metadata <- as.data.frame(t(metadata), stringsAsFactors=FALSE);
  ## rownames(metadata) <- seq_len(nrow(metadata));
  ## names(metadata) <- tolower(gsub("\\s","_",
  ##                                 gsub("\\.", "",
  ##                                      metadata[1,])));       ## Rename variables
  ## metadata <- metadata[-1,];
  ## metadata$Publication_date  <- excel2Date(as.integer(metadata$Publication_date));
  ## ## -- Extract table name --
  ## ## Note use of 'word' character    /here                /here for 13a, 6b, etc.
  ## regex_table_name <- "^(\\w+\\d+)\\s*Reserve Bank of Australia(\\s*-*\\s*)(.+)$";
  ## ## Return table name/number details
  ## tableno_name <- gsub("\\sNA", "", paste(.meta[1,], collapse=" "));
  ## table_code <- sub(regex_table_name, "\\1", tableno_name, ignore.case=TRUE);
  ## table_name <- sub(regex_table_name, "\\3", tableno_name, ignore.case=TRUE);
  ## ## Append to metadata table
  ## metadata <- transform(metadata,
  ##                       Table_Code = table_code,
  ##                       Table_Name = table_name);
  ## ## Read data tables
  ## data <- lapply(grep("data", sheet_names, ignore.case=TRUE, value=TRUE),
  ##                function(sheet_name) {
  ##                      z <- read_excel(file, sheet=sheet_name);
  ##                      ## Return pre-header information from RBA files 
  ##                      header_row <- which(sapply(1:nrow(z),
  ##                                                 function(i)
  ##                                                     grepl("series\\s*id", paste(z[i,], collapse=" "), 
  ##                                                           ignore.case=TRUE)));
  ##                      names(z) <- tolower(gsub("\\s","_",
  ##                                               gsub("\\.","", z[header_row,]))); ## Rename variables
  ##                      names(z) <- sub("series_id", "date",
  ##                                      names(z), ignore.case=TRUE);           ## Rename Series_ID field
  ##                      z <- z[-(1:header_row), !is.na(names(z))];             ## Drop empty columns
  ##                      z <- gather(z, series_id, value, -date, convert=TRUE); ## Transform to key:value pairs
  ##                      z <- transform(z,
  ##                                     date = excel2Date(as.integer(date)),
  ##                                     value = as.numeric(value));
  ##                      return(z);
  ##                });
  data <- do.call(rbind, data);
  return(data);
}
