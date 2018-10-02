rba_stats_url <- function()
  paste0(options()$raustats["rba_domain"],
         options()$raustats["rba_stats_path"]);

### Function: rba_table_cache
#' @name rba_table_cache
#' @title Return list of RBA tables
#' @description Function to return an updated list of data tables available from the RBA website.
#' @importFrom rvest html_session follow_link html_attr html_text html_nodes
#' @return data frame in long format
#' @export
#' @examples
#'   rba_tablecache <- rba_table_cache();
rba_table_cache <- function()
{
    ## DEBUG <- FALSE
    ## if (DEBUG) {
    ##     library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
    ## }
    ## Create ABS URL and open session 
    url <- file.path(rba_stats_url());
    s <- html_session(url);
    .paths <- html_nodes(s, "a");
    path_statistical_data <- unique(html_attr(.paths, "href")[grepl("^statistical tables$",
                                                                      html_text(.paths), ignore.case=TRUE)]);
    path_historical_data <- unique(html_attr(.paths, "href")[grepl("^historical data$",
                                                                   html_text(.paths), ignore.case=TRUE)]);
    path_discontinued_data <- unique(html_attr(.paths, "href")[grepl("^discontinued data$",
                                                                     html_text(.paths), ignore.case=TRUE)]);
    ## Get list of current data tables
    rs <- jump_to(s, path_statistical_data);
    .paths <- html_nodes(rs, "a");
    statistical_tables <- data.frame(table_type = "statistical tables",
                                     table = html_text(.paths[grepl("xls", .paths, ignore.case=TRUE)]),
                                     path = paste0(sub("/$", "", options()$raustats["rba_domain"]),
                                                   html_attr(.paths[grepl("xls", .paths, ignore.case=TRUE)],
                                                             "href")));
    ## Get list of historical data tables
    rs <- jump_to(s, path_historical_data);
    .paths <- html_nodes(rs, "a");
    historical_tables <- data.frame(table_type = "historical data",
                                    table = html_text(.paths[grepl("xls", .paths, ignore.case=TRUE)]),
                                    path = paste0(sub("/$", "", options()$raustats["rba_domain"]),
                                                  html_attr(.paths[grepl("xls", .paths, ignore.case=TRUE)],
                                                            "href")));
    ## Get list of discontinued data tables
    rs <- jump_to(s, path_discontinued_data);
    .paths <- html_nodes(rs, "a");
    discontinued_tables <- data.frame(table_type = "discontinued data",
                                      table = html_text(.paths[grepl("xls", .paths, ignore.case=TRUE)]),
                                      path = paste0(sub("/$", "", options()$raustats["rba_domain"]),
                                                    html_attr(.paths[grepl("xls", .paths, ignore.case=TRUE)],
                                                              "href")));
    z <- rbind(statistical_tables,
               historical_tables,
               discontinued_tables);
    z <- transform(z,
                   ## Regexp for en-dash: \u2013
                   ##   ''       em-dash: \u2014
                   table_name = sub("(.+)\\s(\\u2013|-)\\s(\\w\\d+(\\.\\d+)*)$", "\\1", table),
                   table_code = sub("(.+)\\s(\\u2013|-)\\s(\\w\\d+(\\.\\d+)*)$", "\\3", table));
    z <- z[,c("table_code", "table_name", "table_type", "path")];
    return(z);
}


### Function: rba_search
#' @name rba_search
#' @title Return list of data tables from RBA website
#' @description Function to return a list of all RBA data tables.
#' @export
#' @param pattern Character string or regular expression to be matched
#' @param fields Character vector of column names through which to search
#' @param update_cache Update table cache (\code{rba_tablecache}), FALSE by default.
#' @return data frame in long format
#' @export
#' @examples
#'   x <- rba_search(pattern = "A1", fields="table_code", update_cache=TRUE);
#'
rba_search <- function(pattern, fields="table_name", update_cache=FALSE)
{
    DEBUG <- FALSE
    if (DEBUG) {
        library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
        table_code = "A1";
    }
    if (missing(pattern))
        stop("No pattern supplied")

    if (update_cache) {
        rba_cache <- rba_table_cache();
    } else {
        rba_cache <- rba_tablecache;
    }
    z <- rba_cache[grepl(pattern, rba_cache[,fields]),] 
    return(z);
}


#' @name rba_data
#' @title Return data for a specified RBA time series
#' @description Function to get data from a specified RBA time series.
#' @importFrom utils download.file unzip
#' @param table_code Character vector specifying one or more ABS collections or catalogue numbers to download.
#' @param series_type RBA series type, one of either 'statistical tables', 'historical data' or 'discontinued data'.
#' @param update_cache Boolean argument expressing whether to retrieve an updated list of RBA tables or not (Default).
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'    x <- rba_data("A1");
#'    y <- rba_data("A1", update_cache=TRUE);
#'    
rba_data <- function(table_code, series_type="statistical tables", update_cache=FALSE)
{
    ## DEBUG <- FALSE
    ## if (DEBUG) {
    ##     library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
    ##     table_code = "A1";
    ##     series_type="statistical tables";
    ##     update_cache=FALSE;
    ## }
    ## Update RBA table list
    if (update_cache) {
        cat("Updating RBA table cache.\n");
        rba_table_list <- rba_table_cache();
    } else {
        rba_table_list <- raustats::rba_tablecache;
    }
    ## Select the relevant tables
    paths <- rba_table_list[grepl(series_type, rba_table_list$table_type, ignore.case=TRUE) &
                            grepl(paste(table_code, collapse="|"), rba_table_list$table_code,
                                  ignore.case=TRUE),];
    ## Download files
    local_files <- file.path(tempdir(), basename(as.character(paths$path)));
    cat("Downloading data\n");
    mapply(function(x, y) download.file(x, y, mode="wb"),
           as.character(paths$path), local_files);
    ## Read data
    data <- rba_read_tss(local_files);
    return(data);
}



### Function: rba_read_tss
#' @name rba_read_tss
#' @title Read RBA time series spreadsheet
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
#'   ABS.5206001 %>% as.data.frame %>% head;
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
  ## if (DEBUG) {
  ##     library(readxl)
  ##     file <- file.path("data-raw", "a01whist-summary.xls");
  ## }
  sheet_names <- excel_sheets(file);
  if (!all(c("notes", "data") %in% tolower(sheet_names)))
    stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
  ## Read metadata
  .meta <- read_excel(file, sheet=grep("data", excel_sheets(file), ignore.case=TRUE, value=TRUE),
                      col_names=FALSE, col_types="text");
  ## Return pre-header information from ABS files 
  header_row <- which(sapply(1:nrow(.meta),
                             function(i)
                                 grepl("series\\s*id", paste(.meta[i,], collapse=" "), ignore.case=TRUE)));
  metadata <- .meta[1:header_row,];
  metadata <- metadata[complete.cases(metadata),];            ## Drop NA rows
  metadata <- as.data.frame(t(metadata), stringsAsFactors=FALSE);
  rownames(metadata) <- seq_len(nrow(metadata));
  names(metadata) <- gsub("\\s","_",
                          gsub("\\.", "",
                               metadata[1,]));                ## Rename variables
  metadata <- metadata[-1,];
  metadata$Publication_date  <- excel2Date(as.integer(metadata$Publication_date));
  ## -- Extract table name --
  ## Note use of 'word' character    /here                /here for 13a, 6b, etc.
  regex_table_name <- "^(\\w+\\d+)\\s*Reserve Bank of Australia(\\s*-*\\s*)(.+)$";
  ## Return table name/number details
  tableno_name <- gsub("\\sNA", "", paste(.meta[1,], collapse=" "));
  table_code <- sub(regex_table_name, "\\1", tableno_name, ignore.case=TRUE);
  table_name <- sub(regex_table_name, "\\3", tableno_name, ignore.case=TRUE);
  ## Append to metadata table
  metadata <- transform(metadata,
                        Table_Code = table_code,
                        Table_Name = table_name);
  ## Read data tables
  data <- lapply(grep("data", sheet_names, ignore.case=TRUE, value=TRUE),
                 function(sheet_name) {
                       z <- read_excel(file, sheet=sheet_name);
                       ## Return pre-header information from ABS files 
                       header_row <- which(sapply(1:nrow(z),
                                                  function(i)
                                                      grepl("series\\s*id", paste(z[i,], collapse=" "), 
                                                            ignore.case=TRUE)));
                       names(z) <- gsub("\\s","_", gsub("\\.","", z[header_row,])); ## Rename variables
                       names(z) <- sub("Series_ID", "date",
                                       names(z), ignore.case=TRUE);           ## Rename Series_ID field
                       z <- z[-(1:header_row), !is.na(names(z))];             ## Drop empty columns
                       z <- gather(z, Series_ID, Value, -date, convert=TRUE); ## Transform to key:value pairs
                       z <- transform(z,
                                      date = excel2Date(as.integer(date)),
                                      Value = as.numeric(Value));
                       return(z);
                 });
  data <- do.call(rbind, data);
  data <- left_join(data, metadata, by="Series_ID");
  data <- data[complete.cases(data),];
  names(data) <- tolower(names(data));
  return(data);
}
