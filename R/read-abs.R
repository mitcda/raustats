abs_ausstats_url <- function()
  paste0(options()$raustats["abs_domain"],
         options()$raustats["abs_ausstats_path"]);

#' @name get_abs_data
#' @title Return data files from a specified url
#' @description TBC
#' @importFrom magrittr %>% inset
#' @importFrom rvest html_session follow_link html_attr
#' @importFrom xml2 read_xml read_html
#' @importFrom urltools url_parse url_compose
#' @importFrom utils download.file unzip
#' @param series Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param tables A character vector of regular expressions denoting tables to download. The default
#'   ('All') downloads all time series spreadsheet tables for each specified catalogue. Use a list
#'   to specify different table sets for each specified ABS catalogue number.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param type One of either 'tss' - time series spreadsheet (DEFALT or 'dem' - demographic data
#' @param ... other arguments to
#' @return data frame in long format
#' @export
#' @examples
#'    x <- get_abs_data("3101.0");
#'    y <- get_abs_data("5206.0", tables=c("Table 1", "Table 2"));
#' 
#'    x <- get_abs_data("5206.0", tables="Table 1", release="Dec 2017");
get_abs_data <- function(series, tables="All", releases="Latest", type="tss")
{
  DEBUG <- FALSE
  if (DEBUG) {
    library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
    series <- "5206.0";
    ## -- Previous tables --
    tables <- c("Table 1", "Table 2");
    releases <- c("Mar 2017", "Dec 2016", "Sep 2016", "Jun 2016", "Mar 2016");
    ## -- Current tables --
    ## tables <- "All";
    ## releases <- "Latest";
  }
  ## Create ABS URL and open session 
  url <- file.path(abs_ausstats_url(), series);
  s <- html_session(url);
  
  releases <- unique(tolower(releases));
  if (length(releases) == 1 && releases == "latest") {
      .paths <- "";
  } else {
      ## Get path to 'Past & Future Releases' page
      .paths <- html_nodes(s, "a");
      .paths <- .paths[grepl(options()$raustats["abs_releases_regex"], .paths)];
      .paths <- html_attr(.paths, "href");
      s <- jump_to(s, .paths)
      .paths <- html_nodes(s, "a");
      .paths <- .paths[grepl(paste(releases, collapse="|"), .paths, ignore.case=TRUE)];
      .paths <- html_attr(.paths, "href");
  }

  ## Return list of all downloadable files, for specified catalogue tables ('cat_tables')
  cat_tables <- sapply(.paths,
                       function(x) {
                           .url <- jump_to(s, x)
                           z <- get_abs_cat_tables(.url$url)
                       },
                       simplify=FALSE);
  
  ## Select only the user specified tables ('sel_tables')
  if (length(tables) == 1 && tolower(tables) == "all") {
      ## If 'all' tables, download all
      sel_tables <- sapply(cat_tables,
                           function(x)
                               if (any(grepl("^all time series.*", x$table_name, ignore.case=TRUE))) {
                                   ## Check whether all tables provided as single compressed archive and use
                                   x[grepl("^all time series.*", x$table_name, ignore.case=TRUE),]
                               } else {
                                   ## Else load all tables
                                   x
                               },
                           simplify=FALSE)
  } else {
      ## Else, return only selected tables
      sel_tables <- sapply(cat_tables,
                           function(x) {
                               x[grepl(paste0("^(",
                                              paste(paste0(tables, "\\W.+" ), collapse="|"),
                                              ")"),
                                       x$table_name, ignore.case=TRUE),]
                           },
                           simplify=FALSE);
  }

  ## Select only the user specified tables ('sel_tables')
  sel_paths <- sapply(sel_tables,
                      function(x)
                          apply(x, 1, 
                                function(y) {
                                 ## PSEUDO CODE
                                    ## if zip in path1/path2, select zip file, else select xls(x) file
                                    if (any(grepl("\\.zip", y, ignore.case=TRUE))) {
                                        grep("\\.zip", unlist(y), ignore.case=TRUE, value=TRUE)
                                    } else {
                                        grep("\\.xlsx*", unlist(y), ignore.case=TRUE, value=TRUE)
                                    }
                                }),
                      simplify=FALSE);
  ## Create URLs for selected files
  sel_urls <- paste0(options()$raustats["abs_domain"],
                     ## Replace all spaces with '%20'
                     gsub("\\s", "%20", sub("^/","", unlist(sel_paths))));

  
  
}


#' @name download_abs_data
#' @title Function to download files from the ABS website and store locally
#' @description TBC
#' @importFrom utils download.file unzip
#' @param data_urls Character vector specifying one or more ABS data
#'     URLs.
#' @return Downloads data from the ABS website and returns a character
#'     vector listing the location where files are saved.
#' @export
#' @examples
#'    x <- get_abs_data("3101.0");
#'    y <- get_abs_data("5206.0", tables=c("Table 1", "Table 2"));
#' 
#'    x <- get_abs_data("5206.0", tables="Table 1", release="Dec 2017");
download_abs_data <- function(data_urls) {
    ## Create local file names for storin 
    local_files <- sprintf("%s_%s.%s",
                           sub("^.+&(\\d+\\w+)\\.(zip|xlsx*).+$", "\\1", data_urls),
                           sub("^.+(\\d{2}).(\\d{2}).(\\d{4}).+$", "\\3\\2\\1", data_urls),
                           sub("^.+&(\\d+\\w+)\\.(zip|xlsx*).+$", "\\2", data_urls));

    ## -- Download files --
    mapply(function(x, y) utils::download.file(x, y, mode="wb"),
           data_urls,
           file.path(tempdir(), local_files));

    return(file.path(tempdir(), local_files));
}


#' @name download_abs_data
#' @title Function to download files from the ABS website and store locally
#' @description TBC
#' @importFrom utils download.file unzip
#' @param files One or more local zip files.
#' @return Downloads data from the ABS website and returns a character
#'     vector listing the location where files are saved.
#' @export
#' @examples
#'    x <- get_abs_data("3101.0");
#'    y <- get_abs_data("5206.0", tables=c("Table 1", "Table 2"));
#' 
#'    x <- get_abs_data("5206.0", tables="Table 1", release="Dec 2017");
unzip_abs_files <- function(files) {
    if (DEBUG) files <- file.path(tempdir(), local_files)
    ## Only extract
    files <- files[grepl("\\.zip$", files, ignore.case=TRUE)];
    sapply(files,
           function(x)
               if (grepl("\\.zip$", x, ignore.case=TRUE)) {
                   destdir <- sub("\\.zip", "", basename(x));
                   xl_files <- file.path(tempdir(), destdir, unzip(files[1], list=TRUE)$Name);
                   unzip(x, exdir=file.path(tempdir(), destdir));
               } else {
                   xl_files <- x;
               });
    return(xl_files);
}
        

#' @name get_abs_cat_tables
#' @title Return ABS catalogue tables
#' @description Return list of tables from specified ABS catalogue number
#' @importFrom rvest html_session follow_link html_attr
#' @importFrom xml2 read_xml read_html
#' @importFrom urltools url_parse url_compose
#' @param url Valid ABS data collection URL.
#' @return Returns a data frame listing the data collection tables and links.
#' @export
#' @examples
#'    url <- get_abs_data("5206.0", tables=c("Table 1"));
#'    tables <- get_abs_cat_tables(url);
get_abs_cat_tables <- function(url)
{
  ## Test URL is valid and Downloads page accessible
  s <- html_session(url);
  if (!options()$raustats["abs_downloads_regex"] %in% html_text(html_nodes(s, "a")))
    stop(sprintf("URL: %s is not a valid ABS catalogue link.", url));

  ## Return data table
  ## The ABS data catalogue lists the data inside a HTML table within a table, i.e.
  ##  <table>
  ##    <table> </table>
  ##  </table>
  ## The following code exploits this structure to extract the list of available tables
  ## and associated links.
  l <- follow_link(s, options()$raustats["abs_downloads_regex"])
  ht <- html_nodes(html_nodes(l, "table"), "table")
  nodes <- sapply(
        sapply(ht,
               function(x) 
                   html_nodes(x, "tr")),
        function(x)
            c(html_text(html_nodes(x, "td")),
              html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")),
        simplify=FALSE);
  dt <- as.data.frame(do.call(rbind, nodes), stringsAsFactors = FALSE);
  names(dt) <- paste0("x", seq_len(ncol(dt)));
  dt <- dt[grepl("^Table|All Time Series", dt$x1, ignore.case=TRUE), ];
  dt <- replace(dt, dt == "", NA_character_);
  dt <- dt[,colSums(is.na(dt)) < nrow(dt)]
  names(dt) <- c("table_name", "path1", "path2");
  return(dt);
 
  ## z <- url %>%
  ##   html_session %>%
  ##   follow_link( options()$raustats["abs_downloads_regex"]) %>%
  ##   html_nodes("table") %>% html_nodes("table") %>%
  ##   sapply(.,
  ##          function(x) 
  ##            x %>% html_nodes("tr") %>%
  ##            sapply(.,
  ##                   function(x)
  ##                     c(x %>% html_nodes("td") %>% html_text,
  ##                       x %>% html_nodes("td") %>% html_nodes("a") %>% html_attr("href"))
  ##                   ),
  ##          simplify=FALSE) %>%
  ##   unlist(recursive = FALSE) %>%
  ##   do.call(rbind, .) %>%
  ##   as.data.frame %>%
  ##   set_names(paste0("x", seq_len(ncol(.))));
  ## z  <- z[grepl("^Table|All Time Series", z$x1, ignore.case=TRUE), ] %>%
  ##   replace(., . == "", NA_character_) %>%
  ##   select_if(function(x) any(!is.na(x))) %>%
  ##   set_names("table_name", "path1", "path2");
  ## return(z);
}



#' @name read_abs
#' @title Read ABS time series data file(s)
#' @description This function extracts time series data from ABS data files. 
#' @importFrom readxl read_excel excel_sheets
#' @importFrom magrittr %>% set_names
#' @importFrom dplyr mutate mutate_at select rename left_join right_join
#' @importFrom tidyr gather
#' @importFrom rvest html_session follow_link html_attr
#' @export
#' @param files Names of one or more ABS data file
#' @param type One of 'ts' - time series or 'dem' - demographic data
#' @return data frame in long format
#' @examples
#' File <- file.path(DataDir, "5206001_Key_Aggregates.xls");
#' Data <- read_abs(File);
#' Data %>% as.data.frame %>% head;
readABS <- function(files, type="tss")
{
  x <- lapply(files,
              function(file)
                .readABS(file, type=type)
              ) %>%
    do.call(rbind, .)
}


.readABS <-
  function(file, type="ts")
{
  if (DEBUG) {
    file <- file.path(DataDir, "5206001_Key_Aggregates.xls");
  }
  .SheetNames <- file %>% excel_sheets;
  if (!all(c("Index", "Data1")  %in% .SheetNames))
    stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
  ## Read metadata
  .meta <- read_excel(file, "Index");
  ## Return pre-header information from ABS files 
  HeaderRow <- sapply(1:nrow(.meta),
                      function(i)
                        grepl("series\\s*id", paste(.meta[i,], collapse=" "), ignore.case=TRUE)) %>% which
  Metadata <- .meta                             %>%
    set_names(.meta[HeaderRow,]  %>%
              gsub("\\.","",.)   %>%
              gsub("\\s","_",.))                %>%  ## Rename variables
    .[-(1:HeaderRow),]                          %>%  ## Drop header rows
    .[,!is.na(names(.))]                        %>%  ## Drop empty columns
    .[grepl("\\w\\d{7}\\w", .$Series_ID),]      %>%
    mutate(Series_Start     = Series_Start %>% as.integer %>% as.Date.excel,
           Series_End       = Series_End   %>% as.integer %>% as.Date.excel,
           No_Obs           = No_Obs       %>% as.integer,
           Collection_Month = Collection_Month %>% as.integer);
  
  grepCatNo_Name <- "^.*(\\d{4}\\.\\d+(\\.\\d+)*)\\s+(.+)$";
  ## Note use of 'word' character    /here                /here for 13a, 6b, etc.
  grepTableName <- "^Table(s*)\\s+(\\w+(\\s+\\w+\\s+\\w+)*)\\.\\s+(.+)$";
  ## Get publication details
  CatNo_Name <- sapply(1:HeaderRow,
                       function(i)
                         grep(grepCatNo_Name, paste(.meta[i,] %>% c %>% .[!is.na(.)], collapse=" "),
                              ignore.case=TRUE, value=TRUE) %>% unlist %>%
                         sub(grepCatNo_Name, "\\1|\\3", ., ignore.case=TRUE) %>%
                         strsplit(., split="\\|")) %>% unlist %>% trimws;
  TableNo_Name <- sapply(1:HeaderRow,
                         function(i)
                           grep(grepTableName, paste(.meta[i,] %>% c %>% .[!is.na(.)], collapse=" "),
                                ignore.case=TRUE, value=TRUE) %>% unlist %>%
                           sub(grepTableName, "\\2|\\4", ., ignore.case=TRUE) %>%
                           strsplit(., split="\\|")) %>% unlist %>% trimws;
  
  ## Add publication details to Metadata table
  Metadata  %<>%
    mutate(Catalogue_No      = CatNo_Name[1],
           Publication_Title = CatNo_Name[2],
           Table_No          = TableNo_Name[1],
           Table_Title       = TableNo_Name[2]);
  ## Read data tables
  Data <- lapply(grep("data", .SheetNames, ignore.case=T, value=T),
                 function(SheetName) {
                   z <- readxl::read_excel(file, SheetName);
                   ## Return pre-header information from ABS files 
                   HeaderRow <- sapply(1:nrow(z),
                                       function(i)
                                         grepl("series\\s*id", paste(z[i,], collapse=" "),
                                               ignore.case=TRUE)) %>% which
                   z                                   %<>%
                     magrittr::set_names(z[HeaderRow,]      %>%
                               gsub("\\.","",.)   %>%
                               gsub("\\s","_",.))       %>%  ## Rename variables
                     dplyr::rename(Date = Series_ID)           %>%  ## Name Date field
                     .[-(1:HeaderRow),]                 %>%  ## Drop header rows
                     .[,!is.na(names(.))]               %>%  ## Drop empty columns
                     tidyr::gather(Series_ID, Value, -Date, convert=TRUE) %>%
                     dplyr::mutate(Date = Date %>% as.integer %>% as.Date.excel,
                                   Value = Value %>% as.numeric);
                   return(z);
                 })                                              %>%
    do.call(rbind, .);
  Data %<>% dplyr::left_join(Metadata, by=c("Series_ID"="Series_ID"))
}


### Function: read_abs
#' @name read_abs
#' @alias read_abs_ read_abs_tss read_abs
#' @title Read ABS time series data file(s)
#' @description This function extracts time series data from ABS data files. 
#' @importFrom readxl read_excel excel_sheets
#' @importFrom magrittr %>% set_names inset
#' @importFrom dplyr mutate mutate_at select rename left_join right_join
#' @importFrom tidyr gather
#' @export
#' @param files Names of one or more ABS data file [DEPRECATED]
#' @param catno Character vector specifying one or more ABS Catalogue numbers to download.
#' @param tables Regular expression denoting tables to download. If NULL (Default), downloads all time series spreadsheet tables for each specified catalogue. Use a list to specify different table sets for each specified ABS catalogue number.
#' @param type One of either 'tss' - time series spreadsheet (DEFALT or 'dem' - demographic data
#' @param ... other arguments to 
#' @return data frame in long format
#' @author David Mitchell <david.p.mitchell@@homemail.com.au>
#' @examples
#'   x <- file.path(DataDir, "5206001_Key_Aggregates.xls");
#' ABS.5206001 <- read_abs(File);
#' ABS.5206001 %>% as.data.frame %>% head;
read_abs <- function(files, catno, tables=NULL, type="tss") {
  x <- lapply(files,
              function(file)
                read_abs_(file, type=type)
  ) %>%
    do.call(rbind, .)
}


#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
read_abs_ <- function(file, type="tss") {
  require(magrittr);
  require(readxl)
  if (DEBUG) {
    file <- file.path(tempdir(), "6248055001TS0002_Jun 2007.xls");
  }
  .SheetNames <- file %>% excel_sheets %>% tolower;
  if (!all(c("index", "data1")  %in% .SheetNames))
    stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
  ## Read metadata
  .meta <- read_excel(file, sheet=file %>% excel_sheets %>%
                              grep("index", ., ignore.case=TRUE, value=TRUE));
  ## Return pre-header information from ABS files 
  HeaderRow <- sapply(1:nrow(.meta),
                      function(i)
                        grepl("series\\s*id", paste(.meta[i,], collapse=" "), ignore.case=TRUE)) %>% which;
  Metadata <- .meta %>%
    set_names(.meta[HeaderRow,]  %>%
                gsub("\\.","",.)  %>%
                gsub("\\s","_",.)) %>%               ## Rename variables
    .[-(1:HeaderRow),] %>%                           ## Drop header rows
    .[,!is.na(names(.))] %>%                         ## Drop empty columns
    .[grepl("\\w\\d{4,7}\\w", .$Series_ID),] %>%     ## Drop if Series ID invalid 
    mutate(Series_Start     = Series_Start %>% as.integer %>% as.Date.excel,
           Series_End       = Series_End   %>% as.integer %>% as.Date.excel,
           No_Obs           = No_Obs       %>% as.integer,
           Collection_Month = Collection_Month %>% as.integer);
  
  grepCatNo_Name <- "^.*(\\d{4}\\.\\d+(\\.\\d+)*)\\s+(.+)$";
  ## Note use of 'word' character    /here                /here for 13a, 6b, etc.
  grepTableName <- "^Table(s*)\\s+(\\w+(\\s+\\w+\\s+\\w+)*)\\.\\s+(.+)$";
  ## Get publication details
  CatNo_Name <- sapply(1:HeaderRow,
                       function(i)
                         grep(grepCatNo_Name, paste(.meta[i,] %>% na.remove, collapse=" "),
                              ignore.case=TRUE, value=TRUE)) %>% unlist %>%
    sub(grepCatNo_Name, "\\1|\\3", .) %>%
    strsplit(., split="\\|") %>% unlist %>% trimws;
  TableNo_Name <- sapply(1:HeaderRow,
                         function(i)
                           grep(grepTableName, paste(.meta[i,] %>% na.remove, collapse=" "),
                                ignore.case=TRUE, value=TRUE)) %>% unlist %>%
                  sub(grepTableName, "\\2|\\4", .) %>%
                  strsplit(., split="\\|") %>% unlist %>% trimws;
  
  ## Add publication details to Metadata table
  Metadata  %<>%
    mutate(Catalogue_No      = CatNo_Name[1],
           Publication_Title = CatNo_Name[2],
           Table_No          = TableNo_Name[1],
           Table_Title       = TableNo_Name[2]);
  ## Read data tables
  Data <- lapply(grep("data", .SheetNames, ignore.case=TRUE, value=TRUE),
                 function(SheetName) {
                   z <- read_excel(file, sheet=file %>% excel_sheets %>%
                                           grep(SheetName, ., ignore.case=TRUE, value=TRUE));
                   ## Return pre-header information from ABS files 
                   HeaderRow <- sapply(1:nrow(z),
                                       function(i)
                                         grepl("series\\s*id", paste(z[i,], collapse=" "), 
                                               ignore.case=TRUE)) %>% which
                   z                                   %<>%
                     set_names(z[HeaderRow,]      %>%
                                 gsub("\\.","",.)   %>%
                                 gsub("\\s","_",.)) %>%        ## Rename variables
                     rename(date = Series_ID) %>%              ## Name date field
                     .[-(1:HeaderRow),] %>%                    ## Drop header rows
                     .[,!is.na(names(.))] %>%                  ## Drop empty columns
                     gather(Series_ID, Value, -date, convert=TRUE) %>%
                     mutate(date = date %>% as.integer %>% as.Date.excel,
                            Value = Value %>% as.numeric);
                   return(z);
                 })                                              %>%
    do.call(rbind, .);
  Data %<>% left_join(Metadata, by=c("Series_ID"="Series_ID"))
}


### Update

## Where read_abs_() attempts to retrieve the Cat and Table metadata,
## you have the following (and similarly for CatNo_Name):

##   ## TableNo_Name <- sapply(1:HeaderRow,
##   ##                        function(i)
##   ##                          grep(grepTableName, paste(.meta[i,] %>% na.remove, collapse=" "),
##   ##                               ignore.case=TRUE, value=TRUE)) %>% unlist %>%
##   ##   sub(grepTableName, "\\2|\\4", .) %>%
##   ##   strsplit(., split="\\|") %>% unlist %>% trimws;


## This isn’t working for me, I assume due to some library issues—my
## environment is calling imputeTS::na.remove(), whereas I suspect
## you’re intending to call tseries::na.remove(), which can obviously
## be specified. (Also noting that tseries is commented out in
## 01-Libraries.R.)


## Alternatively, I can get a base solution with the following:

##   TableNo_Name <- sapply(1:HeaderRow,
##                          function(i)
##                              grep(grepTableName,
##                                   paste(.meta[i,] %>% unlist() %>%
##                                             .[complete.cases(.)],
##                                         collapse=" "),
##                                   ignore.case=TRUE, value=TRUE)) %>%
##       unlist %>%
##       sub(grepTableName, "\\2|\\4", ., ignore.case = TRUE) %>%
##       strsplit(., split="\\|") %>% unlist %>% trimws;

## Do you have a preference for the tseries or base approach?

## Further, the Engineering Construction tables seem to require that
## the sub(grepTableName, ...) has ignore.case = TRUE (to capture
## "TABLE").

## Can I save these changes to the files in Modelling\abs_data, or
## should I place a copy elsewhere?
