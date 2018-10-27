abs_ausstats_url <- function()
  paste0(options()$raustats["abs_domain"],
         options()$raustats["abs_ausstats_path"]);

#' @name abs_cat_data
#' @title Return data files from a specified url
#' @description TBC
#' @importFrom rvest html_session follow_link html_attr jump_to
#' @importFrom xml2 read_xml read_html
#' @param series Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param tables A character vector of regular expressions denoting tables to download. The default
#'   ('All') downloads all time series spreadsheet tables for each specified catalogue. Use a list
#'   to specify different table sets for each specified ABS catalogue number.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param type One of either 'tss' - time series spreadsheet (the default) or 'css' - cross-section
#'   spreadsheet.
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'    x <- abs_cat_data("3101.0");
#'    y <- abs_cat_data("5206.0", tables=c("Table 1", "Table 2"));
#'    z <- abs_cat_data("5206.0", tables="Table 1", release="Dec 2017");
abs_cat_data <- function(series, tables="All", releases="Latest", type="tss")
{
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
  cat_tables <- lapply(.paths,
                       function(x) {
                         .url <- jump_to(s, x)
                         z <- abs_cat_tables(.url$url)
                       });
  ## Select only the user specified tables ('sel_tables')
  if (length(tables) == 1 && tolower(tables) == "all") {
    ## If 'all' tables, download all
    sel_tables <- lapply(cat_tables,
                         function(x)
                           if (any(grepl("^all time series.*", x$table_name, ignore.case=TRUE))) {
                             ## Check whether all tables provided as single compressed archive and use
                             x[grepl("^all time series.*", x$table_name, ignore.case=TRUE),]
                           } else {
                             ## Else load all tables
                             x
                           });
  } else {
    ## Else, return only selected tables
    sel_tables <- lapply(cat_tables,
                         function(x) {
                           x[grepl(paste0("^(",
                                          paste(paste0(tables, "\\W.+" ), collapse="|"),
                                          ")"),
                                   x$table_name, ignore.case=TRUE),]
                         });
  }
  ## Select only the user specified tables ('sel_tables')
  sel_paths <- lapply(sel_tables,
                      function(x)
                        apply(x, 1, 
                              function(y) {
                                ## if zip in path1/path2, select zip file, else select xls(x) file
                                if (any(grepl("\\.zip", y, ignore.case=TRUE))) {
                                  grep("\\.zip", unlist(y), ignore.case=TRUE, value=TRUE)
                                } else {
                                  grep("\\.xlsx*", unlist(y), ignore.case=TRUE, value=TRUE)
                                }
                              }));
  ## Create URLs for selected files
  sel_urls <- paste0(options()$raustats["abs_domain"],
                     ## Replace all spaces with '%20'
                     gsub("\\s", "%20", sub("^/","", unlist(sel_paths))));
  ## Combine data into 
  z <- lapply(sel_urls, abs_download_data);
  z <- lapply(z, abs_unzip_files);
  data <- lapply(z, abs_read_tss);
  data <- do.call(rbind, data);
  rownames(data) <- seq_len(nrow(data));
  return(data);
}


#' @name abs_download_data
#' @title Function to download files from the ABS website and store locally
#' @description TBC
#' @importFrom utils download.file unzip
#' @param data_urls Character vector specifying one or more ABS data
#'     URLs.
#' @return Downloads data from the ABS website and returns a character
#'     vector listing the location where files are saved.
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#' 
abs_download_data <- function(data_urls) {
  local_filenames <- abs_local_filename(data_urls);
  ## -- Download files --
  mapply(function(x, y) download.file(x, y, mode="wb"),
         data_urls,
         file.path(tempdir(), local_filenames));
  ## Return results
  return(file.path(tempdir(), local_filenames));
}

#' @name abs_local_filename
#' @title Create local file names for storing downloaded ABS data files
#' @description Function to create local filename from web-based file name
#' @param url Character vector specifying one or more ABS data URLs.
#' @return Returns a local file names (character vector) in which downloaded files will be saved.
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
abs_local_filename <- function(url)
{
  sprintf("%s_%s.%s",
          sub("^.+&(\\w+)\\.(zip|xlsx*).+$", "\\1", data_urls),
          sub("^.+(\\d{2}).(\\d{2}).(\\d{4}).+$", "\\3\\2\\1", data_urls),
          sub("^.+&(\\w+)\\.(zip|xlsx*).+$", "\\2", data_urls));
}


#' @name abs_unzip_files
#' @title Function to download files from the ABS website and store locally
#' @description TBC
#' @importFrom utils download.file unzip
#' @param files One or more local zip files.
#' @return Downloads data from the ABS website and returns a character
#'     vector listing the location where files are saved.
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
abs_unzip_files <- function(files) {
  ## Only extract from zip files
  files <- files[grepl("\\.zip$", files, ignore.case=TRUE)];
  xl_files <- sapply(files,
                     function(x)
                       if (grepl("\\.zip$", x, ignore.case=TRUE)) {
                         destdir <- sub("\\.zip", "", basename(x));
                         unzip(x, exdir=file.path(tempdir(), destdir));
                         file.path(tempdir(), destdir, unzip(x, list=TRUE)$Name);
                       } else {
                         x;
                       });
  return(xl_files);
}


#' @name abs_cat_tables
#' @title Return ABS catalogue tables
#' @description Return list of tables from specified ABS catalogue number
#' @importFrom rvest html_session follow_link html_attr
#' @importFrom xml2 read_xml read_html
#' @param url Valid ABS data collection URL.
#' @return Returns a data frame listing the data collection tables and links.
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'    url <- abs_cat_data("5206.0", tables=c("Table 1"));
#'    tables <- abs_cat_tables(url);
abs_cat_tables <- function(url)
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
  nodes <- lapply(
    sapply(ht,
           function(x) 
             html_nodes(x, "tr")),
    function(x)
      c(html_text(html_nodes(x, "td")),
        html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")));
  dt <- suppressWarnings(as.data.frame(do.call(rbind, nodes), stringsAsFactors = FALSE));
  names(dt) <- paste0("x", seq_len(ncol(dt)));
  dt <- dt[grepl("^Table|All Time Series", dt$x1, ignore.case=TRUE), ];
  dt <- replace(dt, dt == "", NA_character_);
  dt <- dt[,colSums(is.na(dt)) < nrow(dt)]
  names(dt) <- c("table_name", "path1", "path2");
  return(dt);
}


### Function: abs_read_tss
#' @name abs_read_tss
#' @aliases abs_read_tss abs_read_tss_
#' @title Read ABS time series data file(s)
#' @description This function extracts time series data from ABS data files.
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @param files Names of one or more ABS data file [DEPRECATED]
#' @param type One of either 'tss' - time series spreadsheet (DEFAULT
#'     or 'dem' - demographic data
#' @param ... other arguments to ... 
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'   x <- abs_read_tss(file.path("data-raw", "5206001_Key_Aggregates.xls"));
#'   y <- abs_read_tss(file.path("data-raw", c("5206001_Key_Aggregates.xls","5206002_expenditure_volume_measures.xls")));
abs_read_tss <- function(files, type="tss") {
  x <- lapply(files,
              function(file)
                abs_read_tss_(file, type=type));
  z <- do.call(rbind, x);
  return(z);
}


abs_read_tss_ <- function(file, type="tss") {
  sheet_names <- tolower(excel_sheets(file));
  if (!all(c("index", "data1")  %in% sheet_names))
    stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
  ## -- Read metadata --
  .meta <- read_excel(file,
                      sheet = grep("index", excel_sheets(file), ignore.case=TRUE, value=TRUE));
  ## Return pre-header information from ABS files 
  header_row <- which(sapply(1:nrow(.meta),
                             function(i)
                               grepl("series\\s*id", paste(.meta[i,], collapse=" "), ignore.case=TRUE)));
  metadata <- .meta;
  names(metadata) <- tolower(gsub("\\s","_",
                                  gsub("\\.", "",
                                       .meta[header_row,])));         ## Rename variables
  metadata <- metadata[-(1:header_row), !is.na(names(metadata))];     ## Drop header rows & empty columns
  metadata <- metadata[complete.cases(metadata),];                    ## Drop NA rows
  metadata <- metadata[grepl("\\w\\d{4,7}\\w", metadata$series_id),]; ## Drop if Series ID invalid 
  metadata <- transform(metadata,
                        series_start     = excel2Date(as.integer(series_start)),
                        series_end       = excel2Date(as.integer(series_end)),
                        no_obs           = as.integer(no_obs),
                        collection_month = as.integer(collection_month));
  ##
  ## Get publication details
  ## -- Catalogue number & name --
  regex_catno_name <- "^.*(\\d{4}\\.\\d+(\\.\\d+)*)\\s+(.+)$";
  catno_name <- sapply(1:header_row,
                       function(i)
                         grep(regex_catno_name, paste(.meta[i,], collapse=" "),
                              ignore.case=TRUE, value=TRUE));
  catno_name <- gsub("(\\s*NA)+", "", sub(regex_catno_name, "\\1|\\3", unlist(catno_name), ignore.case=TRUE));
  catno_name <- trimws(unlist(strsplit(catno_name, split="\\|")));
  ##
  ## -- Table number & name --
  regex_table_name <- "^.*Tables*\\s+(\\w+(\\s+\\w+\\s+\\w+)*)\\.*\\s+(.+)$";
  ## Note use of 'word' character    ^here                ^here for 13a, 6b, etc.
  tableno_name <- sapply(1:header_row,
                         function(i)
                           grep(regex_table_name,
                                paste(.meta[i,], collapse=" "),
                                ignore.case=TRUE, value=TRUE));
  tableno_name <- gsub("(\\s*NA)+", "",
                       sub(regex_table_name, "\\1|\\3", unlist(tableno_name), ignore.case=TRUE));
  tableno_name <- trimws(unlist(strsplit(tableno_name, split="\\|")));
  ##
  ## Add publication details to metadata table
  metadata  <- transform(metadata,
                         catalogue_no      = catno_name[1],
                         publication_title = catno_name[2],
                         table_no          = tableno_name[1],
                         table_title       = tableno_name[2]);
  ## Extract data
  data <- lapply(grep("data", excel_sheets(file), ignore.case=TRUE, value=TRUE),
                 function(sheet_name) {
                   z <- read_excel(file, sheet=sheet_name);
                   ## Return pre-header information from ABS files 
                   header_row <- which(sapply(1:nrow(z),
                                              function(i)
                                                grepl("series\\s*id", paste(z[i,], collapse=" "), 
                                                      ignore.case=TRUE)));
                   names(z) <- gsub("\\s","_",
                                    gsub("\\.","", z[header_row,]));       ## Rename variables
                   names(z) <- sub("series_id", "date", names(z), ignore.case=TRUE); ## Rename Series_ID field
                   z <- z[-(1:header_row), !is.na(names(z))];              ## Drop empty columns
                   z <- gather(z, series_id, value, -date, convert=TRUE);  ## Transform data to key:value pairs
                   z <- transform(z,
                                  date = excel2Date(as.integer(date)),
                                  value = as.numeric(value));
                   names(z) <- tolower(names(z));
                   return(z);
                 });
  data <- do.call(rbind, data);
  data <- left_join(data, metadata, by="series_id");
  data <- data[complete.cases(data),];
  names(data) <- tolower(names(data));
  return(data);
}
