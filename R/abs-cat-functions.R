### ABS Catalogue functions

#' @name abs_urls
#' @title ABS URL addresses and paths used in accessing ABS Catalogue data calls
#' @description This function returns a list of URLs and data paths used to construct ABS Catalogue
#'   data access calls. It is used in other functions in this package and need not be called
#'   directly.
#' @return a list with a base url and a url section for formatting ABS Catalogue statistics calls
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_urls <- function()
{
  list(base_url = "https://www.abs.gov.au",
       ausstats_path = "ausstats/abs@.nsf",
       mf_path = "mf",
       downloads_regex = "Downloads",
       releases_regex = "Past.*Future.*Releases");
}


#' @name abs_filetypes
#' @title Valid ABS file types
#' @description This function returns a vector of valid ABS file types for using list of URLs and data paths used to construct ABS Catalogue
#'   data access calls. It is used in other functions in this package and need not be called
#'   directly.
#' @return a vector containing a list of valid ABS file types.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_filetypes <- function()
{
  c(zip_files = "application/x-zip",
    excel_files = "application/vnd.ms-excel",
    openxml_files = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    pdf_files = "application/pdf");
}


#' @name abs_cat_stats
#' @title Get ABS catalogue series data
#' @description This function downloads ABS catalogue series statistics, by ABS catalogue number.
#' @importFrom rvest html_session follow_link html_attr jump_to
#' @importFrom xml2 read_xml read_html
#' @param cat_no Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param tables A character vector of regular expressions denoting tables to download. The default
#'   ('All') downloads all time series spreadsheet tables for each specified catalogue. Use a list
#'   to specify different table sets for each specified ABS catalogue number.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param types One of either 'tss' -- ABS time series spreadsheet (the default) or 'css' -- ABS
#'   data cube (cross-section spreadsheet).
#' @param na.rm logical (default: \code{TRUE}) - remove observations containing missing values.
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## Download quarterly Australian National Accounts, Tables 1 & 2 
#'     ana_q <- abs_cat_stats("5206.0", tables=c("Table 1\\W+", "Table 2\\W+"));
#'
#'     ## Download December 2017 Australian National Accounts, Table 1
#'     ana_q_2017q4 <- abs_cat_stats("5206.0", tables="Table 1\\W+", release="Dec 2017");
#'   }
abs_cat_stats <- function(cat_no, tables="All", releases="Latest", types="tss", na.rm=TRUE)
{
  if (missing(cat_no))
    stop("No cat_no supplied.");
  ## if (tolower(releases) != "latest" ||
  ##     releases IS NOT A DATE )
  ##   stop("releases arguments ")
  if (any(!types %in% c("tss","css")))
    stop("Allowable type arguments limited to one or both: 'tss' and 'css'.");
  ## Get available catalogue tables
  if (FALSE) {
    cat_no <- "5206.0"; tables <- c("Table 1\\W+", "Table 2\\W+");
    releases <- "Latest"; types <- "tss"; include_urls <- FALSE;
  }
  cat_tables <- abs_cat_tables(cat_no=cat_no, releases=releases, types=types, include_urls=TRUE)
  ## Select only the user specified tables ('sel_tables')
  if (length(tables) == 1 && tolower(tables) == "all") {
    ## If 'all' tables, download all
    sel_tables <- if (any(grepl("^all time series.*", cat_tables$item_name, ignore.case=TRUE))) {
                    ## If all tables provided as single compressed archive, select that
                    cat_tables[grepl("^all time series.*", cat_tables$item_name, ignore.case=TRUE),]
                  } else {
                    ## Else, select all tables
                    cat_tables
                  };
  } else {
    ## Else, return only selected tables
    sel_tables <- cat_tables[grepl(sprintf("(%s)", paste(tables, collapse="|")),
                                   cat_tables$item_name, ignore.case=TRUE),]
    ## Stop if regular expression does not return any tables
    if (nrow(sel_tables) == 0)
      stop(paste("Specified table regular expressions do not match any table names, re-specify."))
  }
  ## Select only the user specified tables ('sel_tables')
  sel_urls <- apply(sel_tables, 1,
                    function(y) {
                      ## If zip in path_zip, select zip file, else select xls(x) file
                      if (any(grepl("\\.zip", y, ignore.case=TRUE))) {
                        unique(grep("\\.zip", unlist(y), ignore.case=TRUE, value=TRUE))
                      } else {
                        unique(grep("\\.xlsx*", unlist(y), ignore.case=TRUE, value=TRUE))
                      }
                    });
  ## Download ABS TSS/Data Cubes ..
  z <- lapply(sel_urls, abs_cat_download);
  z <- lapply(z,
              function(x) 
                if (!grepl("\\.zip", x, ignore.case=TRUE)) {
                  x
                } else {
                  abs_cat_unzip(files=x)
                });
  ## .. and combine into single data frame
  data <- lapply(z, function(x) abs_read_tss(x, na.rm=na.rm));
  data <- do.call(rbind, data);
  rownames(data) <- 1:nrow(data);
  return(data);
}


#' @name abs_cat_tables
#' @title Return ABS catalogue tables
#' @description Return list of data tables available from specified ABS catalogue number.
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom httr http_error
#' @importFrom dplyr case_when bind_rows
#' @param cat_no ABS catalogue numbers.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param types ABS publication types to return. Permissable options include one or more of: 'tss'
#'   -- ABS Time Series Spreadsheets, 'css' - ABS Data Cubes and 'pub' -- ABS Publications. The
#'   default returns all Time Series Spreadsheets and Data Cubes.
#' @param include_urls Include full URLs to returned ABS data files. Default (FALSE) does not
#'   include data file URLs.
#' @return Returns a data frame listing the data collection tables and URLs for Excel (column:
#'   \code{path_xls}) and, if available, Zip (column: \code{path_zip}) files.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## List latest available quarterly National Accounts tables
#'     ana_tables <- abs_cat_tables("5206.0", releases="Latest");
#'     ana_tables_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
#'
#'     ## List latest available CPI Time Series Spreadsheet tables only
#'     cpi_tables <- abs_cat_tables("6401.0", releases="Latest", types="tss");
#'     cpi_tables_url <- abs_cat_tables("5206.0", releases="Latest", types="tss", include_urls=TRUE);
#'   
#'     ## List latest available ASGS Volume 3 Data Cubes
#'     asgs_vol3_tables <- abs_cat_tables("1270.0.55.003", releases="Latest", types="css");
#'     asgs_vol3_tables_url <- abs_cat_tables("1270.0.55.003", releases="Latest",
#'                                            types="css", include_urls=TRUE);
#'   
#'     ## List latest available ASGS ANZSIC publications (PDF) files
#'     anzsic_2006 <- abs_cat_tables("1292.0", releases="Latest", types="pub", include_urls=TRUE);
#'   }
abs_cat_tables <- function(cat_no, releases="Latest", types=c("tss", "css"), include_urls=FALSE)
{
  ## if (FALSE) {
  ##   -- DEBUGGING CODE --
  ## cat_no <- "6401.0"; types <- "tss"; releases <- "Latest"; include_urls <- TRUE;
  ## cat_no <- "5209.0.55.001"; types <- "css"; releases <- "Latest"; include_urls <- TRUE;
  ## cat_no <- "1270.0.55.001"; releases <- "Latest"; types <- "css"; include_urls <- TRUE;
  ## cat_no <- "6202.0"; releases <- "Latest"; types <- "css"; include_urls <- TRUE;
  ## cat_no <- "3105.0.65.001"; releases <- "Latest"; types <- "css"; include_urls <- TRUE;
  ## }
  if (missing(cat_no))
    stop("No cat_no supplied.");
  if (any(!types %in% c("tss", "css", "pub")))
    stop("Allowable type arguments limited to one or more of: 'tss', 'css' or 'pub'.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  ## Spell out type -- for ABS website scraping
  types <- sapply(types,
                     function(x) switch(x,
                                        "tss" = "Time Series Spreadsheet",
                                        "css" = "Data Cubes",
                                        "pub" = "Publication"));
  ## Create ABS URL and open session 
  url <- file.path(abs_urls()$base_url, abs_urls()$ausstats_path, abs_urls()$mf_path, cat_no);
  ## Check for HTTP errors
  raustats_check_url_available(url);
  ## -- OLD CODE --
  ## if (http_error(url))
  ##   stop(sprintf("File cannot be downloaded. Check URL: %s", url))
  ## Open html session
  suppressWarnings(s <- html_session(url));
  releases <- unique(releases);
  if (length(releases) == 1 && tolower(releases) == "latest") {
    .paths <- "";
  } else {
    ## Get path to 'Past & Future Releases' page
    .paths <- html_nodes(s, "a");
    .paths <- .paths[grepl(abs_urls()$releases_regex, .paths)];
    .paths <- html_attr(.paths, "href");
    s <- jump_to(s, .paths)
    .paths <- html_nodes(s, "a");
    .paths <- .paths[grepl(paste(releases, collapse="|"), .paths, ignore.case=TRUE)];
    .paths <- html_attr(.paths, "href");
  }
  ## Return list of all downloadable files, for specified catalogue tables ('cat_tables')
  v <- lapply(.paths,
              function(x) {
                ## Check for HTTP errors
                ## raustats_check_url_available(file.path(s, x));
                y <- jump_to(s, x)
                l <- follow_link(y, abs_urls()$downloads_regex)
                ht <- html_nodes(html_nodes(l, "table"), "table")
                ## Return data table
                ## The ABS data catalogue lists the data inside a HTML table within a table, i.e.
                ##  <table>
                ##    <table> </table>
                ##  </table>
                ## The following nested apply functions, exploits this structure to extract the
                ## list of available publication types and associated links.
                all_nodes <- lapply(sapply(ht, function(x) html_nodes(x, "tr")),
                                    function(x)
                                      c(html_text(html_nodes(x, "td")),
                                        ## html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")));
                                        paste0(abs_urls()$base_url,
                                               html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")))
                                    );
                ## Remove ABS data download section heading from all_nodes
                ##   Where ABS data download section titles that include links, are included
                ##   in the node set, but are not conformant with publication information.
                ##   The following code block, removes these entries.
                data_nodes <- lapply(all_nodes,
                                     function(x) {
                                       if (grepl(paste(c("(^\\W{0,1}$)",
                                                         "(^data\\s*cubes\\W*$)",
                                                         "(^time series spreadsheet\\W*$)"),
                                                       collapse="|"),
                                                 x[1], ignore.case=TRUE)) {
                                         NULL
                                       } else {
                                         x
                                       }
                                     })
                data_nodes <- data_nodes[-which(sapply(data_nodes, is.null))];
                ## Tidy and return data set names and urls
                nodes <- data_nodes[unlist(lapply(data_nodes,
                                                  function(x)
                                                    any(grepl(sprintf("(%s)",
                                                                      paste(types, collapse="|")),
                                                              x, ignore.case=TRUE)) &
                                                    any(grepl("ausstats", x, ignore.case=TRUE))
                                                  ))];
                ## Remove non-breaking spaces (&nbsp;), and blank entries
                nodes <- lapply(nodes,
                                function(x) {
                                  z <- trimws(gsub("\u00a0", "", x));      ## Remove non-breaking spaces
                                  z <- replace(z, z == "", NA_character_); ## Replace blank objects with NA
                                  ## Set entries not starting with 'https*' with 'NA_character_'
                                  z[-1] <- replace(z[-1],                          
                                                   !grepl("^https*.+", z[-1], ignore.case=TRUE),
                                                   NA_character_);
                                  ## Set entries containing 'INotes' with 'NA_character_'
                                  z <- replace(z,                          
                                               grepl("INotes", z, ignore.case=TRUE),
                                               NA_character_);
                                  z <- z[!is.na(z)];                       ## Remove NA objects
                                  ## Set object names: First element = 'item_name'
                                  names(z)[1] <- "item_name";
                                  names(z)[-1] <- case_when(
                                    ## !grepl("(^https*|^Releases|INotes)", z, ignore.case=TRUE) ~ "item_name",
                                    grepl("\\.xlsx*", z[-1], ignore.case=TRUE) ~ "path_xls",
                                    grepl("\\.zip", z[-1], ignore.case=TRUE) ~ "path_zip",
                                    grepl("\\.pdf", z[-1], ignore.case=TRUE) ~ "path_pdf",
                                    TRUE ~ NA_character_)
                                  z <- as.data.frame(t(cbind.data.frame(z, deparse.level=1)),
                                                     stringsAsFactors=FALSE);
                                  return(z);
                                });
                ## Tidy nodes into data.frame (using dplyr::bind_rows)
                dt <- suppressWarnings(bind_rows(nodes))
                ## Lastly replace spaces in all URL paths with '%20' string
                for(name in grep("^path_", names(dt), ignore.case=TRUE, value=TRUE)) # names(dt)[-1]
                  dt[,name] <- gsub("\\s+", "%20", dt[,name]);
                return(dt);
              });
  ## Add catalogue number and release information to table
  v <- lapply(seq_along(v),
              function(i) {
                v[[i]]$release <- sub("^$", "Latest", releases[i]);
                v[[i]]$cat_no <- cat_no;
                as.data.frame(v)
              });
  ## Bind all results together
  z <- do.call(rbind, v);
  ## If rbind breaks on different row names try:
  ## z <- do.call(function(...) rbind(..., make.row.names=FALSE), v);
  ## names(z) <- c("item_name", ..., "cat_no", "release");
  z <- if (!include_urls) {
         z[,c("cat_no", "release", "item_name")]
       } else {
         z[,c("cat_no", "release", "item_name",
              names(z)[!names(z) %in% c("cat_no", "release", "item_name")])]
       }
  row.names(z) <- seq_len(nrow(z));
  return(z)
}


#' @name abs_cat_releases
#' @title Return ABS catalogue table releases
#' @description Return list of all releases available for specified ABS catalogue number.
#' @importFrom rvest html_session html_table html_text html_nodes html_attr follow_link
#' @importFrom httr http_error
#' @param cat_no ABS catalogue numbers.
#' @param include_urls Include full path URL to specified ABS catalogue releases. Default (FALSE)
#'   does not include release URLs.
#' @return Returns a data frame listing available ABS catalogue releases.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## List all available quarterly National Accounts tables
#'     ana_releases <- abs_cat_releases("5206.0");
#'     ana_release_urls <- abs_cat_releases("5206.0", include_urls=TRUE);
#'   
#'     ## List latest available CPI Time Series Spreadsheet tables only
#'     cpi_releases <- abs_cat_releases("6401.0");
#'     cpi_release_urls <- abs_cat_releases("6401.0", include_urls=TRUE);
#'   }
abs_cat_releases <- function(cat_no, include_urls=FALSE)
{
  ## if (FALSE) {
  ##   ## -- DEBUGGING CODE --
  ##   cat_no <- "5206.0"
  ##   include_urls <- FALSE
  ## }
  if (missing(cat_no))
    stop("No cat_no supplied.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  ## Create ABS URL and open session 
  url <- file.path(abs_urls()$base_url, abs_urls()$ausstats_path, abs_urls()$mf_path, cat_no);
  ## Check for HTTP errors
  raustats_check_url_available(url)
  ## if (http_error(url))
  ##   stop(sprintf("File cannot be downloaded. Check URL: %s", url))
  suppressWarnings(s <- html_session(url));
  ## Get path to 'Past & Future Releases' page
  .paths <- html_nodes(s, "a");
  .paths <- .paths[grepl(abs_urls()$releases_regex, .paths)];
  .paths <- html_attr(.paths, "href");
  s <- jump_to(s, .paths)
  ## Get list of available ABS catalogue releases (See: https://devhints.io/xpath for Xpath hints)
  .tables <- html_nodes(s, "table");
  .tables <- .tables[grepl("Past Releases", .tables, ignore.case=TRUE)];
  .paths <- html_nodes(.tables, "a");
  ## Return results 
  if (!include_urls) {
    z <- data.frame(releases = html_text(.paths))
  } else {
    z <- data.frame(releases = html_text(.paths),
                    urls = file.path(abs_urls()$base_url,
                                     abs_urls()$ausstats_path,
                                     html_attr(.paths, "href")))
  }
  row.names(z) <- seq_len(nrow(z));
  return(z)
}


#' @name abs_cat_download
#' @title Function to download files from the ABS website and store locally
#' @description Downloads specified ABS catalogue data files from the ABS website, using a valid ABS
#'   data table URL.
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
             this_filename <- abs_local_filename(url);
             ## Check if any data_urls are not ABS data URLs
             if (!grepl("^https*:\\/\\/www\\.abs\\.gov\\.au\\/ausstats.+",
                        url, ignore.case=TRUE))	
               stop(sprintf("Invalid ABS url: %s", url));
             ##
             ## -- Download files --
             cat(sprintf("Downloading: %s", this_filename));
             ## Check for errors
             raustats_check_url_available(url)
             resp <- GET(url, write_disk(file.path(exdir, this_filename), overwrite=TRUE),
                         raustats_ua(), progress());
             ## ## File download validation code based on:
             ## ##  https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
             ## if (http_error(resp)) {
             ##   stop(
             ##     sprintf(
             ##       "ABS catalogue file request failed (Error code: %s)\nInvalid URL: %s", 
             ##       status_code(resp),
             ##       url
             ##     ),
             ##     call. = FALSE
             ##   )
             ## }
             ## Check content-type is compliant
             if (!http_type(resp) %in% abs_filetypes()) {
               stop("ABS file request did not return Excel, Zip or PDF file", call. = FALSE)
             }
             return(file.path(exdir, this_filename));
           })
    ## local_filename <- abs_local_filename(data_url);
  ## ## Check if any data_urls are not ABS data URLs
  ## if (!grepl("^https*:\\/\\/www\\.abs\\.gov\\.au\\/ausstats.+",
  ##            data_url, ignore.case=TRUE))	
  ##   stop(sprintf("Invalid ABS url: %s", data_url));
  ## ##
  ## ## -- Download files --
  ## cat(sprintf("Downloading: %s", local_filename));
  ## resp <- GET(data_url, write_disk(file.path(exdir, local_filename), overwrite=TRUE),
  ##             raustats_ua(), progress());
  ## ## File download validation code based on:
  ## ##  https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
  ## if (http_error(resp)) {
  ##   stop(
  ##     sprintf(
  ##       "ABS catalogue file request failed (Error code: %s)\nInvalid URL: %s", 
  ##       status_code(resp),
  ##       data_url
  ##     ),
  ##     call. = FALSE
  ##   )
  ## }
  ## ## Check content-type is compliant
  ## if (!http_type(resp) %in% abs_filetypes()) {
  ##   stop("ABS file request did not return Excel, Zip or PDF file", call. = FALSE)
  ## }
  ## Return results
  ## return(file.path(exdir, local_filename));
  return(local_filenames);
}


#' @name abs_local_filename
#' @title Create local file names for storing downloaded ABS data files
#' @description Function to create local filename from web-based file name.
#' @param url Character vector specifying one or more ABS data URLs.
#' @return Returns a local file names (character vector) in which downloaded files will be saved.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_local_filename <- function(url)
{
  sprintf("%s_%s.%s",
          sub("^.+&(.+)\\.(zip|xlsx*|pdf)&.+$", "\\1", url),
          sub("^.+(\\d{2}).(\\d{2}).(\\d{4}).+$", "\\3\\2\\1", url),
          sub("^.+&(.+)\\.(zip|xlsx*|pdf)&.+$", "\\2", url));
}


#' @name abs_cat_unzip
#' @title Uncompress locally-stored ABS Catalogue data file archives
#' @description Function to uncompress locally-stored ABS Catalogue data file archives.
#' @importFrom utils unzip zip
#' @param files One or more local zip files.
#' @param exdir Target directory for extracted archive files. Directory is created if it doesn't
#'   exist. If missing, creates a new subdirectory in \code{tempdir()} using the respective zip
#'   files (specified in \code{files}.
#' @return Returns a character vector listing the names of all files extracted.
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


### Function: abs_read_tss
#' @name abs_read_tss
#' @title Extract data from an ABS time series data file
#' @description This function extracts time series data from ABS data files.
#' @param files Names of one or more ABS data files
#' @param type One of either 'tss' -- ABS Time Series Spreadsheet (the Default) or 'css' -- Data
#'   Cube.R
#' @param na.rm logical. If \code{TRUE} (default), remove observations containing missing values.
#' @return data frame in long format
#' @export
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
#' @param type One of either 'tss' -- ABS Time Series Spreadsheet (the Default) or 'css' -- Data
#'   Cube.R
#' @param na.rm logical. If \code{TRUE} (default), remove observations containing missing values.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_read_tss_ <- function(file, type="tss", na.rm=na.rm) {
  ## Avoid 'No visible binding for global variables' note
  { series_start <- series_end <- no_obs <- collection_month <- series_id <- value <- NULL }
  
  sheet_names <- tolower(excel_sheets(file));
  if (!all(c("index", "data1")  %in% sheet_names))
    stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
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
                         table_title       = tableno_name[2]);
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
                   z <- z[-(1:header_row), !is.na(names(z))];             ## Drop empty columns
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
