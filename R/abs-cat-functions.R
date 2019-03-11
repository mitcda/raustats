abs_urls <- function()
{
  list(base_url = "https://www.abs.gov.au",
       ausstats_path = "ausstats/abs@.nsf/mf",
       downloads_regex = "Downloads",
       releases_regex = "Past.*Future.*Releases");
}


#' @name abs_cat_stats
#' @title Get ABS catalogue series data
#' @description TBC
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
abs_cat_stats <- function(cat_no, tables="All", releases="Latest", types="tss")
{
  if (missing(cat_no))
    stop("No cat_no supplied.");
  ## if (tolower(releases) != "latest" ||
  ##     releases IS NOT A DATE )
  ##   stop("releases arguments ")
  if (any(!types %in% c("tss","css")))
    stop("Allowable type arguments limited to one or both: 'tss' and 'css'.");
  ## Get available catalogue tables
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
                      ## If zip in path1/path2, select zip file, else select xls(x) file
                      if (any(grepl("\\.zip", y, ignore.case=TRUE))) {
                        grep("\\.zip", unlist(y), ignore.case=TRUE, value=TRUE)
                      } else {
                        grep("\\.xlsx*", unlist(y), ignore.case=TRUE, value=TRUE)
                      }
                    });
  ## Download ABS TSS/Data Cubes ..
  z <- lapply(sel_urls, abs_cat_download);
  z <- lapply(z, abs_cat_unzip);
  ## .. and extract into single data frame
  data <- lapply(z, abs_read_tss);
  data <- do.call(rbind, data);
  rownames(data) <- seq_len(nrow(data));
  return(data);
}


#' @name abs_cat_tables
#' @title Return ABS catalogue tables
#' @description Return list of tables from specified ABS catalogue number
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom httr http_error 
#' @param cat_no ABS catalogue numbers.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param types ABS publication types to return. Permissable options include one or more of: 'tss'
#'   -- ABS Time Series Spreadsheets, 'css' - ABS Data Cubes and 'pub' -- ABS Publications. The
#'   default returns all Time Series Spreadsheets and Data Cubes.
#' @param include_urls Include full URLs to returned ABS data files. Default (FALSE) does not
#'   include data file URLs.
#' @return Returns a data frame listing the data collection tables and links.
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
  if (missing(cat_no))
    stop("No cat_no supplied.");
  ## if (tolower(releases) != "latest" ||
  ##     releases IS NOT A DATE )
  ##   stop("releases arguments ")
  if (any(!types %in% c("tss", "css", "pub")))
    stop("Allowable type arguments limited to one or more of: 'tss', 'css' or 'pub'.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  ## Spell out type -- for ABS website scraping
  types <- sapply(types,
                     function(x) switch(x,
                                        "tss" = "Time Series Spreadsheet",
                                        "css" = "Data Cube",
                                        "pub" = "Publication"));
  ## Create ABS URL and open session 
  url <- file.path(abs_urls()$base_url, abs_urls()$ausstats_path, cat_no);
  ## Check for HTTP errors
  if (http_error(url))
    stop(sprintf("File cannot be downloaded: %s", url))
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
                y <- jump_to(s, x)
                ## z <- abs_cat_tables(.url$url)
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
                                               html_attr(html_nodes(html_nodes(x, "td"), "a"), "href"))
                                        ));
                nodes <- all_nodes[unlist(lapply(all_nodes,
                                                 function(x) any(grepl(sprintf("(%s)",
                                                                               paste(types, collapse="|")),
                                                                       x, ignore.case=TRUE)) &
                                                             any(grepl("ausstats", x, ignore.case=TRUE))
                                                 ))];
                ## Remove non-breaking space (&nbsp;) and blank entries
                nodes <- lapply(nodes,
                                 function(x) {
                                   z <- trimws(gsub("\u00a0", "", x));
                                   z <- replace(z, z == "", NA_character_);
                                   z <- z[!is.na(z)]
                                 });
                ## Tidy HTML return into data.frame
                dt <- suppressWarnings(as.data.frame(do.call(rbind, nodes), stringsAsFactors = FALSE));
                ## Check if non-'path' columns contain types string, and discard if not
                idx <- sapply(names(dt)[-1],
                              function(x) any(grepl(sprintf("(%s)",
                                                            paste(types, collapse="|")),
                                                    dt[,x], ignore.case=TRUE))
                              );
                dt <- dt[, c(TRUE, idx)];
                ## Specify dt column names
                names(dt) <- c("item_name", paste("path", seq_len(ncol(dt)-1), sep="_"));
                ## Lastly replace spaces in URL paths with '%20' string
                for(name in names(dt)[-1])
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


#' @name abs_cat_download
#' @title Function to download files from the ABS website and store locally
#' @description Gets files from ABS website specified by URL
#' @importFrom httr GET http_type http_error progress status_code write_disk 
#' @param data_url Character vector specifying an ABS data URLs.
#' @param exdir Target directory for downloaded files (defaults to \code{tempdir()}). Directory is
#'   created if it doesn't exist.
#' @return Downloads data from the ABS website and returns a character vector listing the location
#'   where files are saved.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
abs_cat_download <- function(data_url, exdir=tempdir()) {
  if (FALSE) {
    exdir <- tempdir()
    abs_tables_5206_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
    data_url <- head(abs_tables_5206_url$path_1, 1)
    xx <- abs_cat_download(data_url);
  }
  if (!dir.exists(exdir)) dir.create(exdir);
  local_filename <- abs_local_filename(data_url);
  ## Check if any data_urls are not ABS data URLs
  if (!grepl("^https*:\\/\\/www\\.abs\\.gov\\.au\\/ausstats.+",
             data_url, ignore.case=TRUE))	
    stop(sprintf("Invalid ABS url: %s", data_url));
  ## Check if any data_urls are not accessible
  ## if (any(sapply(data_urls, http_error)))
  ##   stop(sprintf("One or more url(s) not accessible: %s",
  ##                paste(data_urls[sapply(data_urls, http_error)], collapse=", ")));
  
  ## -- Download files --
  cat(sprintf("Downloading: %s", local_filename));
  resp <- GET(data_url, write_disk(file.path(exdir, local_filename), overwrite=TRUE),
              raustats_ua(), progress());
  ## File download validation code based on:
  ##  https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
  if (http_error(resp)) {
    stop(
      sprintf(
        "ABS catalogue file request failed (Error code: %s)\nInvalid URL: %s", 
        status_code(resp),
        data_url
      ),
      call. = FALSE
    )
  }
  ## Check content-type is compliant
  if (!http_type(resp) %in% c("application/x-zip", "application/vnd.ms-excel")) {
    stop("ABS file request did not return Excel or Zip file", call. = FALSE)
  }
  ## Return results
  return(file.path(exdir, local_filename));
}


## @name abs_local_filename
## @title Create local file names for storing downloaded ABS data files
## @description Function to create local filename from web-based file name
## @param url Character vector specifying one or more ABS data URLs.
## @return Returns a local file names (character vector) in which downloaded files will be saved.
## @author David Mitchell <david.pk.mitchell@@gmail.com>
##
abs_local_filename <- function(url)
{
  sprintf("%s_%s.%s",
          sub("^.+&(.+)\\.(zip|xlsx*)&.+$", "\\1", url),
          sub("^.+(\\d{2}).(\\d{2}).(\\d{4}).+$", "\\3\\2\\1", url),
          sub("^.+&(.+)\\.(zip|xlsx*)&.+$", "\\2", url));
}


#' @name abs_cat_unzip
#' @title Uncompress locally-stored ABS Catalogue data file archives
#' @description Function to uncompress locally-stored ABS Catalogue data file archives
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
#' @title Read ABS time series data file(s)
#' @description This function extracts time series data from ABS data files.
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom stats complete.cases
#' @param files Names of one or more ABS data files
#' @param type  One of either 'tss'  -- ABS Time Series  Spreadsheet (the Default) or  'css' -- Data
#'   Cube.R
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## Read specified ABS Excel time series files
#'     tables <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
#'     downloaded_tables <- abs_cat_download(tables$path_2[1], exdir=tempdir())
#'     extracted_files <- abs_cat_unzip(downloaded_tables)
#'     x <- abs_read_tss(extracted_files);
#'   }
abs_read_tss <- function(files, type="tss") {
  x <- lapply(files,
              function(file)
                abs_read_tss_(file, type=type));
  z <- do.call(rbind, x);
  rownames(z) <- seq_len(nrow(z));
  return(z);
}


### Function: abs_read_tss_
## @name abs_read_tss
## @title Read ABS time series data file(s)
## @description This function extracts time series data from ABS data files.
abs_read_tss_ <- function(file, type="tss") {
  if (FALSE) {
    file <- "/tmp/340101.xls"
  }
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
  data <- data[complete.cases(data),];
  names(data) <- tolower(names(data));
  return(data);
}
