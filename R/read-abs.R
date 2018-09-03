abs_domain <- function() "http://www.abs.gov.au/";
abs_ausstats_path <- function() "ausstats/abs@.nsf/mf";
abs_ausstats_url <- function() paste0(abs_domain(), abs_ausstats_path());
abs_downloads_link <- function() "Downloads";
abs_releases_link <- function() "Past.*Future.*Releases";

#' @name download_abs_data
#' @title Return data files from a specified url
#' @description
#' @importFrom magrittr %>% inset
#' @importFrom rvest html_session follow_link html_link html_attr
#' @importFrom xml2 read_xml read_html
#' @importFrom urltools url_parse url_compose
#' @importFrom utils unzip download.file
#' @param series Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param tables A character vector of regular expressions denoting tables to download. If NULL
#'   (Default), the function downloads all time series spreadsheet tables for each specified
#'   catalogue. Use a list to specify different table sets for each specified ABS catalogue number.
#' @param release Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param type One of either 'tss' - time series spreadsheet (DEFALT or 'dem' - demographic data
#' @param ... other arguments to
#' @return data frame in long format
#' @export
#' @examples
#'    x <- get_abs_data("3101.0");
#'    y <- get_abs_data("5206.0", tables=c("Table 1"));
#' 
#'    x <- get_abs_data("5206.0", tables="Table 1", release="Dec 2017");
download_abs_data <- function(series, tables=NULL, release="Latest", type="tss")
{
  DEBUG <- FALSE
  if (DEBUG) {
    library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
    series <- "5206.0";
    tables <- c("Table 1", "Table 2")
    release <- "Latest"
    release <- c("Mar 2017", "Dec 2016", "Sep 2016", "Jun 2016", "Mar 2016");
  }
  ## Create 
  url <- file.path(abs_ausstats_url(), series);

  file_regex <- paste0(sub("\\.","",catno), ".+.zip", ".+Time.+Series.+Spreadsheet");

  if (tolower(release) != "latest") {
    ## Get path to 'Past & Future Releases' page
    .abs_pfr_url <- url %>%
      html_session %>%
      html_nodes("a") %>%
      .[grepl("Past.+Future.+Releases", .)] %>%
      html_attr("href");

    ## Test
    s <- url %>% html_session %>% jump_to(., url=.abs_pfr_url);

    .abs_release_urls <- s %>%
      html_nodes("a") %>%
      .[grepl(sprintf("(%s)", paste(sub("\\s+","\\\\s*", release), collapse="|")), ., ignore.case=TRUE)] %>%
      ## if (DEBUG) html_text;
      html_attr("href");

    ## Return all table nodes
    ## -- Test code ---
    test_abs_release_urls <- s %>% jump_to(., url=.abs_release_urls[[1]]) %>%
      follow_link("Downloads") %>%
      html_nodes("table") %>% html_nodes("table");

    if (DEBUG)
      test_abs_release_urls %>% html_nodes("tr") %>% length
    
    test_abs_release_urls %>% lengthclass;
    .tmp <- test_abs_release_urls %>% sapply(.,
                                             function(x) 
                                               x %>% html_nodes("tr") %>%
                                               sapply(.,
                                                      function(x)
                                                      x %>% html_nodes("td")));

    ## Example 0
    .tmp <- test_abs_release_urls %>% sapply(.,
                                             function(x) 
                                               x %>% html_nodes("tr") %>%
                                               sapply(.,
                                                      function(x)
                                                        x %>% html_nodes("td") %>%
                                                        html_text),##  %>%
                                                        ## magrittr::set_names(paste0("x", seq_len(.)))), ## %>%
                                             simplify=FALSE) %>% bind_rows;

    .tmp <- test_abs_release_urls %>% sapply(.,
                                             function(x) 
                                               x %>% html_nodes("tr") %>%
                                               sapply(.,
                                                      function(x)
                                                        x %>% html_nodes("td") %>% html_nodes("a") %>%
                                                        html_attr("href")), simplify=FALSE)

    
    ## Example 1
    .tmp <- test_abs_release_urls %>% sapply(.,
                                             function(x) 
                                               x %>% html_nodes("tr") %>%
                                               sapply(.,
                                                      function(x)
                                                        x %>% html_nodes("td")) %>%
                                               purrr::map_df(~bind_cols(data_frame(table = html_text(.x) %>%
                                                                                     trimws)),
                                                             html_nodes(.x, "a") %>%
                                                             html_attr(.x, "href")));

    ## Example 2
    .tmp <- test_abs_release_urls %>% sapply(.,
                                             function(x) 
                                               x %>% html_nodes("tr") %>%
                                               purrr::map_df(~bind_cols(data_frame(table = .x %>% read_html),
                                                                        html_nodes(.x, "td"),
                                                                        html_nodes(., "a") %>%
                                                                        html_attr(., "href"))));

    
    abs_release_urls <- sapply(.abs_release_urls,
                               function(x) s %>%
                                           jump_to(., url=x) %>%
                                           follow_link("Downloads") %>%
                                           html_nodes("table")
                               );
                               
    abs_release_urls <- sapply(.abs_release_urls,
                               function(x) s %>%
                                           jump_to(., url=x) %>%
                                           follow_link("Downloads") %>%
                                           html_nodes("table") %>%## read_html %>%
                                           html_nodes("td") %>%
                                           html_attr("align"))
html_table(fill=TRUE),
                               simplify=FALSE);
  
  ## https://stackoverflow.com/questions/47065161/how-to-scrape-using-rvest-in-pages-with-multiple-tables
  ## https://stackoverflow.com/questions/45756026/how-to-scrape-multiple-tables-that-are-without-ids-or-class-using-r
  ## https://stackoverflow.com/questions/43657427/rvest-scraping-multiple-tables-with-preceding-titles
  ## https://stackoverflow.com/questions/40140133/scraping-tables-on-multiple-web-pages-with-rvest-in-r
  ## http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
  
  ## -- UP TO HERE --
  abs_release_urls <- sapply(.abs_release_urls,
                             function(x) s %>%
                                         jump_to(., url=x) %>%
                                         follow_link("Downloads") %>%
                                         read_html %>%
                                         html_nodes("tr") %>%## read_html %>%html %>% 
                                         purrr::map_df(~dplyr::bind_cols(tibble::data_frame(table = html_text(.x) %>%
                                                                                              trimws)))),
                                                                         html_nodes(.x, "td") %>%
                                                                         html_nodes(.x, "a") %>% ##
                                                                         html_attr(.x, "href") %>%
                             set_names(paste0("x", seq_along(.))) %>%
                             purrr::invoke(tibble::data_frame, .))) %>%
                             readr::type_convert(),
                             simplify=FALSE);

  
    html_nodes('tr') %>% 
    map_df(~bind_cols(data_frame(bgcolor = html_attr(.x, 'bgcolor')),    # grab attribute
                      # extract each row's values to 1-row data.frame
                      html_nodes(.x, 'td') %>% 
                          html_text(trim = TRUE) %>% 
                          set_names(paste0('x', seq_along(.))) %>%    # or `%>% t() %>% as_data_frame()`
                          invoke(data_frame, .))) %>% 
    type_convert()   


  
  ## Example
html <- '<tr BGCOLOR = "#F8C0E0">
<td> BASOPHILS <td> microl     <td> 0.477 <td> 0.425 <td align="center"> 0.052 <td align="center"> 1.920 <td align="center">    51.5 <td align="center">    32
</tr>
<tr BGCOLOR = "#F8F0B0">
<td> CALCIUM <td > mg/dl        <td>  12.2 <td>   1.7 <td align="center">   7.6 <td align="center">  14.9 <td align="center">    71 <td align="center">    33
</tr>'
  
parsed_df <- html %>% 
    read_html() %>% 
    html_nodes('tr') %>% 
    map_df(~bind_cols(data_frame(bgcolor = html_attr(.x, 'bgcolor')),    # grab attribute
                      # extract each row's values to 1-row data.frame
                      html_nodes(.x, 'td') %>% 
                          html_text(trim = TRUE) %>% 
                          set_names(paste0('x', seq_along(.))) %>%    # or `%>% t() %>% as_data_frame()`
                          invoke(data_frame, .))) %>% 
    type_convert()   

  
    html_table(fill=TRUE)
    
    names(abs_release_urls)
    abs_release_urls[[1]] %>% html_attr("href")
    grep("Table 1", abs_release_urls[[1]] %>% html_text, value=TRUE)
    
    ## Create 'Past & Future Releases' URL
    abs_pfr_url <- abs_domain() %>%
      url_parse %>%
      inset("path", value=.abs_pfr_url) %>%
      url_compose;

    ## Return specified past/future release paths
    .abs_release_urls <- abs_pfr_url %>%
      html_session %>%
      html_nodes("a") %>%
      .[grepl(sprintf("(%s)", paste(sub("\\s+","\\\\s*", release), collapse="|")), ., ignore.case=TRUE)] %>%
      ## if (DEBUG) html_text;
      html_attr("href");
    ## Create vector of past/future release paths
    abs_release_urls <- sapply(.abs_release_urls,
                               function(x) abs_ausstats_url() %>% sub("mf$", x, .));


    
    %>%
      url_parse %>%
      inset("path", value=.abs_pfr_url) %>%
      url_compose;
    
read_html %
      ## html_nodes(xpath="//a//span[starts_with(., 'Past')]");
      ## grep("Past.+Future.+Releases", .Path, ignore.case=TRUE, value=TRUE)
      ## -- html_nodes(., xpath="//a[text()[starts_with(., 'Past')]]");
      html_node(., xpath="//span[starts_with(., 'Past')]");

    " and ends_with(., 'Releases')]]") %>%

  follow_link(xpath="//a[text()[starts_with(., 'Past') and ends_with(., 'Releases')]]") %>%
    ## rvest::follow_link("Downloads") %>%
      read_html %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
      grep(file_regex, ., ignore.case=TRUE, value=TRUE);
  

  }

  ## Return file paths
  .Path <- url %>%
    rvest::html_session %>%
    rvest::follow_link("Downloads") %>%
    xml2::read_html %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
    grep(file_regex, ., ignore.case=TRUE, value=TRUE);
  ## Get URL
  .URL <- sapply(.Path,
                 function(x) url %>%
                             url_parse %>%
                             inset("path", value=x) %>%
                             url_compose,
                 USE.NAMES=FALSE);
  file_regex <- paste0(sub("\\.","",CatNo), ".+.zip");
  .Destfiles <- .URL %>% basename %>%
    sub(paste0(".*(",file_regex,").*"),"\\1",.) %>%
    file.path(tempdir(), .)
  for (i in seq_along(.URL))
    utils::download.file(.URL[i], .Destfiles[i], mode="wb");
  .Files <- sapply(.Destfiles, function(x) utils::unzip(x, list=TRUE)$Name);
  for (i in seq_along(.Destfiles))
   utils::unzip(.Destfiles[i], exdir=tempdir());
}


#' @name get_abs_cat_tables
#' @title Return ABS catalogue tables
#' @description Return list of tables from specified ABS catalogue number
#' @importFrom magrittr %>% inset
#' @importFrom rvest html_session follow_link html_link html_attr
#' @importFrom xml2 read_xml read_html
#' @importFrom urltools url_parse url_compose
#' @param url Valid ABS data collection URL.
#' @return returns a data frame listing the data collection tables and links.
#' @export
#' @examples
#'    url <-
#'    tables <- get_abs_cat_tables(url);
#'    y <- get_abs_data("5206.0", tables=c("Table 1"));
#' 
#'    x <- get_abs_data("5206.0", tables="Table 1", release="Dec 2017");
get_abs_cat_tables <- function(url)
{
  DEBUG <- FALSE
  if (DEBUG) {
    library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
    series <- "5206.0";
    tables <- c("Table 1", "Table 2")
    release <- "Latest"
    release <- c("Mar 2017", "Dec 2016", "Sep 2016", "Jun 2016", "Mar 2016");

    abs_domain <- function() "http://www.abs.gov.au/";
    abs_ausstats_path <- function() "ausstats/abs@.nsf/mf";
    abs_ausstats_url <- function() paste0(abs_domain(), abs_ausstats_path());
    abs_downloads_link <- function() "Downloads";
    abs_releases_regex <- function() "Past.*Future.*Releases";

    ## Get path to 'Past & Future Releases' page
    .abs_pfr_url <- file.path(abs_ausstats_url(), series) %>%
      html_session %>%
      html_nodes("a") %>%
      .[grepl(abs_releases_regex(), .)] %>%
      html_attr("href");

    ## Test
    url <- file.path(abs_ausstats_url(), series) %>%
      html_session %>%
      jump_to(., url=.abs_pfr_url);

    ## Get release pages - for inclusion in other function
    if (FALSE)
      url <- s %>%
        html_nodes("a") %>%
        .[grepl(sprintf("(%s)", paste(sub("\\s+","\\\\s*", release), collapse="|")), ., ignore.case=TRUE)] %>%
        html_attr("href");
    
  }
  ## Test URL is valid and Downloads page accessible
  if (!abs_downloads_link() %in%
      (url$url %>% html_session %>%
       html_nodes("a") %>%
       html_text))
    stop(sprintf("URL: %s is not a valid ABS catalogue link.",
                 url$url));

  ## Return data table
  ## The ABS data catalogue lists the data inside a HTML table within a table, i.e.
  ##  <table>
  ##    <table> </table>
  ##  </table>
  ## The following code exploits this structure to extract the list of available tables
  ## and associated links.
  z <- url %>%
    follow_link("Downloads") %>%
    html_nodes("table") %>% html_nodes("table") %>%
    sapply(.,
           function(x) 
             x %>% html_nodes("tr") %>%
             sapply(.,
                    function(x)
                      c(x %>% html_nodes("td") %>% html_text,
                        x %>% html_nodes("td") %>% html_nodes("a") %>% html_attr("href"))
                    ),
           simplify=FALSE) %>%
    unlist(recursive = FALSE) %>%
    do.call(rbind, .) %>%
    as_data_frame %>%
    set_names(paste0("x", seq_along(.))) %>%
    filter(grepl("^Table|All Time Series", x1, ignore.case=TRUE)) %>%
    replace(., . == "", NA_character_) %>%
    select_if(function(x) any(!is.na(x))) %>%
    set_names("table_name", "path1", "path2");
  
  return(z);
}





#' @name read_abs
#' @title Read ABS time series data file(s)
#' @description This function extracts time series data from ABS data files. 
#' @importFrom readxl read_excel excel_sheets
#' @importFrom magrittr %>% set_names
#' @importFrom dplyr mutate mutate_at select rename join left_join right_join
#' @importFrom tidyr gather
#' @importFrom rvest
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
#' @importFrom dplyr mutate mutate_at select rename join left_join right_join
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
