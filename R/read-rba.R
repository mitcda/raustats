
urlRBA <- "http://www.rba.gov.au/statistics/tables/";


### Function: searchRBA
#' @name searchRBA
#' @title Return list of data tables from RBA website
#'
#' @import rvest
#' @import urltools
#' @import magrittr
#' @import readxl
#' @export
#' @param files Names of one or more ABS data file
#' @param type One of 'ts' - time series or 'dem' - demographic data
#' @return data frame in long format
#' @examples
#' File <- file.path(DataDir, "5206001_Key_Aggregates.xls");
#' ABS.5206001 <- read_abs(File);
#' ABS.5206001 %>% as.data.frame %>% head;
#' @export
readRBA <- function(files, type="ts")

searchRBA <- function(url) {
  if (DEBUG) {
    url <- urlRBA <- "http://www.rba.gov.au/statistics/tables/";
    library(rvest);
    library(urltools);
    library(magrittr);
    library(readxl);
  }
  .Path <- url %>%
    html_session %>%
    follow_link("Downloads") %>%
    read_html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
    grep(File.regex, ., ignore.case=TRUE, value=TRUE);

  ## -- UP TO HERE --
    html_nodes(xpath = "//div[contains(@class,'title')]") %>%
      html_nodes("a", href matches "*.xls|csv")
  
  ## Get list of tables
  .Path <- url %>%
    read_html %>%
    html_nodes(xpath = "//div[contains(@id,'tables-list')]") %>%
    html_nodes("a") %>%
    html_text
    #html_nodes;# (xpath = "/a")

  %>%
        `[`(grep("tables-list", .))

  html_children()

  gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  


  .Paths <- url %>%
  read_html %>%
  html_nodes("div") %>%
  ## html_attr("id") %>%
  html_children()
  
  grep(File.regex, ., ignore.case=TRUE, value=TRUE);
 
  ## Get URL
  .URL <- sapply(.Path,
                 function(x) ABS.URL %>%
                             url_parse %>%
                             inset("path", value=x) %>%
                             url_compose,
                 USE.NAMES=FALSE);


}


getData <- function(url)
{
  ABS.BaseURL <- "http://www.abs.gov.au/ausstats/abs@.nsf/mf";
  CatNo <- "3101.0";
  ABS.URL <- file.path(ABS.BaseURL, CatNo); ##  (ABS Cat no. 3101.0)
  # Import all tables
  #  - Table 1: GDP
  #  - Table 2: Expenditure on GDP
  File.regex <- paste0(sub("\\.","",CatNo), ".+.zip", ".+Time.+Series.+Spreadsheet");
  ## Return file paths
  .Path <- ABS.URL %>%
    html_session %>%
    follow_link("Downloads") %>%
    read_html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
    grep(File.regex, ., ignore.case=TRUE, value=TRUE);
  ## Get URL
  .URL <- sapply(.Path,
                 function(x) ABS.URL %>%
                             url_parse %>%
                             inset("path", value=x) %>%
                             url_compose,
                 USE.NAMES=FALSE);
  File.regex       <- paste0(sub("\\.","",CatNo), ".+.zip");
  .Destfiles <- .URL %>% basename %>% sub(paste0(".*(",File.regex,").*"),"\\1",.) %>% file.path(tempdir(), .)
  for (i in seq_along(.URL))
    download.file(.URL[i], .Destfiles[i], mode="wb");
  .Files <- sapply(.Destfiles, function(x) unzip(x, list=TRUE)$Name);
  for (i in seq_along(.Destfiles))
    unzip(.Destfiles[i], exdir=tempdir());
}






### Function: readRBA
#' @name readRBA
#' @title Import RBA time series data files
#'
#' 
#' @import magrittr
#' @import readxl
#' @export
#' @param files Names of one or more ABS data file
#' @param type One of 'ts' - time series or 'dem' - demographic data
#' @return data frame in long format
#' @examples
#' File <- file.path(DataDir, "5206001_Key_Aggregates.xls");
#' ABS.5206001 <- read_abs(File);
#' ABS.5206001 %>% as.data.frame %>% head;
#' @export
readRBA <- function(files, type="ts")
{
  x <- lapply(files,
              function(file)
                .readABS(file, type=type)
              ) %>%
    do.call(rbind, .)
}


.readRBA <- function(file, type="ts")
{
  require(magrittr);
  require(readxl)
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
  Data <- lapply(grep("data", .SheetNames, ignore.case=T, value=T),
                 function(SheetName) {
                   z <- read_excel(file, SheetName);
                   ## Return pre-header information from ABS files 
                   HeaderRow <- sapply(1:nrow(z),
                                       function(i)
                                         grepl("series\\s*id", paste(z[i,], collapse=" "),
                                               ignore.case=TRUE)) %>% which
                   z                                   %<>%
                     set_names(z[HeaderRow,]      %>%
                               gsub("\\.","",.)   %>%
                               gsub("\\s","_",.))       %>%  ## Rename variables
                     rename(Date = Series_ID)           %>%  ## Name Date field
                     .[-(1:HeaderRow),]                 %>%  ## Drop header rows
                     .[,!is.na(names(.))]               %>%  ## Drop empty columns
                     gather(Series_ID, Value, -Date, convert=TRUE) %>%
                     mutate(Date = Date %>% as.integer %>% as.Date.excel,
                            Value = Value %>% as.numeric);
                   return(z);
                 })                                              %>%
    do.call(rbind, .);
  Data %<>% left_join(Metadata, by=c("Series_ID"="Series_ID"))
}
