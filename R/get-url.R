## =========================================================================
## Filename:     get-url.R
## Created:      August 2018
## Updated:      <2018-09-04 21:20:21 david at grover>
## Author:       David Mitchell (david.mitchell@infrastructure.gov.au)
## Description: 
##
## =========================================================================

#' @name create_url
#' @title Create search url
#' @description
#' @importFrom rvest html_session follow_link html_attr
#' @importFrom xml2 read_xml
#' @importFrom urltools url_parse url_compose
create_url <- function(base_url, catno, files)
{
  url_compose
}




#' @name get_data
#' @title Return data files from a specified url 
#' @description
#' @importFrom magrittr %>% inset
#' @importFrom rvest html_session follow_link html_attr
#' @importFrom xml2 read_xml
#' @importFrom urltools url_parse url_compose
#' @importFrom utils unzip download.file
#' @param url 
#' @export
get_data <- function(url)
{
DEBUG <- FALSE
if (DEBUG) {
  base_url <- "http://www.abs.gov.au/ausstats/abs@.nsf/mf";
  catno <- "3101.0";
  url <- file.path(base_url, catno); ##  (ABS Cat no. 3101.0)
}
  file_regex <- paste0(sub("\\.","",catno), ".+.zip", ".+Time.+Series.+Spreadsheet");
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

## =============================== EOF =====================================
