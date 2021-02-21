### ABS catalogue utility functions

#' @name is_abs_tss
#' @title Check if file is a valid ABS time series spreadsheet
#' @description This function checks if the specified  returns a list of URLs and data paths used to construct ABS Catalogue
#'   data access calls. It is used in other functions in this package and need not be called
#'   directly.
#' @importFrom readxl read_excel excel_sheets
#' @param file An ABS data filename
#' @return logical
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS catalogue helper functions
is_abs_tss <- function(file)
{
  if (!grepl("\\.xlsx*", file, ignore.case=TRUE))
    stop(sprintf("File: %s is not an Excel spreadsheet.", basename(file)));
  ## Get sheet names
  sheet_names <- tolower(excel_sheets(file));
  ## Check if 'file' contains valid ABS time series spreadsheet sheet names
  if (!all(c("index", "data1")  %in% sheet_names)) {
    ## stop(sprintf("File: %s is not a valid ABS time series file.", basename(file)));
    return(FALSE)
  } else {
    return(TRUE)
  }
}


##' @name split_absstat_query
##' @title Split long ABS Stat query into several smaller chunks
##' @description This function checks if a draft ABS.Stat query call exceeds the string length limit
##'   or would return more than one million cells and if so, splits the query into smaller chunks for
##'   calling the ABS.Stat site. This function is called by the \link{\code{abs_stat}} function, it
##'   need not be called directly.
##' @importFrom purrr cross
##' @param url An ABS Stat JSON SDMX query string
##' @return string or list
##' @author David Mitchell <david.pk.mitchell@@gmail.com>
##' @keywords internal
##' @family ABS.Stat helper functions
split_absstat_query <- function(url)
{
  if (FALSE) {
    url <- "http://stat.data.abs.gov.au/SDMX-JSON/data/CPI/1+2+3+4+5.50.10001.10.Q/all?detail=Full&dimensionAtObservation=AllDimensions"

    url <- "http://stat.data.abs.gov.au/SDMX-JSON/data/ERP_QUARTERLY/1.0+1+2+3+4+5+6+7+8.1+3+2.0+1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100.Q/all?detail=Full&dimensionAtObservation=AllDimensions&startPeriod=2017-Q2&endPeriod=2017-Q2"

    ## Create test URL
    url <- paste0("http://stat.data.abs.gov.au/SDMX-JSON/data/ERP_QUARTERLY/",
                  paste(1:5, collapse="+"), ".",
                  paste(50, collapse="+"), ".",
                  paste(10001:10350, collapse="+"), ".",
                  paste(10, collapse="+"), ".",
                  paste("Q", collapse="+"),
                  "/",
                  "all",
                  "?detail=Full",
                  "&dimensionAtObservation=AllDimensions",
                  "&startPeriod=2017-Q2",
                  "&endPeriod=2017-Q2")
  }
  if (nchar(url) > 1000) {
    ## Extract data filter terms
    regexp_absstat_url <- ;
    filter_text <- sub(sprintf("^(%s/%s/.+/)(.+)(/.+)$",
                               abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
                       "\\2", url);
    url_head <- sub(sprintf("^(%s/%s/.+/)(.+)(/.+)$",
                            abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
                    "\\1", url);
    url_tail <- sub(sprintf("^(%s/%s/.+/)(.+)(/.+)$",
                            abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
                    "\\3", url);
    ## Total filter string length
    len_filter_text <- nchar(filter_text);
    avail_filter_text <- 1000 - round(nchar(url) - len_filter_text, -1);
    ## Split filter into different components
    filter_list <- lapply(unlist(strsplit(filter_text, "\\.")),
                          function(x) {
                            z <- unlist(strsplit(x, "\\+"))
                          });
    ## Derive component string lengths
    len_filter_list <- lapply(filter_list, function(x) sum(nchar(x)+1))
    maxlen_filter_list <- lapply(filter_list, function(x) max(nchar(x)+1))
    n_filter_list <- lapply(filter_list, length)
    ## Find longest 
    i <- which.max(len_filter_list)
    n_groups <- floor((avail_filter_text - sum(unlist(len_filter_list[-i]))) /
                      maxlen_filter_list[[i]]);
    ## Split filter Shrink the number of 
    split_filter <- lapply(filter_list,
                           function(x) split(x, ceiling(seq_along(x) / n_groups)));
    split_filter <- cross(split_filter);
    ## Re-create URL filter strings
    split_url <- lapply(split_filter,
                        function(x)
                          paste(lapply(x,
                                       function(y) paste(y, collapse="+")),
                                collapse="."));

    new_split_url <- paste0(url_head,
                            lapply(split_filter,
                                   function(x)
                                     paste(lapply(x,
                                                  function(y) paste(y, collapse="+")),
                                           collapse=".")),
                            url_tail);
    return(z)
  }
}

