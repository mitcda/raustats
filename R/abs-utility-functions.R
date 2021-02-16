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


#' @name chunk_absstat_query
#' @title Split long ABS Stat query into smaller chunks
#' @description This function checks if a draft ABS.Stat query call exceeds the string length limit
#'   or would return more than one million cells and if so, splits the query into smaller chunks for
#'   calling the ABS.Stat site. This function is called by the \link{\code{abs_stat}} function, it
#'   need not be called directly.
#' @param url An ABS Stat JSON SDMX query string
#' @return string or list
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS.Stat helper functions
chunk_absstat_query <- function(url)
{
  if (FALSE) {
    url <- "http://stat.data.abs.gov.au/SDMX-JSON/data/CPI/1+2+3+4+5.50.10001.10.Q/all?detail=Full&dimensionAtObservation=AllDimensions"
  }
  if (nchar(url) > 1000) {
    
    
  }

  sel <- sub(sprintf("^%s/%s/.+/(.+)/.+$",
                     abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path),
             "\\1", url);
  split_sel <- unlist(strsplit(sel, "\\."))
  split_sel <- lapply(split_sel,
                      function(x) unlist(strsplit(x, "\\+")));
  split_max_len <- 
  
  ## -- UP TO HeRE --

  ds_filter <- abs_search(pattern=c("Estimated Resident Population", "Males|Females|Persons",
                                    "^\\d{1,3}$", "Jun-2017"),
                          dataset=abs_ds, code_only=TRUE) %>%
    map(~ .x %>% split(., ceiling(seq_along(.)/26))) %>%
    cross;
  

  return(z)
}


