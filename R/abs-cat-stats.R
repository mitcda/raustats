#' @name abs_cat_stats
#' @title Get ABS catalogue series data
#' @description This function downloads ABS catalogue series statistics, by ABS catalogue number.
#' @importFrom rvest html_session follow_link html_attr jump_to
#' @importFrom xml2 read_xml read_html
#' @param title `r lifecycle::badge("experimental")`
#'   Character vector specifying one or more ABS publication titles.
#' @param cat_no Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param tables A character vector of regular expressions denoting tables to download. The default
#'   ('All') downloads all time series spreadsheet tables for each specified catalogue. Use a list
#'   to specify different table sets for each specified ABS catalogue number.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param types
#'   `r lifecycle::badge("deprecated")`
#'   One of either 'tss' -- ABS time series spreadsheet (the default) or 'css' -- ABS
#'   data cube (cross-section spreadsheet).
#' @param na.rm logical (default: \code{TRUE}) - remove observations containing missing values.
#' @return data frame in long format
#' @export
#' @family ABS catalogue functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## Download quarterly Australian National Accounts, Tables 1 & 2 
#'     xx <- abs_cat_series(pattern="national.*income.*expenditure.*product");
#'     ana_q <- abs_cat_stats("5206.0", tables=c("Table 1\\W+", "Table 2\\W+"));
#'     ana_q <- abs_cat_stats("5206.0", tables=c("Table 1\\W+", "Table 2\\W+"));
#'
#'     ## Download December 2017 Australian National Accounts, Table 1
#'     ana_q_2017q4 <- abs_cat_stats("5206.0", tables="Table 1\\W+", release="Dec 2017");
#'   }
abs_cat_stats <- function(title, cat_no, tables="All", releases="Latest",
                          types="tss", na.rm=TRUE)
{
  ## if (FALSE) {
  ##   title <- "Wage Price Index, Australia";
  ##   cat_no <- "6345.0";
  ##   tables <- c("Table 1", "Table 2");
  ##   xx <- abs_cat_stats(title=title, tables=tables);
  ##   title <- abs_cat_series(pattern="national.*income.*expenditure.*product")$title;
  ##   tables <- c("Table 1\\W+", "Table 2\\W+");
  ##   cat_no <- "5206.0"; tables <- c("Table 1\\W+", "Table 2\\W+"); releases <- "Dec 2018";
  ##   releases <- "Latest"; types <- "tss"; include_urls <- FALSE; na.rm=TRUE;
  ##   xx <- abs_cat_stats(title, tables=tables);
  ## }
  if (missing(title) && missing(cat_no))
    stop("One of either title or cat_no needs to be supplied.");
  ## if (tolower(releases) != "latest" ||
  ##     releases IS NOT A DATE )
  ##   stop("releases arguments ")
    ## Get ABS publication urls
  if (!missing(title)) {
    ## Get available catalogue tables
    cat_tables <- abs_cat_tables(title=title, releases=releases,
                                 include_urls=TRUE); # types=types,
  } else {
    ## Old-style ABS URL
    cat_tables <- abs_cat_tables(cat_no=cat_no, releases=releases,
                                 include_urls=TRUE); # types=types,
  }
  # if (any(!types %in% c("tss","css")))
  #  stop("Allowable type arguments limited to one or both: 'tss' and 'css'.");
  ## Select only the user specified tables ('sel_tables')
  if (length(tables) == 1 && tolower(tables) == "all") {
    ## If 'all' tables, download all
    sel_tables <- if (any(grepl("^all time series.*", cat_tables$table_name, ignore.case=TRUE))) {
                    ## If all tables provided as single compressed archive, select that
                    cat_tables[grepl("^all time series.*", cat_tables$table_name, ignore.case=TRUE),]
                  } else {
                    ## Else, select all tables
                    cat_tables
                  };
  } else {
    ## Else, return only selected tables
    sel_tables <- cat_tables[grepl(sprintf("(%s)", paste(tables, collapse="|")),
                                   cat_tables$table_name, ignore.case=TRUE),];
    ## Stop if regular expression does not return any tables
    if (nrow(sel_tables) == 0)
      stop(paste("Specified table regular expressions do not match any table names, re-specify."))
    ## Select distinct tables
    sel_tables <- sel_tables[!duplicated(sel_tables$table_name),];
  }
  ## Select only the user specified tables ('sel_tables')
  sel_urls <- sel_tables$file_url;
  ## -- DEPRECATED --
  ## apply(sel_tables, 1,
  ##                   function(y) {
  ##                     ## If zip in path_zip, select zip file, else select xls(x) file
  ##                     if (any(grepl("\\.zip", y, ignore.case=TRUE))) {
  ##                       unique(grep("\\.zip", unlist(y), ignore.case=TRUE, value=TRUE))
  ##                     } else {
  ##                       unique(grep("\\.xlsx*", unlist(y), ignore.case=TRUE, value=TRUE))
  ##                     }
  ##                   });
  ## -- END DEPRECATED --
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

## ----------------------------------- EOF ---------------------------------- ##
