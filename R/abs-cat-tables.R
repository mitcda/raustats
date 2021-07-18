#' @name abs_cat_tables
#' @title Return list of tables for specified ABS publication
#' @description Return list of data tables available for specified ABS data series.
#' @importFrom dplyr case_when bind_rows bind_cols left_join right_join case_when mutate across
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom httr http_error
#' @param title `r lifecycle::badge("experimental")`
#'   ABS publication title.
#' @param cat_no ABS catalogue numbers.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param types `r lifecycle::badge("deprecated")`
#'   ABS publication types to return. Permissable options include one or more of: 'tss'
#'   -- ABS Time Series Spreadsheets, 'css' -- ABS Data Cubes and 'pub' -- ABS Publications. The
#'   default returns all Time Series Spreadsheets and Data Cubes.
#' @param include_urls Include full URLs to returned ABS data files. Default (FALSE) does not
#'   include data file URLs.
#' @return Returns a data frame listing the data collection tables and URLs.
#' @family ABS catalogue functions
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## List latest available quarterly National Accounts tables
#'     ana_tables <-
#'       abs_cat_tables("Australian National Accounts: National Income, Expenditure and Product",
#'                      releases="Latest");
#'     ana_tables_url <- abs_cat_tables(cat_no="5206.0", releases="Latest", include_urls=TRUE);
#'
#'     ## List latest available CPI Time Series Spreadsheet tables only
#'     cpi_tables <- abs_cat_tables(title="Consumer Price Index, Australia",
#'                                  releases="Latest", include_urls=TRUE);
#'     cpi_tables_url <- abs_cat_tables(cat_no="6401.0", releases="Latest");
#'   
#'     ## List latest available ASGS Volume 3 Data Cubes
#'     ## asgs_vol3_tables <- abs_cat_tables(cat_no="1270.0.55.003", releases="Latest", types="css");
#'     ## asgs_vol3_tables_url <- abs_cat_tables(cat_no="1270.0.55.003", releases="Latest",
#'     ##                                        types="css", include_urls=TRUE);
#'   
## #'     ## List latest available ASGS ANZSIC publications (PDF) files
## #'     anzsic_2006 <- abs_cat_tables(cat_no="1292.0", releases="Latest",
## #'                                   types="pub", include_urls=TRUE);
#'   }
abs_cat_tables <- function(title, cat_no, releases="Latest",
                           types=c("tss", "css"), include_urls=FALSE)
{
  ## if (FALSE) {
  ##   title <- "Wage Price Index, Australia";  cat_no <- "6345.0";
  ##   title <- "Australian National Accounts: National Income, Expenditure and Product";
  ##   cat_no <- "5206.0"
  ##   releases <- "Latest";
  ##   releases <- "Sep 2017";
  ##   cat_table <- abs_cat_tables(title="Wage Price Index, Australia", releases="Latest",
  ##                               types=c("tss"), include_urls=FALSE);
  ##   ## Test results
  ##   cat_table <-
  ##     abs_cat_tables(title="Australian National Accounts: National Income, Expenditure and Product",
  ##                    releases="Latest", types="tss", include_urls=FALSE);
  ##   cat_no <- "6291.0.55.001"
  ##   cat_table <- abs_cat_tables(cat_no="6291.0.55.001")
  ##   # asgs_files <- abs_cat_tables(cat_no="1270.0.55.001", include_urls=TRUE);
  ## }
  if (missing(title) && missing(cat_no))
    stop("One of either title or cat_no needs to be supplied.");
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
  ## Get ABS publication urls
  if (!missing(title)) {
    ## New-style ABS URL
    abs_releases <- abs_cat_releases(title=title, include_urls=TRUE);
  } else {
    ## Old-style ABS URL
    ## cat_no="6401.0"; tables="CPI.+All Groups"; releases="Dec 2017"; types="tss";
    abs_releases <- abs_cat_releases(cat_no=cat_no, include_urls=TRUE);
  }
  ## Get URLs for all specified releases
  release_urls <-
    file.path(
      abs_releases[c(unlist(sapply(releases,
                                   function(x)
                                       grep(x, abs_releases$type, ignore.case=TRUE))),
                     unlist(sapply(releases,
                                   function(x)
                                       grep(x, abs_releases$reference_period,
                                            ignore.case=TRUE)))),
                   "url"]);
  release_urls <- unique(release_urls);
  ## Get list of tables for all releases specified
  w <- lapply(release_urls,
              function(x) {
                if (grepl(file.path(abs_urls()$base_url, abs_urls()$ausstats_path),
                          x, ignore.case=TRUE)) {
                  ## Return table information from old style page
                  z <- bind_cols(abs_get_old_site_tables(x),
                                 url = x) # release_url
                } else {
                  ## Return table information from new style page
                  z <- bind_cols(abs_get_new_site_tables(x),
                                 url = x) # release_url
                }
              }) %>%
    bind_rows;
  ## Combine ABS releases and release table data frames
  z <- right_join(abs_releases %>%
                    mutate(across(url,
                                  ~ case_when(!grepl("^https.+", .x, ignore.case=TRUE) ~
                                                  file.path(abs_urls()$base_url, .x),
                                              TRUE ~ .x))),
                  w,
                  by=c("url")); # ="release_url"
  z <- z[!duplicated(z$table_name),];
  ## Return results
  if (!include_urls)
    z <- z[,!grepl("url$", names(z), ignore.case=TRUE)]
  ## row.names(z) <- seq_len(nrow(z));
  ## Add 'cat_table' class name
  class(z) <- append(class(z), "cat_table");
  return(z)
}

## ----------------------------------- EOF ---------------------------------- ##
