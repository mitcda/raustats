#' @name abs_cat_releases
#' @title Return ABS catalogue table releases
#' @description Return list of all releases available for specified ABS catalogue number.
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom httr http_error
#' @param title `r lifecycle::badge("experimental")`
#'   ABS publication title.
#' @param cat_no ABS catalogue numbers.
#' @param include_urls Include full path URL to specified ABS catalogue releases. Default (FALSE)
#'   does not include release URLs.
#' @return Returns a data frame listing available ABS catalogue releases.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS catalogue functions
#' @examples
#'   \donttest{
#'     ## List all available quarterly National Accounts tables
#'     ana_releases <-
#'       abs_cat_releases("Australian National Accounts: National Income, Expenditure and Product")
#'     ana_releases <- abs_cat_releases(cat_no="5206.0", include_urls=TRUE);
#'     
#'     ## List latest available CPI Time Series Spreadsheet tables only
#'     cpi_releases <- abs_cat_releases("Consumer Price Index, Australia", include_urls=TRUE);
#'     cpi_release_urls <- abs_cat_releases(cat_no="6401.0");
#'   }
abs_cat_releases <- function(title, cat_no, include_urls=FALSE)
{
  ## -- DEBUGGING CODE --
  ## if (FALSE) {
  ##   title <- "Wage Price Index, Australia"
  ##   cat_no <- "6345.0"
  ##   include_urls <- FALSE
  ##      cat_releases <- abs_cat_releases(cat_no="6291.0.55.001")
  ##  cat_releases <- abs_cat_releases(cat_no="1270.0.55.001", include_urls=TRUE)
  ## }
  if (missing(title) && missing(cat_no))
    stop("One of either title or cat_no needs to be supplied.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  ## Create ABS URL and open session
  ## Get ABS publication url
  if (!missing(title)) {
    ## New-style ABS URL
    title_path <- abs_cat_series(pattern=title, level="title", include_urls=TRUE)$title_path
    url <- valid_url(file.path(abs_urls()$base_url, title_path));
  } else {
    ## Old-style ABS URL
    url <- valid_url(file.path(abs_urls()$base_url, abs_urls()$ausstats_path,
                               abs_urls()$mf_path, cat_no));
  }
  ## Check for HTTP errors
  raustats_check_url_available(url)
  ## if (http_error(url))
  ##   stop(sprintf("File cannot be downloaded. Check URL: %s", url))
  suppressWarnings(s <- html_session(url));
  ## Get path to 'Previous Releases' page
  previous_url <- unique(
    html_attr(html_nodes(s, xpath = sprintf("//a[@class='%s']",
                                            abs_expressions()$xpath_release_links_class)),
              "href"));
  ## Check previous_url exists
  raustats_check_url_available(previous_url)
  suppressWarnings(s <- html_session(previous_url));
  ## Alt option
  all_releases <- 
    html_nodes(s, xpath =
                    sprintf("//div[@class='%s']",
                            abs_expressions()$xpath_release_views_row))
   all_releases <- all_releases[!grepl("Release\\s*date", all_releases, ignore.case=TRUE)]; # Remove future releases
  ## Links
  z <- data.frame(
    title = html_text(all_releases),
    type = ifelse(grepl(abs_expressions()$regex_release_type, html_text(all_releases), ignore.case=TRUE),
                  sub(abs_expressions()$regex_release_type, "\\1", html_text(all_releases), ignore.case=TRUE),
                  "Previous release"),
    reference_period = sub(sprintf(".+(%s).*", 
                                   paste(paste(c(month.name, month.abb), "\\d{4}", sep="\\s*"),
                                         collapse="|")),
                           "\\1",
                           html_text(all_releases),
                           ignore.case=TRUE),
    stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
  ## Add URLs
  if (include_urls) {
    z <- transform(z,
                   url =  html_attr(html_nodes(all_releases, "a"), "href"),
                   stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
    ## Prepend ABS base URL on Latest|Previous releases
    idx_new_release <- grepl("^(Latest|Previous)", z$type, ignore.case=TRUE);
    z[idx_new_release, "url"] <-
      sprintf("%s%s", abs_urls()$base_url, z[idx_new_release, "url"]);
    ## Replace all whitespace in ABS URLs
    z$url <- gsub("\\s", "%20", z$url)
  }
  ## Return results
  row.names(z) <- seq_len(nrow(z));
  return(z)
}

## ----------------------------------- EOF ---------------------------------- ##
