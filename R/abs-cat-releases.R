#' @name abs_cat_releases
#' @title Return ABS catalogue table releases
#' @description Return list of all releases available for specified ABS catalogue number.
#' @importFrom magrittr %>%
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom httr http_error
#' @param title ABS publication title.
#' @param cat_no ABS catalogue numbers.
#' @param include_urls Include full path URL to specified ABS catalogue releases. Default (FALSE)
#'   does not include release URLs.
#' @return Returns a data frame listing available ABS catalogue releases.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS catalogue functions
#' #' @examples
#'   \donttest{
#'     ## List all available quarterly National Accounts tables
#'     ana_releases <- abs_cat_releases("5206.0");
#'     ana_release_urls <- abs_cat_releases("5206.0", include_urls=TRUE);
#'   
#'     ## List latest available CPI Time Series Spreadsheet tables only
#'     cpi_releases <- abs_cat_releases("6401.0");
#'     cpi_release_urls <- abs_cat_releases("6401.0", include_urls=TRUE);
#'   }
abs_cat_releases <- function(title, cat_no, include_urls=FALSE)
{
  ## -- DEBUGGING CODE --
  if (FALSE) {
    title <- "Wage Price Index, Australia"
    cat_no <- "6345.0"
    include_urls <- FALSE
  }
  if (missing(title) && missing(cat_no))
    stop("One of either title or cat_no needs to be supplied.");
  if (!is.logical(include_urls))
    stop("include_urls must be either TRUE or FALSE");
  ## Create ABS URL and open session
  ## Get ABS publication url
  if (!missing(title)) {
    ## New-style ABS URL
    topic_path <- abs_cat_select(pattern=title, level="topic", include_urls=TRUE)$topic_path
    url <- file.path(abs_urls()$base_url, topic_path);
  } else {
    ## Old-style ABS URL
    url <- file.path(abs_urls()$base_url, abs_urls()$ausstats_path,
                     abs_urls()$mf_path, cat_no);
  }
  ## Check for HTTP errors
  raustats_check_url_available(url)
  ## if (http_error(url))
  ##   stop(sprintf("File cannot be downloaded. Check URL: %s", url))
  suppressWarnings(s <- html_session(url));
  ## Get path to 'Previous Releases' page
  previous_url <- s %>%
    html_nodes(xpath = sprintf("//a[@class='%s']",
                               abs_expressions()$xpath_release_links_class)) %>%
    html_attr("href") %>% unique;
  ## Check previous_urls exists
  raustats_check_url_available(previous_urls)
  suppressWarnings(s <- html_session(previous_url));
  ## Alt option
  all_releases <- s %>%
    html_nodes(xpath =
                 sprintf("//div[@class='%s']",
                         abs_expressions()$xpath_release_views_row)) %>%
    .[!grepl("Release\\s*date", ., ignore.case=TRUE)]; # Remove future releases
  ## Links
  z <- list(
    title = all_releases %>% html_text,
    type = all_releases %>% html_text %>%
      { ifelse(grepl(abs_expressions()$regex_release_type, ., ignore.case=TRUE),
               sub(abs_expressions()$regex_release_type, "\\1", ., ignore.case=TRUE),
               "Previous release") },
    release = all_releases %>%
      html_text %>%
      { sub(sprintf(".+(%s).*", 
                    paste(paste(c(month.name, month.abb), "\\d{4}", sep="\\s*"), collapse="|")),
            "\\1", ., ignore.case=TRUE) }
  ) %>%
    as.data.frame;
  ## Add URLs
  if (include_urls) {
    z <- transform(z,
                   url = all_releases %>% html_nodes("a") %>% html_attr("href"))
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
