#' @name abs_cat_tables
#' @title Return list of tables for specified ABS publication
#' @description Return list of data tables available for specified ABS data series.
#' @importFrom dplyr case_when bind_rows bind_cols left_join right_join case_when mutate across
#' @importFrom rvest html_session html_text html_nodes html_attr follow_link
#' @importFrom httr http_error
#' @param title ABS publication title.
#' @param cat_no ABS catalogue numbers.
#' @param releases Date or character string object specifying the month and year denoting which
#'   release to download. Default is "Latest", which downloads the latest available data. See
#'   examples for further details.
#' @param types ABS publication types to return. Permissable options include one or more of: 'tss'
#'   -- ABS Time Series Spreadsheets, 'css' -- ABS Data Cubes and 'pub' -- ABS Publications. The
#'   default returns all Time Series Spreadsheets and Data Cubes.
#' @param include_urls Include full URLs to returned ABS data files. Default (FALSE) does not
#'   include data file URLs.
#' @return Returns a data frame listing the data collection tables and URLs for Excel (column:
#'   \code{path_xls}) and, if available, Zip (column: \code{path_zip}) files.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## List latest available quarterly National Accounts tables
#'     ana_tables <- abs_cat_tables(cat_no="5206.0", releases="Latest");
#'     ana_tables_url <- abs_cat_tables(cat_no="5206.0", releases="Latest", include_urls=TRUE);
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
abs_cat_tables <- function(title, cat_no, releases="Latest", types=c("tss", "css"),
                           include_urls=FALSE)
{
  if (FALSE) {
    title <- "Wage Price Index, Australia"
    cat_no <- "6345.0";
    releases <- "Latest";
    types <- "tss";
  }
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
      abs_releases <- abs_cat_releases(cat_no=cat_no, include_urls=TRUE);
  }
  ## Get URLs for all specified releases
  release_urls <-
    file.path(abs_urls()$base_url,
              abs_releases[c(unlist(sapply(releases,
                                           function(x)
                                             grep(x, abs_releases$type, ignore.case=TRUE))),
                             unlist(sapply(releases,
                                           function(x)
                                             grep(x, abs_releases$release, ignore.case=TRUE)))),
                           "url"]);
  release_urls <- unique(release_urls);
  ## Get list of tables for all releases specified
  w <- lapply(release_urls,
              function(x) {
                if (grepl(file.path(abs_urls()$base_url, abs_urls()$ausstats_path),
                          x, ignore.case=TRUE)) {
                  ## Return table information from old style page
                  z <- bind_cols(abs_get_old_site_tables(x),
                                 release_url = x)
                } else {
                  ## Return table information from new style page
                  z <- bind_cols(abs_get_new_site_tables(x),
                                 release_url = x)
                }
              }) %>%
    bind_rows;
  ## Combine ABS releases and release table data frames
  z <- right_join(abs_releases %>%
                  mutate(across(url, ~ case_when(!grepl("^https.+", .x, ignore.case=TRUE) ~
                                                   file.path(abs_urls()$base_url, .x),
                                                 TRUE ~ .x))),
                  w,
                  by=c("url"="release_url"));
    
  ## Check for HTTP errors
  ## raustats_check_url_available(url);
  ## ## Open html session
  ## suppressWarnings(s <- html_session(url));
  ## if (length(releases) == 1 && tolower(releases) == "latest") {
  ##   .paths <- "";
  ## } else {
  ##   ## Get path to 'Past & Future Releases' page
  ##   .paths <- html_nodes(s, "a");
  ##   .paths <- .paths[grepl(abs_urls()$releases_regex, .paths)];
  ##   .paths <- html_attr(.paths, "href");
  ##   s <- jump_to(s, .paths)
  ##   .paths <- html_nodes(s, "a");
  ##   .paths <- .paths[grepl(paste(releases, collapse="|"), .paths, ignore.case=TRUE)];
  ##   .paths <- html_attr(.paths, "href");
  ## }
  ## ## Return list of all downloadable files, for specified catalogue tables ('cat_tables')
  ## v <- lapply(.paths,
  ##             function(x) {
  ##               ## Check for HTTP errors
  ##               y <- jump_to(s, x)
  ##               l <- follow_link(y, abs_urls()$downloads_regex)
  ##               ht <- html_nodes(html_nodes(l, "table"), "table")
  ##               ## Return data table
  ##               ## The ABS data catalogue lists the data inside a HTML table within a table, i.e.
  ##               ##  <table>
  ##               ##    <table> </table>
  ##               ##  </table>
  ##               ## The following nested apply functions, exploits this structure to extract the
  ##               ## list of available publication types and associated links.
  ##               all_nodes <- lapply(sapply(ht, function(x) html_nodes(x, "tr")),
  ##                                   function(x)
  ##                                     c(html_text(html_nodes(x, "td")),
  ##                                       ## html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")));
  ##                                       paste0(abs_urls()$base_url,
  ##                                              html_attr(html_nodes(html_nodes(x, "td"), "a"), "href")))
  ##                                   );
  ##               ## Remove ABS data download section heading from all_nodes
  ##               ##   Where ABS data download section titles that include links, are included
  ##               ##   in the node set, but are not conformant with publication information.
  ##               ##   The following code block, removes these entries.
  ##               data_nodes <- lapply(all_nodes,
  ##                                    function(x) {
  ##                                      if (grepl(paste(c("(^\\W{0,1}$)",
  ##                                                        "(^publications\\W*$)",
  ##                                                        "(^data\\s*cubes\\W*$)",
  ##                                                        "(^time\\s*series\\s*spreadsheets?\\W*$)"),
  ##                                                      collapse="|"),
  ##                                                x[1], ignore.case=TRUE)) {
  ##                                        NULL
  ##                                        ## Also drop SuperTABLE (srd) format files
  ##                                      } else if (any(grepl("\\.srd", x, ignore.case=TRUE))) {
  ##                                        NULL
  ##                                      } else {
  ##                                        x
  ##                                      }
  ##                                    })
  ##               data_nodes <- data_nodes[-which(sapply(data_nodes, is.null))];
  ##               ## Tidy and return data set names and urls
  ##               nodes <- data_nodes[unlist(lapply(data_nodes,
  ##                                                 function(x) {
  ##                                                   any(
  ##                                                     grepl(sprintf("(%s)",
  ##                                                                   paste(types, collapse="|")),
  ##                                                           x, ignore.case=TRUE) &
  ##                                                     grepl("ausstats", x, ignore.case=TRUE))
  ##                                                 }))];
  ##               ## Remove non-breaking spaces (&nbsp;), and blank entries
  ##               nodes <- lapply(nodes,
  ##                               function(x) {
  ##                                 z <- trimws(gsub("\u00a0", "", x));      ## Remove non-breaking spaces
  ##                                 z <- replace(z, z == "", NA_character_); ## Replace blank objects with NA
  ##                                 ## Set entries not starting with 'https*' with 'NA_character_'
  ##                                 z[-1] <- replace(z[-1],                          
  ##                                                  !grepl("^https*.+", z[-1], ignore.case=TRUE),
  ##                                                  NA_character_);
  ##                                 ## Set entries containing 'INotes' with 'NA_character_'
  ##                                 z <- replace(z,                          
  ##                                              grepl("INotes", z, ignore.case=TRUE),
  ##                                              NA_character_);
  ##                                 z <- z[!is.na(z)];                       ## Remove NA objects
  ##                                 ## Set object names: First element = 'item_name'
  ##                                 names(z)[1] <- "item_name";
  ##                                 names(z)[-1] <- case_when(
  ##                                   ## !grepl("(^https*|^Releases|INotes)", z, ignore.case=TRUE) ~ "item_name",
  ##                                   grepl("\\.xlsx*", z[-1], ignore.case=TRUE) ~ "path_xls",
  ##                                   grepl("\\.zip", z[-1], ignore.case=TRUE) ~ "path_zip",
  ##                                   grepl("\\.pdf", z[-1], ignore.case=TRUE) ~ "path_pdf",
  ##                                   TRUE ~ NA_character_)
  ##                                 z <- as.data.frame(t(cbind.data.frame(z, deparse.level=1)),
  ##                                                    stringsAsFactors=FALSE);
  ##                                 return(z);
  ##                               });
  ##               ## Tidy nodes into data.frame (using dplyr::bind_rows)
  ##               dt <- suppressWarnings(bind_rows(nodes)) 
 ##               ## Lastly replace spaces in all URL paths with '%20' string
  ##               for(name in grep("^path_", names(dt), ignore.case=TRUE, value=TRUE)) # names(dt)[-1]
  ##                 dt[,name] <- gsub("\\s+", "%20", dt[,name]);
  ##               return(dt);
  ##             });
  ## ## Add catalogue number and release information to table
  ## v <- lapply(seq_along(v),
  ##             function(i) {
  ##               v[[i]]$release <- sub("^$", "Latest", releases[i]);
  ##               v[[i]]$cat_no <- cat_no;
  ##               as.data.frame(v)
  ##             });
  ## ## Bind all results together
  ## z <- do.call(rbind, v);
  ## If rbind breaks on different row names try:
  ## z <- do.call(function(...) rbind(..., make.row.names=FALSE), v);
  ## names(z) <- c("item_name", ..., "cat_no", "release");
  ##
  ## Return results
  if (!include_urls)
    z <- z[,!grepl("url$", names(z), ignore.case=TRUE)]
  ## row.names(z) <- seq_len(nrow(z));
  return(z)
}

## ----------------------------------- EOF ---------------------------------- ##
