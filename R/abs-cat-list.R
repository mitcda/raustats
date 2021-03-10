#' @name abs_cat_list
#' @title Returns a list of all ABS statistical releases
#' @description
#'   `r lifecycle::badge("experimental")`
#'   This function traverses the ABS website and get the name and location of the latest
#'   ABS statistical releases.
#' @importFrom dplyr left_join
#' @importFrom rvest html_attr html_nodes html_session
#' @return NULL
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @family ABS catalogue functions
#' @keywords internal
#' @examples
#'   \donttest{
#'     abs_cat_list();
#'   }
abs_cat_list <- function() {
  if (!exists("abs_cat_list", envir=.raustats_cache, inherits=FALSE)) {
    message("Scanning all ABS statistical collections (called on first use each session).")
    url <- file.path(abs_urls()$base_url,
                     abs_urls()$statistics_path);
    ## Check for HTTP errors
    raustats_check_url_available(url);
    ## Open html session
    suppressWarnings(s <- html_session(url));
    ## Return search results
    ## -- Statistical groups
    stat_group_nodes <- html_nodes(s, xpath = ".//div[contains(@class, 'block-views')]");
    stat_group_names <- html_text(html_nodes(stat_group_nodes, "h2"));
    ## -- Statistical collections
    stat_coll_nodes <- lapply(stat_group_nodes,
                              function(x)
                                html_nodes(x, xpath = ".//a[contains(@class, 'field-group-link')]"));
    stat_coll_tbl <- mapply(function(x, y) {
      z <- data.frame(stat_group = y,
                      stat_view = trimws(html_text(html_nodes(x,"h3"))),
                      stat_view_path = html_attr(x, "href"),
                      stringsAsFactors=FALSE);
      return(z)},
      stat_coll_nodes, stat_group_names,
      SIMPLIFY=FALSE);
    stat_coll_tbl <- do.call(rbind, stat_coll_tbl);
    ## -- Data sets
    stat_topic_tbl <-
      lapply(stat_coll_tbl$stat_view_path,
             function(x) {
               url <- file.path(abs_urls()$base_url, x)
               raustats_check_url_available(url);
               stat_topic_nodes <-
                 html_nodes(html_session(url),
                            xpath = "//div[contains(@class, 'abs-topic')]");
               stat_topic_names <- trimws(html_text(html_nodes(stat_topic_nodes, "h2")));
               stat_topic_paths <- html_attr(html_nodes(stat_topic_nodes, "a"), "href");
               return(
                 if (length(stat_topic_nodes) == 0) {
                   NULL
                 } else {
                   data.frame(stat_view_path = x,
                              stat_title = stat_topic_names,
                              stat_title_path = stat_topic_paths,
                              stringsAsFactors=FALSE)
                 })
             });
    stat_topic_tbl <- do.call(rbind, stat_topic_tbl[!sapply(stat_topic_tbl, is.null)]);
    ## Combine group, collection and topic names and path in one table, and return
    z <- left_join(stat_topic_tbl,
                   stat_coll_tbl,
                   by="stat_view_path");
    ## Save results to 'abs_cat_list' variable
    assign("abs_cat_list",
          z[, c("stat_group", "stat_view", "stat_view_path", "stat_title", "stat_title_path")],
           envir=.raustats_cache);
  }
};

## ---------------------------------- EOF ----------------------------------- ##
