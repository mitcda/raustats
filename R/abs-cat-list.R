#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}:
#' @name abs_cat_list
#' @title Returns a list of all ABS statistical releases
#' @description This function traverses the ABS website and get the name and location of the latest
#'   ABS statistical releases.
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows left_join select
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
    stat_group_nodes <- s %>% html_nodes(xpath = ".//div[contains(@class, 'block-views')]");
    stat_group_names <- stat_group_nodes %>% html_nodes("h2") %>% html_text;
    ## -- Statistical collections
    stat_coll_nodes <- stat_group_nodes %>%
      lapply(function(x)
        html_nodes(x, xpath = ".//a[contains(@class, 'field-group-link')]"));
    stat_coll_tbl <- mapply(function(x, y) {
      z <- list(stat_group = y,
                stat_view = x %>% html_nodes("h3") %>%
                  html_text %>% trimws,
                stat_view_path = x %>% html_attr("href")) %>%
        as.data.frame},
      stat_coll_nodes, stat_group_names,
      SIMPLIFY=FALSE) %>%
      bind_rows;
    ## -- Data sets
    stat_topic_tbl <-
      lapply(stat_coll_tbl$stat_view_path,
             function(x) {
               url <- file.path(abs_urls()$base_url, x)
               raustats_check_url_available(url);
               stat_topic_nodes <- url %>% html_session %>%
                 html_nodes(xpath = "//div[contains(@class, 'abs-topic')]");
               stat_topic_names <- stat_topic_nodes %>%
                 html_nodes("h2") %>% html_text %>% trimws;
               stat_topic_paths <- stat_topic_nodes %>%
                 html_nodes("a") %>% html_attr("href");
               return(
                 if (length(stat_topic_nodes) == 0) {
                   NULL
                 } else {
                   data.frame(stat_view_path = x,
                              stat_topic = stat_topic_names,
                              stat_topic_path = stat_topic_paths)
                 })
             }) %>%
      bind_rows;
    ## Combine group, collection and topic names and path in one table, and return
    assign("abs_cat_list",
           stat_topic_tbl %>%
           left_join(stat_coll_tbl,
                     by="stat_view_path") %>%
           select(stat_group, stat_view, stat_view_path, stat_topic, stat_topic_path),
           envir=.raustats_cache);
  }
};

## ---------------------------------- EOF ----------------------------------- ##
