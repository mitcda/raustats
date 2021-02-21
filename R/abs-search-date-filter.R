### ABS Webiste search functions
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}:
#' @name abs_search_date_filter
#' @title Create date filter for ABS search function
#' @description This function creates a valid date range filter to insert into ABS search URL.
#' @importFrom lubridate %m-% %m+% days years 
#' @param filter Date range filter -- one of: 'Today', 'Past
#'   week', 'Past fortnight', 'Past month', 'Past 3 months', 'Past 6 months', 'Past year', or single
#'   calendar year (e.g. '2021' or '2020').
#' @return a string containg formatted ABS search date filter
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS search helper functions
abs_search_date_filter <- function(filter) {
  ## DEBUGGING LINES
  if (FALSE) {
    filter <- "Today"
    filter <- "2021"
  }
  curr_date <- Sys.Date();
  filter_dates <- c('Today', 'Past week', 'Past fortnight', 'Past month',
                    'Past 3 months', 'Past 6 months', 'Past year');
  filter_date <- match.arg(tolower(filter),
                           c(tolower(filter_dates),
                             as.character(as.integer(format(curr_date, "%Y")):1901)))
  filter_url <-
    paste0("f.Date|",
           switch(filter_date,
                  ## 'Today'' format: f.Date|d=d%3D25Jan2021+%3A%3A+Today
                  today = sprintf("d=%s+::+Today", format(curr_date, '%d%b%Y')),
                  ## 'Past week' format: f.Date|d=d>17Jan2021<26Jan2021+%3A%3A+Past+week
                  `past week` = sprintf("d=d>%s<%s+::+Past+week",
                                        format(curr_date %m-% days(8), '%d%b%Y'),
                                        format(curr_date %m+% days(1), '%d%b%Y')),
                  ## 'Past fortnight' format: f.Date|d=d>10Jan2021<26Jan2021+%3A%3A+Past+fortnight
                  `past fortnight` = sprintf("d=d>%s<%s+::+Past+fortnight",
                                             format(curr_date %m-% days(15), '%d%b%Y'),
                                             format(curr_date %m+% days(1), '%d%b%Y')),
                  ## 'Past month' format: f.Date|d=d>24Dec2020<26Jan2021+%3A%3A+Past+month
                  `past month` = sprintf("d=d>%s<%s+::+Past+month",
                                         format(curr_date %m-% months(1) %m-% days(1), '%d%b%Y'),
                                         format(curr_date %m+% days(1), '%d%b%Y')),
                  ## 'Past 3 months' format: f.Date|d=d>24Oct2020<26Jan2021+%3A%3A+Past+3+months
                  `past 3 months` = sprintf("d=d>%s<%s+::+Past+3+months",
                                            format(curr_date %m-% months(3) %m-% days(1), '%d%b%Y'),
                                            format(curr_date %m+% days(1), '%d%b%Y')),
                  ## 'Past 6 months' format: f.Date|d=d>24Jul2020<26Jan2021+%3A%3A+Past+6+months
                  `past 6 months` = sprintf("d=d>%s<%s+::+Past+6+months",
                                            format(curr_date %m-% months(6) %m-% days(1), '%d%b%Y'),
                                            format(curr_date %m+% days(1), '%d%b%Y')),
                  ## 'Past year' format: f.Date|d=d>24Jan2020<26Jan2021+%3A%3A+Past+year
                  `past year` = sprintf("d=d>%s<%s+::+Past+year",
                                        format(curr_date %m-% years(1) %m-% days(1), '%d%b%Y'),
                                        format(curr_date %m+% days(1), '%d%b%Y')),
                  ## Otherwise: 'Single year' format: f.Date|d=d=2021+%3A%3A+2021'
                  sprintf("d=d=%s+::+%s", filter_date, filter_date))
           );
  return(filter_url);
}

## ---------------------------------- EOF ----------------------------------- ##
