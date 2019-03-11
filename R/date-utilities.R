### Function: excel2Date
#' @name excel2Date
#' @title Convert Excel numeric date to R Date object
#' @description Function to convert Excel numeric date to R Date object
#' @param x Excel-based date numeric object
#' @return Date object
## #' @examples
## #'   \donttest{
## #'     raustats:::excel2Date(43445);
## #'   }
#' @keywords internal
excel2Date <- function(x) {
  as.Date(x, origin="1899-12-30");
}

### Function: quarter2Date
#' @name quarter2Date
#' @title Convert dates formatted as year-quarter to dates objects
#' @description Function to convert dates formatted as year-quarter to date-format objects
#' @param x Year-quarter date format
#' @param base.month Specifies base month for first quarter. Can be a scalar: 1,2,3 or character
#'   object: Jan, Feb, Mar.
#' @param format The input date format. Default is "\%Y-Q\%q".
#' @return This function returns a Date format object.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
## #' @examples
## #'   \donttest{
## #'     x <- c("1960-Q1","1960-Q2","1960-Q3","1960-Q4","1961-Q1","1961-Q2");
## #'     quarter2Date(x);
## #'     quarter2Date(x, base.month="Jan");
## #'   }
#' @keywords internal
quarter2Date <- function(x, base.month="Mar", format="%Y-Q%q")
{
  ## Check format
  if (!grepl("%Y", format) & !grepl("%q", format))
    stop("Format should contain year (%Y) and quarter (%q) regular expressions.")
  format  <- sub("(%q)", "(\\\\d)",
                 sub("(%Y)", "(\\\\d{4})", format));
  Year <- as.integer(sub(format,"\\1", x));
  Qtr <- as.integer(sub(format,"\\2", x));
  ## Re-encode month
  Mth <- if (base.month == 1 | base.month == "Jan") {
           Qtr * 3 - 2;
         } else if (base.month == 2 | base.month == "Feb") {
           Qtr * 3 - 1;
         } else if (base.month == 3 | base.month == "Mar") {
           Qtr * 3;
         } else {
           stop(paste("base.month should be either a scalar = 1,2 or 3",
                      "or a character object = \"Jan\", \"Feb\" or \"Mar\"."));
         }
  z <- as.Date(paste(Year, month.abb[Mth], "01", sep="-"), format="%Y-%b-%d");
  return(z);
}


### Function: last_day
#' @name last_day
#' @title Set Date object to the last day of the month
#' @description Function to change the date of a Date object to the last day of the month
#' @importFrom lubridate ceiling_date days
#' @param date date object
#' @return Date object
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
## #' @examples
## #'   \donttest{
## #'     date <- seq.Date(as.Date("2005-06-01"), length=36, by="month");
## #'    last_day(date)
## #'   }
#' @keywords internal
last_day <- function(date)
  ceiling_date(date, "month") - days(1);


### Function: fin_year
#' @name fin_year
#' @title Create financial year date object
#' @description Function to create a financial year date object
#' @param date date object
#' @param ending character string abbreviation or number denoting ending month of the financial year
#' @return Date object 
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
## #' @examples
## #'   \donttest{
## #'     x <- seq.Date(as.Date("2005-06-01"), length=36, by="month");
## #'    fin_year(x)
## #'   }
#' @keywords internal
fin_year <- function(date, ending="Jun")
{
  if (is.character(ending)) {
    if (!substr(ending,1,3) %in% month.abb)
      stop(sprintf("Invalid month supplied to ending: %s", ending))
    ending <- match(ending, month.abb);
  } else {
    if (!ending %in% 1:12)
      stop(sprintf("Invalid month supplied: %d - should be in 1:12", ending));
  }

  Year <- as.integer(format(date, "%Y"));
  Month <- as.integer(format(date, "%m"));
  Year <- ifelse(Month > ending, Year + 1, Year);
  z <- as.Date(paste(Year, month.abb[ending], "01", sep="-"), format="%Y-%b-%d");
  return(z);
}
