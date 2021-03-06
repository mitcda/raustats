% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abs-api-functions.R
\name{abs_datasets}
\alias{abs_datasets}
\title{Download ABS.Stat datasets}
\usage{
abs_datasets(lang = "en", include_notes = FALSE)
}
\arguments{
\item{lang}{Preferred language (default 'en' - English).}

\item{include_notes}{Include ABS annotation information for each series.}
}
\value{
data frame in long format
}
\description{
This function returns a list of all datasets available from ABS.Stat.
}
\examples{
  \donttest{
    datasets <- abs_datasets()
    datasets <- abs_datasets(include_notes=TRUE)
  }
}
\author{
David Mitchell <david.pk.mitchell@gmail.com>
}
