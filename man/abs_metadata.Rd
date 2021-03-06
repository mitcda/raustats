% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abs-api-functions.R
\name{abs_metadata}
\alias{abs_metadata}
\title{Download dataset metadata from the ABS API}
\usage{
abs_metadata(id, lang = "en")
}
\arguments{
\item{id}{ABS dataset ID.}

\item{lang}{Preferred language (default 'en' - English).}
}
\value{
data frame in long format
}
\description{
This function queries and returns all metadata associated with a specified dataset
  from ABS.Stat.
}
\examples{
  \donttest{
    datasets <- abs_datasets();
    x <- abs_metadata("CPI");
    x <- abs_metadata(grep("cpi", datasets$id, ignore.case=TRUE, value=TRUE));
    names(x)
    y <- abs_metadata(datasets$id[1]);
    names(y)
  }
}
\author{
David Mitchell <david.pk.mitchell@gmail.com>
}
