% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abs-cat-functions.R
\name{abs_cat_unzip}
\alias{abs_cat_unzip}
\title{Uncompress locally-stored ABS Catalogue data file archives}
\usage{
abs_cat_unzip(files, exdir)
}
\arguments{
\item{files}{One or more local zip files.}

\item{exdir}{Target directory for extracted archive files. Directory is created if it doesn't
exist. If missing, creates a new subdirectory in \code{tempdir()} using the respective zip
files (specified in \code{files}.}
}
\value{
Returns a character vector listing the names of all files extracted.
}
\description{
Function to uncompress locally-stored ABS Catalogue data file archives.
}
\author{
David Mitchell <david.pk.mitchell@gmail.com>
}
