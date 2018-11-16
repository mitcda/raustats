
---
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
```


# raustats: An R package for searching and downloading data from the ABS & RBA.

You can install:

The latest release version from CRAN with
```{r, eval = FALSE}
install.packages("wbstats")
```

or

The latest development version from github with
```{r, eval = FALSE}
devtools::install_github("GIST-ORNL/wbstats")
```


## Introduction

#' raustats: An R package for accessing data and statistics from the ABS and RBA websites
#'
#' The raustats package provides structured access to all.
#'
#' To learn more about the raustats package, start with the vignettes:
#' \code{browseVignettes(package = "raustats")}

The [Australian Bureau of Statistics (ABS)](http://www.abs.gov.au/) is
Australia’s national statistical agency, providing trusted official statistics
on a wide range of economic, social, population and environmental matters of
importance to Australia. Key ABS statistical collections include:
 
  * Australian National Accounts
  * International Trade
  * Consumer Price Index (CPI)
  * Labour Force
  * Population trends


The [Reserve Bank of Australia (RBA)](http://www.rba.gov.au/) is Australia's
central bank. In addition to its legislative responsibilities, it collects and
publishes statistics on money, credit, the Australian banking systems and other
relevant economic metrics. Key RBA statistics include:

  * Banking system assets and liabilities
  * Money and credit statistics
  * Household and business finances
  * Interest rates
  * Exchange rates
  * Inflation and inflation expectations.


The ABS and RBA make their statistics primarily available through Excel and/or
CSV spreadsheets. 

This package provides functions to search and download data and statistics from
the [Australian Bureau of Statistics (ABS)](http://www.abs.gov.au/) and [Reserve
Bank of Australia (RBA)](http://www.rba.gov.au/) websites, as well as draft
access to the ABS.Stat - Beta data catalogue API.


