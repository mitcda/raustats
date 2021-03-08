
<!-- README.md is generated from README.Rmd. Please edit that file -->

# raustats

The `raustats` package provides functions that facilitate downloading of
data and statistics from the Australian Bureau of Statistics (ABS) and
Reserve Bank of Australia (RBA) websites

## Installation

You can install the released version of raustats from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("raustats")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mitcda/raustats")
```

## Quick-start guide

This section provides a quick-start guide to downloading ABS and RBA
statistics using `raustats`.

First, load the library:

``` r
library(raustats)
```

## Downloading ABS Catalogue Statistics

The `abs_cat_stats` function downloads ABS statistics either by series
title or by the previously used ABS Catalogue number. For example, the
latest Consumer Price Index (CPI) data spreadsheets (previous ABS
Catalogue no. 6401.0) can be downloaded with either of the following
calls:

``` r
cpi_all <- abs_cat_stats("Consumer Price Index, Australia");
cpi_all <- abs_cat_stats(cat_no="6401.0")
```

The `tables` argument can be used to supply a regular expression to
download a subset of all available tables. For example, the following
call downloads only Table 1:

``` r
cpi <- abs_cat_stats("Consumer Price Index, Australia", tables="Table.+1\\D")
```

Alternatively, `tables` also accepts a regular expression that matches
the table name as specified on the ABS data access page, as follows:

``` r
cpi <- abs_cat_stats("Consumer Price Index, Australia",
                     tables="CPI: All Groups, Index Numbers and Percentage Change")
```

Statistics from previous releases can be accessed using the `releases`
argument. Further description of the `abs_cat_stats` function and other
functions available in `raustats` is provided in vignette: *Introduction
to `raustats`*.

## Downloading ABS data via [ABS.Stat](http://stat.data.abs.gov.au/)

The `abs_stats` function downloads ABS statistics from
[ABS.Stat](http://stat.data.abs.gov.au/). For example, to download the
latest CPI data series for ‘All Groups’ change in prices across
Australia and each of the eight capital cities:

``` r
cpi_api <- abs_stats("CPI", filter=list(MEASURE=1, REGION=c(1:8,50),
                                        INDEX=10001, TSEST=10, FREQUENCY="Q"))
```

The package includes additional functions to search for datasets and
data series available on [ABS.Stat](http://stat.data.abs.gov.au/), which
are documented in the accompanying vignette: *Introduction to
`raustats`*.

## Downloading RBA data

The `rba_stats` function downloads statistics available on the RBA
website. For example, the latest statistics covering the RBA’s assets
and liabilities (RBA Statistical Table A1) can be downloaded with the
following call:

``` r
rba_balsheet <- rba_stats("A1")
```

A more detailed explanation of `rba_stats` and other RBA data access
functions is provided in the introductory vignette: *Introduction to
`raustats`*.

<!-- -------------------------------- EOF ---------------------------------- -->
