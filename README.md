
<!-- README.md is generated from README.Rmd. Please edit that file -->

# raustats: An R package for accessing ABS and RBA statistics

An R package for downloading Australian economic statistics from the
Australian Bureau of Statistics (ABS) and Reserve Bank of Australia
(RBA) websites.

## Installation

You can install the released version of raustats from
[CRAN](https://cran.r-project.org) with:

``` r
install.packages("raustats")
```

or the latest development version from github with:

``` r
devtools::install_github("mitcda/raustats")
```

## How to use raustats

To learn more about the raustats package, start with the vignettes:

``` r
browseVignettes(package = "raustats")
```

## Introduction

The [Australian Bureau of Statistics (ABS)](http://www.abs.gov.au/) is
Australia’s national statistical agency, providing trusted official statistics
on a wide range of economic, social, population and environmental matters of
importance to Australia. Key ABS statistical collections include:

  - Australian National Accounts
  - International Trade
  - Consumer Price Index (CPI)
  - Labour Force
  - Population trends

The [Reserve Bank of Australia (RBA)](https://www.rba.gov.au/) is Australia’s
central bank. In addition to its legislative responsibilities, it collects and
publishes statistics on money, credit, the Australian banking systems and other
relevant economic metrics. Key RBA statistics include:

  - Banking system assets and liabilities
  - Money and credit statistics
  - Household and business finances
  - Interest rates
  - Exchange rates
  - Inflation and inflation expectations.

The ABS and RBA make their statistics primarily available through Excel and/or
CSV spreadsheets.

This package provides functions to search and download data and statistics from
the [Australian Bureau of Statistics (ABS)](http://www.abs.gov.au/) and [Reserve
Bank of Australia (RBA)](https://www.rba.gov.au/) websites, as well as draft
access to the [ABS.Stat](http://stat.data.abs.gov.au/) - Beta data catalogue
API.

## Examples

### Downloading ABS Catalogue Statistics

ABS catalogue statistics may be downloaded, by catalogue number, using the
`abs_cat_stats()` function. The following example downloads all Consumer Price
Index (CPI) data series (ABS Catalogue no. 6401.0).

``` r
cpi_all <- abs_cat_stats("6401.0")
```

To download only the latest statistics reported in Table 1 (ABS groups Tables 1
and 2), simply provide a regular expression to the `tables` argument:

``` r
cpi <- abs_cat_stats("6401.0", tables="Table.+1")
```

The package also provides functions to ABS statistics via the
[ABS.Stat](http://stat.data.abs.gov.au/) Beta API. See the package help and
vignettes for examples.

### Downloading RBA data

RBA data series may be downloaded by table number, using the `rba_stats()`
function. The following example downloads Table A1 - Liabilities and Assets of
the RBA.

``` r
rba_bs <- rba_stats("A1")
```
