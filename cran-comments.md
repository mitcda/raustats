# New submission

## Test environments
* local Debian 4.18, R 3.5.1 and R-devel
* win-builder (release and devel)


## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘David Mitchell <mitcda@iinet.net.au>’
 
    New submission

* checking installed package size ... NOTE
    installed size is  7.3Mb
    sub-directories of 1Mb or more:
      data   6.9Mb

  The dataset includes a cached list of all data series available
  through the ABS.Stat API for faster access.

* checking examples ... NOTE
    Examples with CPU or elapsed time > 5s
                     user system elapsed
    abs_stats      11.073  0.060  31.835
    abs_cat_stats   3.461  0.127  15.717
    abs_dimensions  3.173  0.028   8.342
    rba_stats       2.913  0.064   9.471
    abs_datasets    2.585  0.020   7.956
    abs_cat_tables  2.575  0.024   9.868
    abs_metadata    2.326  0.032   6.770

  These functions download and transform into R data frame objects one or more
  specified datasets from the ABS or RBA websites. The website response accounts
  for the majority of the elapsed time.


## Downstream dependencies
There are currently no downstream dependencies for this package.



##  Win-release outputs

* using log directory 'd:/RCompile/CRANguest/R-release/raustats.Rcheck'
* using R version 3.5.1 (2018-07-02)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
* checking for file 'raustats/DESCRIPTION' ... OK
* checking extension type ... Package
* this is package 'raustats' version '0.1.0'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'David Mitchell <mitcda@iinet.net.au>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  RBA (3:52, 6:4)
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking serialization versions ... OK
* checking whether package 'raustats' can be installed ... OK
* checking installed package size ... NOTE
  installed size is  7.3Mb
  sub-directories of 1Mb or more:
    data   6.9Mb
* checking package directory ... OK
* checking 'build' directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* loading checks for arch 'i386'
** checking whether the package can be loaded ... OK
** checking whether the package can be loaded with stated dependencies ... OK
** checking whether the package can be unloaded cleanly ... OK
** checking whether the namespace can be loaded with stated dependencies ... OK
** checking whether the namespace can be unloaded cleanly ... OK
** checking loading without being on the library search path ... OK
** checking use of S3 registration ... OK
* loading checks for arch 'x64'
** checking whether the package can be loaded ... OK
** checking whether the package can be loaded with stated dependencies ... OK
** checking whether the package can be unloaded cleanly ... OK
** checking whether the namespace can be loaded with stated dependencies ... OK
** checking whether the namespace can be unloaded cleanly ... OK
** checking loading without being on the library search path ... OK
** checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [7s] OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of 'data' directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from 'inst/doc' ... OK
* checking files in 'vignettes' ... OK
* checking examples ...
** running examples for arch 'i386' ... ERROR
Running examples in 'raustats-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: rba_stats
> ### Title: Return data for a specified RBA time series
> ### Aliases: rba_stats
> 
> ### ** Examples
> 
>  ## Example - table_no
>  x <- rba_stats("A1");
trying URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Warning in download.file(x, y, mode = "wb") :
  InternetOpenUrl failed: 'Eine Umleitungsanforderung �ndert eine nicht sichere in eine sichere Verbindung.'
Error in download.file(x, y, mode = "wb") : 
  cannot open URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Calls: rba_stats ... lapply -> FUN -> mapply -> <Anonymous> -> download.file
Execution halted
** running examples for arch 'x64' ... ERROR
Running examples in 'raustats-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: rba_stats
> ### Title: Return data for a specified RBA time series
> ### Aliases: rba_stats
> 
> ### ** Examples
> 
>  ## Example - table_no
>  x <- rba_stats("A1");
trying URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Warning in download.file(x, y, mode = "wb") :
  InternetOpenUrl failed: 'Eine Umleitungsanforderung �ndert eine nicht sichere in eine sichere Verbindung.'
Error in download.file(x, y, mode = "wb") : 
  cannot open URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Calls: rba_stats ... lapply -> FUN -> mapply -> <Anonymous> -> download.file
Execution halted
* checking for unstated dependencies in 'tests' ... OK
* checking tests ...
** running tests for arch 'i386' ... [2s] OK
  Running 'testthat.R' [2s]
** running tests for arch 'x64' ... [2s] OK
  Running 'testthat.R' [2s]
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in 'inst/doc' ... OK
* checking re-building of vignette outputs ... [321s] WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
Loading required package: readxl
Attaching package: 'raustats'
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202018?OpenDocument
trying URL 'http://www.abs.gov.au/ausstats/meisubs.nsf/log?openagent&all_time_series_workbooks.zip&5206.0&Time%20Series%20Spreadsheet&DD1763855C6A69B0CA25835900114425&0&Sep%202018&05.12.2018&Latest'
Content type 'application/x-zip' length 4325820 bytes (4.1 MB)
==================================================
downloaded 4.1 MB

Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202018?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/6401.0Sep%202018?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Dec%202017?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202017?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202018?OpenDocument
trying URL 'http://www.abs.gov.au/ausstats/meisubs.nsf/log?openagent&5206001_key_aggregates.zip&5206.0&Time%20Series%20Spreadsheet&7B93ED810F0472C9CA2583590011294C&0&Sep%202018&05.12.2018&Latest'
Content type 'application/x-zip' length 102088 bytes (99 KB)
==================================================
downloaded 99 KB

trying URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Quitting from lines 738-739 (raustats_introduction.Rmd) 
Error: processing vignette 'raustats_introduction.Rmd' failed with diagnostics:
cannot open URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Execution halted

* checking PDF version of manual ... OK
* DONE
Status: 2 ERRORs, 1 WARNING, 2 NOTEs

  
  
## Win-devel check output

* using log directory 'd:/RCompile/CRANguest/R-devel/raustats.Rcheck'
* using R Under development (unstable) (2018-12-11 r75837)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
* checking for file 'raustats/DESCRIPTION' ... OK
* checking extension type ... Package
* this is package 'raustats' version '0.1.0'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'David Mitchell <mitcda@iinet.net.au>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  RBA (3:52, 6:4)
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking serialization versions ... OK
* checking whether package 'raustats' can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking 'build' directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* loading checks for arch 'i386'
** checking whether the package can be loaded ... OK
** checking whether the package can be loaded with stated dependencies ... OK
** checking whether the package can be unloaded cleanly ... OK
** checking whether the namespace can be loaded with stated dependencies ... OK
** checking whether the namespace can be unloaded cleanly ... OK
** checking loading without being on the library search path ... OK
** checking use of S3 registration ... OK
* loading checks for arch 'x64'
** checking whether the package can be loaded ... OK
** checking whether the package can be loaded with stated dependencies ... OK
** checking whether the package can be unloaded cleanly ... OK
** checking whether the namespace can be loaded with stated dependencies ... OK
** checking whether the namespace can be unloaded cleanly ... OK
** checking loading without being on the library search path ... OK
** checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [8s] OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of 'data' directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from 'inst/doc' ... OK
* checking files in 'vignettes' ... OK
* checking examples ...
** running examples for arch 'i386' ... ERROR
Running examples in 'raustats-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: rba_stats
> ### Title: Return data for a specified RBA time series
> ### Aliases: rba_stats
> 
> ### ** Examples
> 
>  ## Example - table_no
>  x <- rba_stats("A1");
trying URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Warning in download.file(x, y, mode = "wb") :
  InternetOpenUrl failed: 'Eine Umleitungsanforderung �ndert eine nicht sichere in eine sichere Verbindung.'
Error in download.file(x, y, mode = "wb") : 
  cannot open URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Calls: rba_stats ... lapply -> FUN -> mapply -> <Anonymous> -> download.file
Execution halted
** running examples for arch 'x64' ... ERROR
Running examples in 'raustats-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: abs_stats
> ### Title: Download data from the ABS API
> ### Aliases: abs_stats
> 
> ### ** Examples
> 
>    x <- abs_stats(dataset="CPI", filter="all", return_url=TRUE);
>    x <- abs_stats(dataset="CPI", filter=list(MEASURE=1, REGION=c(1:8,50),
+                                              INDEX=10001, TSEST=10, FREQUENCY="Q"));
Error in doc_parse_raw(x, encoding = encoding, base_url = base_url, as_html = as_html,  : 
  Entity 'nbsp' not defined [26]
Calls: abs_stats ... read_xml.connection -> read_xml.raw -> doc_parse_raw
Execution halted
* checking for unstated dependencies in 'tests' ... OK
* checking tests ...
** running tests for arch 'i386' ... [3s] OK
  Running 'testthat.R' [2s]
** running tests for arch 'x64' ... [2s] OK
  Running 'testthat.R' [2s]
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in 'inst/doc' ... OK
* checking re-building of vignette outputs ... [214s] WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
Loading required package: readxl
Attaching package: 'raustats'
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202018?OpenDocument
trying URL 'http://www.abs.gov.au/ausstats/meisubs.nsf/log?openagent&all_time_series_workbooks.zip&5206.0&Time%20Series%20Spreadsheet&DD1763855C6A69B0CA25835900114425&0&Sep%202018&05.12.2018&Latest'
Content type 'application/x-zip' length 4325820 bytes (4.1 MB)
==================================================
downloaded 4.1 MB

Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202018?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/6401.0Sep%202018?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Dec%202017?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202017?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument
Navigating to /AUSSTATS/abs@.nsf/DetailsPage/5206.0Sep%202018?OpenDocument
trying URL 'http://www.abs.gov.au/ausstats/meisubs.nsf/log?openagent&5206001_key_aggregates.zip&5206.0&Time%20Series%20Spreadsheet&7B93ED810F0472C9CA2583590011294C&0&Sep%202018&05.12.2018&Latest'
Content type 'application/x-zip' length 102088 bytes (99 KB)
==================================================
downloaded 99 KB

trying URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Quitting from lines 738-739 (raustats_introduction.Rmd) 
Error: processing vignette 'raustats_introduction.Rmd' failed with diagnostics:
cannot open URL 'http://www.rba.gov.au/statistics/tables/xls/a01whist-summary.xls'
Execution halted

* checking PDF version of manual ... OK
* DONE
Status: 2 ERRORs, 1 WARNING, 1 NOTE

