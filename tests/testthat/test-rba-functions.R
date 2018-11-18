## context("raustats")

test_that("rba_stats_url returns valid URL",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(rba_urls(), "list");
  expect_s3_class(rvest::html_session(rba_urls()$base_url), "session");
})


test_that("rba_table_cache returns data.frame class object",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  DEBUG
  
  expect_s3_class(rba_table_cache(), "data.frame");
})

## -- UP TO HERE --

test_that("rba_search fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  DEBUG <- FALSE
    if (DEBUG) {
        library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
        table_code = "A1";
    }
})

test_that("rba_search returns valid results",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

})


test_that("rba_read_tss_ returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## library(readxl); library(tidyr); 
  
  expect_s3_class(rba_read_tss_(file.path("data-raw", "5206001_key_aggregates.xls")),
                  "data.frame");
})


test_that("rba_read_tss returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## library(readxl); library(tidyr); 
  
  expect_s3_class(rba_read_tss(file.path("data-raw", "5206001_key_aggregates.xls")),
                  "data.frame");
  expect_s3_class(rba_read_tss(file.path("data-raw",
                                         c("5206001_key_aggregates.xls",
                                           "5206002_expenditure_volume_measures.xls"))),
                               "data.frame");
})


test_that("rba_data returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## library(rvest); library(readxl); library(tidyr); library(dplyr);

  ##     table_code = "A1";
  ##     series_type="statistical tables";
  ##     update_cache=FALSE;
    
  expect_s3_class(rba_data("A1", tables="Table 1"), "data.frame");
  expect_s3_class(rba_data("5206.0", tables=c("Table 1", "Table 2")), "data.frame");
  expect_s3_class(rba_data("5206.0", tables="all"), "data.frame");
  expect_s3_class(rba_data("5206.0", tables="Table 1", release="Dec 2017"), "data.frame");
  expect_s3_class(rba_data("5206.0", tables="all", release="Dec 2016"), "data.frame");
})
