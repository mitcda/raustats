## context("raustats")

test_that("abs_ausstats_url returns valid URL",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_ausstats_url(), "character");
  expect_s3_class(rvest::html_session(abs_ausstats_url()), "session");
})


test_that("abs_read_tss_ returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  library(readxl); library(tidyr); 
  
  expect_s3_class(abs_read_tss_(file.path("data-raw", "5206001_key_aggregates.xls")),
                  "data.frame");
})


test_that("abs_read_tss returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  library(readxl); library(tidyr); 
  
  expect_s3_class(abs_read_tss(file.path("data-raw", "5206001_key_aggregates.xls")),
                  "data.frame");
  expect_s3_class(abs_read_tss(file.path("data-raw",
                                         c("5206001_key_aggregates.xls",
                                           "5206002_expenditure_volume_measures.xls"))),
                               "data.frame");
})


test_that("abs_cat_tables returns a valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  abs_5206_url <- "http://www.abs.gov.au/ausstats/abs@.nsf/mf/5206.0"
  expect_s3_class(abs_cat_tables(abs_5206_url), "data.frame");
})


test_that("abs_cat_tables fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  bad_url <- "http://www.rba.gov.au/"
  expect_error(abs_cat_tables(bad_url));
})



test_that("abs_cat_data returns valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  library(rvest); library(readxl); library(tidyr); library(dplyr);

  series <- "5206.0";
  ## -- Previous tables --
  tables <- c("Table 1", "Table 2");
  releases <- c("Mar 2017", "Dec 2016", "Sep 2016", "Jun 2016", "Mar 2016");

  ## Following commands presently fail

  expect_s3_class(x <- abs_cat_data("5206.0", tables=c("Table 1", "Table 2")), "data.frame");
  expect_s3_class(abs_cat_data("5206.0", tables=c("Table 1", "Table 2")), "data.frame");

  expect_s3_class(abs_cat_data("5206.0"), "data.frame");

  expect_s3_class(abs_cat_data("5206.0", tables="Table 1", release="Dec 2017"), "data.frame");

  
  # TO DO
  DEBUG <- FALSE
  if (DEBUG) {
    ## -- Current tables --
    ## tables <- "All";
    ## releases <- "Latest";
  }
})
