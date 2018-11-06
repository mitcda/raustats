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

  expect_s3_class(abs_cat_tables("5206.0"), "data.frame");
  expect_s3_class(abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE), "data.frame");
  expect_s3_class(abs_cat_tables("6401.0", releases="Latest", types="tss"), "data.frame");
  expect_s3_class(abs_cat_tables("1270.0.55.003", releases="Latest", types="css"), "data.frame");
  expect_s3_class(anzsic_2006 <- abs_cat_tables("1292.0", releases="Latest", types="pub", include_urls=TRUE),
                  "data.frame");
})


test_that("abs_local_filename created valid file name",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  test_all <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&all_time_series_workbooks.zip&5206.0&Time%20Series%20Spreadsheet&23EA5772544F27BECA2582FE001507D1&0&Jun%202018&05.09.2018&Latest"
  test_table_xls <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.xls&5206.0&Time%20Series%20Spreadsheet&C1145211D5AF80E5CA2582FE0014F063&0&Jun%202018&05.09.2018&Latest"
  test_table_zip <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.zip&5206.0&Time%20Series%20Spreadsheet&C1145211D5AF80E5CA2582FE0014F063&0&Jun%202018&05.09.2018&Latest"

  expect_match(abs_local_filename(test_all), "^\\w+\\.(zip|xlsx*)$");
  expect_match(abs_local_filename(test_table_xls), "^\\w+\\.(zip|xlsx*)$");
  expect_match(abs_local_filename(test_table_zip), "^\\w+\\.(zip|xlsx*)$");
})



test_that("abs_cat_tables fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  bad_url <- "http://www.rba.gov.au/"
  expect_error(abs_cat_tables(bad_url));
})



test_that("abs_cat_stats tss call returns valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  library(rvest); library(readxl);

  expect_s3_class(abs_cat_stats("5206.0", tables="Table 1\\W+"), "data.frame");
  expect_s3_class(abs_cat_stats("5206.0", tables=c("Table 1\\W+", "Table 2\\W+")), "data.frame");
  expect_s3_class(abs_cat_stats("6401.0", tables="CPI.+All Groups"), "data.frame");
  expect_s3_class(abs_cat_stats("6401.0", tables="CPI.+All Groups", releases="Dec 2017"), "data.frame");
})
