context("RBA functions")

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

  expect_s3_class(rba_table_cache(), "data.frame");
})


test_that("rba_search fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_error(rba_search())
})


test_that("rba_search returns valid results",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(rba_search(pattern = "Liabilities and Assets"), "data.frame");
  expect_s3_class(rba_search(pattern = "Consumer Prices"), "data.frame");
  expect_s3_class(rba_search(pattern = "Population"), "data.frame");
})


test_that("rba_file_download returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  downloaded_tables <- rba_file_download("https://www.rba.gov.au/statistics/tables/xls/d01hist.xls")
  expect_type(downloaded_tables, "character");
  expect_match(downloaded_tables, "\\w+\\.xlsx*$");
  expect_true(all(file.exists(downloaded_tables)));

})


test_that("rba_read_tss returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  rba_urls <- rba_search(pattern = "Liabilities and Assets")$url
  rba_files <- sapply(rba_urls, rba_file_download);
  expect_s3_class(rba_read_tss(rba_files), "data.frame");
})


test_that("rba_stats returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Test 'table_no' option function call
  expect_s3_class(rba_stats("A1"), "data.frame");
  expect_s3_class(rba_stats(table_no="A1"), "data.frame");
  ## Test 'pattern' option function call
  expect_s3_class(rba_stats(pattern="Liabilities and Assets"), "data.frame");
  ## Test 'url' option function call
  url <- "https://www.rba.gov.au/statistics/tables/xls/d01hist.xls";
  expect_false(httr::http_error(url));
  expect_s3_class(rba_stats(url=url), "data.frame");
})

