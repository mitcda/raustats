context("ABS Catalogue functions")

test_that("abs_ausstats_url returns valid URL",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_urls()$base_url, "character");
  expect_type(abs_urls()$ausstats_path, "character");
  expect_type(abs_urls()$downloads_regex, "character");
  expect_type(abs_urls()$releases_regex, "character");
})


test_that("abs_cat_tables fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  bad_url <- "http://www.rba.gov.au/"
  expect_error(abs_cat_tables(bad_url));
})


test_that("abs_cat_tables returns a valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## ABS Catalogue tables - 5206.0
  abs_tables_5206 <- abs_cat_tables("5206.0")
  expect_s3_class(abs_tables_5206, "data.frame");

  ## ABS Catalogue tables - 5206.0, with URLs
  abs_tables_5206_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
  expect_s3_class(abs_tables_5206_url, "data.frame");

  ## ABS Catalogue tables - 6401.0, types="tss"
  abs_tables_6401 <- abs_cat_tables("6401.0", releases="Latest", types="tss");
  expect_s3_class(abs_tables_6401, "data.frame");

  ## ABS Catalogue tables - 1270.0.55.003, types="css" 
  abs_tables_1270.0.55.003 <- abs_cat_tables("1270.0.55.003", releases="Latest", types="css");
  expect_s3_class(abs_tables_1270.0.55.003, "data.frame");

  ## ABS Catalogue tables - 1292, types="pub"
  abs_tables_1292 <- abs_cat_tables("1292.0", releases="Latest", types="pub", include_urls=TRUE);
  expect_s3_class(abs_tables_1292, "data.frame");
})


test_that("abs_local_filename created valid file name",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  test_all <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&all_time_series_workbooks.zip&5206.0&Time%20Series%20Spreadsheet&23EA5772544F27BECA2582FE001507D1&0&Jun%202018&05.09.2018&Latest"
  expect_match(abs_local_filename(test_all), "^\\w+\\.(zip|xlsx*)$");

  test_table_xls <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.xls&5206.0&Time%20Series%20Spreadsheet&C1145211D5AF80E5CA2582FE0014F063&0&Jun%202018&05.09.2018&Latest"
  expect_match(abs_local_filename(test_table_xls), "^\\w+\\.(zip|xlsx*)$");

  test_table_zip <- "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.zip&5206.0&Time%20Series%20Spreadsheet&C1145211D5AF80E5CA2582FE0014F063&0&Jun%202018&05.09.2018&Latest"
  expect_match(abs_local_filename(test_table_zip), "^\\w+\\.(zip|xlsx*)$");
})


test_that("abs_cat_download downloads specified table files",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  abs_tables_5206_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
  downloaded_tables <- abs_cat_download(head(abs_tables_5206_url$path_2), exdir=tempdir())
  expect_type(downloaded_tables, "character");
  expect_match(downloaded_tables, "\\w+\\.(zip|xlsx*)$");
  expect_true(all(file.exists(downloaded_tables)))
})


test_that("abs_cat_unzip extracts from valid filenames",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  abs_tables_5206_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
  downloaded_tables <- sapply(head(abs_tables_5206_url$path_2),
                              function(x) abs_cat_download(x, exdir=tempdir()));
  extracted_files <- abs_cat_unzip(downloaded_tables);
  expect_type(extracted_files, "character");
  expect_match(extracted_files, "\\w+\\.xlsx*$");
  expect_true(all(file.exists(extracted_files)));
})


test_that("abs_read_tss returns valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  abs_tables_5206_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
  downloaded_tables <- abs_cat_download(head(abs_tables_5206_url$path_2), exdir=tempdir())
  extracted_files <- abs_cat_unzip(downloaded_tables)
  expect_s3_class(abs_read_tss(extracted_files[1]), "data.frame"); ## Extract one file
  expect_s3_class(abs_read_tss(extracted_files), "data.frame");    ## Extract multiple files
})


test_that("abs_cat_stats tss call returns valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_stats("5206.0", tables="Table 1\\W+"), "data.frame");
  expect_s3_class(abs_cat_stats("5206.0", tables=c("Table 1\\W+", "Table 2\\W+")), "data.frame");
  expect_s3_class(abs_cat_stats("6401.0", tables="CPI.+All Groups"), "data.frame");
  expect_s3_class(abs_cat_stats("6401.0", tables="CPI.+All Groups", releases="Dec 2017"), "data.frame");
})
