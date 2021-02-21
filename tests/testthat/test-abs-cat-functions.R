context("ABS Catalogue functions")

test_that("abs_ausstats_url returns valid URL",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_urls()$base_url, "character");
  expect_type(abs_urls()$ausstats_path, "character");
  expect_type(abs_urls()$mf, "character");
  expect_type(abs_urls()$statistics_path, "character");
  expect_type(abs_urls()$downloads_regex, "character");
  expect_type(abs_urls()$releases_regex, "character");
  expect_type(abs_urls()$search_url, "character");
  expect_type(abs_urls()$search_path, "character");
})


test_that("abs_cat_search returns a data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Test ABS search function - character pattern
  expect_s3_class(abs_cat_search(pattern="gross domestic product"), "data.frame");
  ## Test ABS search function - list pattern
  expect_s3_class(
    abs_cat_search(pattern=list(any="gross domestic product",
                                all="gross domestic", phrase=NULL, not=NULL)),
    "data.frame");
  ## Test ABS search function - other arguments
  expect_s3_class(
    abs_cat_search(pattern=list(any="gross domestic product", all="gross domestic",
                                phrase=NULL, not=NULL),
                   resource="Statistical analysis and data",
                   sort_by="Newest", start_date='2001-10-15', end_date='2015-06-30',
                   follow_links=5),
    "data.frame");
  ## Test ABS search function - refine date
  expect_s3_class(
    abs_cat_search("gross domestic product", refine_date="Past 3 months"),
    "data.frame");
  ## Test ABS search function - sort_by
  expect_s3_class(
    abs_cat_search(pattern=list(any="consumer price index"), sort_by='A-Z'),
    "data.frame");
})


test_that("abs_cat_select returns a data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Test ABS search function - character pattern
  expect_s3_class(
    abs_cat_select(pattern="wage price index"),
    "data.frame");
  ## Test ABS search function - list pattern
  expect_s3_class(
    abs_cat_select(pattern="national.*income.*expenditure.*product",
                   level="topic"),
    "data.frame");
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

  test_new_url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/dec-2019/640101.xls"
  expect_match(abs_local_filename(test_new_url), "^\\w+\\.(zip|xlsx*)$");
})


test_that("abs_cat_download downloads specified table files",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Test abs_cat_download.cat_table works 
  abs_tables_5206 <- abs_cat_tables(cat_no="5206.0", releases="Latest", include_urls=TRUE);
  expect_s3_class(abs_tables_5206, "cat_table");
  
  downloaded_tables <- abs_cat_download(abs_tables_5206[1:2,], exdir=tempdir());
  expect_type(downloaded_tables, "character");
  expect_match(downloaded_tables, "\\w+\\.(zip|xlsx*)$");
  expect_true(all(file.exists(downloaded_tables)));

  ## Test abs_cat_download.default works with 'new' ABS website  
  downloaded_tables <- abs_cat_download(abs_tables_5206$file_url[1:2], exdir=tempdir());
  expect_type(downloaded_tables, "character");
  expect_match(downloaded_tables, "\\w+\\.(zip|xlsx*)$");
  expect_true(all(file.exists(downloaded_tables)));

  ## Test abs_cat_download.default works with 'old' ABS website  
  abs_tables_5206 <- abs_cat_tables(cat_no="5206.0", releases="Dec 2018", include_urls=TRUE);
  downloaded_tables <- abs_cat_download(abs_tables_5206$file_url[2:3], exdir=tempdir());
  expect_type(downloaded_tables, "character");
  expect_match(downloaded_tables, "\\w+\\.(zip|xlsx*)$");
  expect_true(all(file.exists(downloaded_tables)));
})


test_that("abs_cat_unzip extracts from valid filenames",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  abs_tables_5206_url <- abs_cat_tables(cat_no="5206.0", releases="Latest", include_urls=TRUE);
  downloaded_tables <-
    abs_cat_download(abs_tables_5206_url[grep("all.*time.*series",
                                              abs_tables_5206_url$file_name, ignore.case=TRUE),
                                         "file_url"],
                     exdir=tempdir());
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

  abs_tables_5206_url <- abs_cat_tables(cat_no="5206.0", releases="Latest", include_urls=TRUE);
  downloaded_tables <-
    abs_cat_download(abs_tables_5206_url[grep("all.*time.*series",
                                              abs_tables_5206_url$file_name, ignore.case=TRUE),
                                         "file_url"],
                     exdir=tempdir());
  extracted_files <- abs_cat_unzip(downloaded_tables);
  expect_s3_class(abs_read_tss(extracted_files[1,]), "data.frame"); ## Extract one file
  expect_s3_class(abs_read_tss(extracted_files[1:2]), "data.frame");    ## Extract multiple files
})


test_that("abs_cat_stats tss call returns valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## -- National Accounts
  ## Import ABS statistics, by title
  series <- abs_cat_select(pattern="national.*income.*expenditure.*product",
                           level="topic");
  expect_s3_class(abs_cat_stats(title=series$topic, tables="Table 1\\W+"), "data.frame");
  ## Import ABS statistics, by catalogue no.
  expect_s3_class(abs_cat_stats(cat_no="5206.0", tables="Table 1\\W+"), "data.frame");
  expect_s3_class(abs_cat_stats(cat_no="5206.0", tables=c("Table 1\\W+", "Table 2\\W+")),
                  "data.frame");
  ## Import ABS Consumer Price Index, by title
  series <- abs_cat_select(pattern="consumer.*price.*index", level="topic")[1, "topic"];
  expect_s3_class(abs_cat_stats(title=series, tables="CPI.+All Groups"), "data.frame");
  ## Import ABS Consumer Price Index, by catalogue no.
  expect_s3_class(
    abs_cat_stats(cat_no="6401.0", tables="CPI.+All Groups"),
    "data.frame");
  ## Import ABS Consumer Price Index, by catalogue no. and selected release
  expect_s3_class(
    abs_cat_stats(cat_no="6401.0", tables="CPI.+All Groups", releases="Dec 2017"),
    "data.frame");
  ## Import ABS Catalogue no. 8731.0, selected tables
  expect_s3_class(
    abs_cat_stats(cat_no="8731.0", tables=c("TABLE 01\\W+", "TABLE 02\\W+")),
    "data.frame");
})
