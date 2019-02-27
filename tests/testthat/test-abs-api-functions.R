context("ABS API functions")

test_that("abs_api_call creates proper url",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_match(abs_api_call(path=abs_api_urls()$datastr_path, args="all"),
               "http:\\/\\/stat\\.data\\.abs\\.gov\\.au\\/.+\\/all");
  expect_false(httr::http_error(abs_api_call(path=abs_api_urls()$datastr_path, args="all")));
})


## test_that("abs_api_call returns error if url is invalid",
## {
##   skip_on_cran()
##   skip_on_travis()
##   skip_on_appveyor()

##   expect_true(httr::http_error(sub("\\.au", "",
##                                    abs_api_call(path=abs_api_urls()$datastr_path, args="all"))));
##   expect_true(httr::http_error(sub("Structure", "",
##                                    abs_api_call(path=abs_api_urls()$datastr_path, args="all"))));
## })


test_that("abs_call_api creates xml_document",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  url <- abs_api_call(path=abs_api_urls()$datastr_path, args="all");
  expect_s3_class(abs_call_api(url), "xml_document");
  expect_s3_class(abs_call_api(url), "xml_node");
})


test_that("abs_datasets returns object of class data.frame with specified names",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  x <- abs_datasets(include_notes=TRUE)
  expect_s3_class(x, "data.frame");
  expect_named(x, c("agencyID", "id", "name", "notes"), ignore.order=TRUE)
})


test_that("abs_metadata returns object of class list with specified names",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  x <- abs_metadata("CPI");
  expect_type(x, "list");
  expect_named(x, c("CL_CPI_MEASURE","CL_CPI_REGION","CL_CPI_INDEX","CL_CPI_TSEST",
                    "CL_CPI_FREQUENCY","CL_CPI_TIME","CL_CPI_OBS_STATUS","CL_CPI_TIME_FORMAT"),
               ignore.order=TRUE);
})


## test_that("abs_cache returns object of class list with specified names",
## {
##   skip_on_cran()
##   skip_on_travis()
##   skip_on_appveyor()

##   skip("abs_cache() test skipped -- takes long time to download all ABS series.")
##   abs_cachelist <- abs_cache(progress=5)
##   expect_type(abs_cachelist, "list");
## })

## test_that("abs_cachelist returns object of class table with specified names",
## {
##   skip_on_cran()
##   skip_on_travis()
##   skip_on_appveyor()

##   abs_ct <- abs_cachelist2table(raustats::abs_cachelist)
##   expect_s3_class(abs_ct, "data.frame");
##   expect_named(abs_ct, c("dataset","dataset_description"), ignore.order=TRUE, ignore.case=TRUE);
## })

test_that("abs_dimensions returns named data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  abs_dim <- abs_dimensions("CPI")
  expect_s3_class(abs_dim, "data.frame");
  expect_named(abs_dim, c("name","type"), ignore.order=TRUE, ignore.case=TRUE);
})

test_that("abs_search returns a list with specified names",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  abs_dataset_search <- abs_search("consumer price index")
  expect_s3_class(abs_dataset_search, "data.frame");
  expect_named(abs_dataset_search, c("id", "agencyID", "name"),
               ignore.order=TRUE, ignore.case=TRUE);

  abs_indicator_search <- abs_search("all groups", dataset="CPI")
  expect_type(abs_indicator_search, "list");
  expect_named(abs_indicator_search[[1]], c("code","description"),
               ignore.order=TRUE, ignore.case=TRUE);
})

test_that("abs_stats fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## library(testthat);
  expect_error(abs_stats());              ## No dataset provided
  expect_error(abs_stats("INVALID_ID"));  ## Non-existent dataset
  expect_error(abs_stats("CPI"));         ## No filter supplied
  expect_error(abs_stats("CPI", filter="invalid_filter"));  ## Invalid filter value
  expect_error(abs_stats("CPI", filter=list(MEASURE=1, REGION=c(1:8,50),
                                            INDEX=10001, TSEST=10, FREQUENCY="Q"),
                         start_date=2008, end_date=2006));  ## start_date > end_date
})

test_that("abs_stats returns valid URL",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_match(abs_stats("CPI", filter="all", return_url=TRUE),
               "^http:\\/\\/stat.data.abs.gov.au\\/SDMX-JSON\\/data\\/CPI");
})

test_that("abs_stats returns valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Test specific filter and start/end dates
  expect_s3_class(abs_stats("CPI", filter=list(MEASURE=1, REGION=c(1:8,50),
                                               INDEX=10001, TSEST=10, FREQUENCY="Q"),
                            start_date="2008-Q3", end_date="2018-Q2"), "data.frame");
  ## Test incomplete filter set
  expect_warning(xx <- abs_stats("CPI", filter=list(REGION=c(1:8,50),
                                                    INDEX=10001, TSEST=10, FREQUENCY="Q"),
                                 start_date="2008-Q3", end_date="2018-Q2"));
  expect_s3_class(xx, "data.frame");
  ## Test function returns character string
  expect_warning(xx <- abs_stats("CPI", filter=list(REGION=c(1:8,50),
                                                    INDEX=10001, TSEST=10, FREQUENCY="Q"),
                                 start_date="2008-Q3", end_date="2018-Q2", return_url=TRUE));
  expect_type(xx, "character");
})
