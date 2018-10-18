## context("raustats")

test_that("abs_api_call creates proper url",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  library(rvest); library(xml2);
  abs_api_call(path=abs_api_urls()$datastr_path, args="all");
}
)

test_that("abs_call_api creates xml_document",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  library(rvest); library(xml2);
  url <- abs_api_call(path=abs_api_urls()$datastr_path, args="all");
  expect_s3_class(abs_call_api(url), "xml_document");
  expect_s3_class(abs_call_api(url), "xml_node");
}
)

test_that("abs_series returns object of class data.frame with specified names",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  library(rvest); library(xml2);
  x <- abs_series(include_notes=TRUE)
  expect_s3_class(x, "data.frame");
  expect_named(x, c("agencyID", "id", "name", "notes"))
})


test_that("abs_metadata returns object of class list with specified names",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  library(testthat);  library(rvest); library(xml2);
  x <- abs_metadata("CPI");
  expect_type(x, "list");
  expect_named(x, c("CL_CPI_MEASURE","CL_CPI_REGION","CL_CPI_INDEX","CL_CPI_TSEST",
                    "CL_CPI_FREQUENCY","CL_CPI_TIME","CL_CPI_OBS_STATUS","CL_CPI_TIME_FORMAT"));
})


test_that("inverse_fn works",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # TO DO
}
)
