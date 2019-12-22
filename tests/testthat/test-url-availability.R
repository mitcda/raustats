context("raustats URL checking functions")

test_that("raustats_check_url_available returns true results",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Check valid ABS Catalogue URLs
  expect_null(raustats_check_url_available(abs_urls()$base_url))
  expect_null(raustats_check_url_available(file.path(abs_urls()$base_url,
                                                     abs_urls()$ausstats_path,
                                                     mf_path = "mf")));
  expect_null(raustats_check_url_available(file.path(abs_urls()$base_url,
                                                     abs_urls()$ausstats_path,
                                                     abs_urls()$mf_path,
                                                     "5206.0")));

  ## Check valid ABS API URLs
  expect_null(raustats_check_url_available(file.path(abs_api_urls()$base_url)));

  ## Check valid RBA paths
  expect_null(raustats_check_url_available(rba_urls()$base_url));
  expect_null(raustats_check_url_available(file.path(rba_urls()$base_url,
                                                     rba_urls()$stats_path)));
  expect_null(raustats_check_url_available(file.path(rba_urls()$base_url,
                                                     rba_urls()$stats_path,
                                                     rba_urls()$tables_path)));
})


test_that("raustats_check_url_available fails gracefully",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Test mis-specified ABS Catalogue URL
  expect_error(raustats_check_url_available(file.path(abs_urls()$base_url,
                                                      abs_urls()$ausstats_path)));
  
  ## Test mis-specified ABS API URL
  expect_error(raustats_check_url_available(file.path(abs_api_urls()$base_url,
                                                      abs_api_urls()$datastr_path,
                                                      abs_api_urls()$sdmx_json_path)));
  
  ## Test mis-specified RBA URLs
  expect_error(raustats_check_url_available(file.path(rba_urls()$base_url,
                                                      rba_urls()$stats_path,
                                                      "Table_1")));
})
