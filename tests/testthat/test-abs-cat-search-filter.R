context("ABS Catalogue search date filter function")

test_that("abs_search_date_filter returns valid: Today",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_search_date_filter('Today'), "character");
  expect_match(abs_search_date_filter('Today'),
               "f\\.Date\\|d=\\d{2}\\w{3}\\d{4}\\+::\\+Today");
})

test_that("abs_search_date_filter returns valid: Past week, etc.",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Past week
  expect_type(abs_search_date_filter('Past week'), "character");
  expect_match(abs_search_date_filter('Past week'),
               "f\\.Date\\|d=d>(\\d{2}\\w{3}\\d{4})<(\\d{2}\\w{3}\\d{4})\\+::\\+Past\\+week");
  ## Past fortnight
  expect_type(abs_search_date_filter('Past fortnight'), "character");
  expect_match(abs_search_date_filter('Past fortnight'),
               "f\\.Date\\|d=d>(\\d{2}\\w{3}\\d{4})<(\\d{2}\\w{3}\\d{4})\\+::\\+Past\\+fortnight");
  ## Past month
  expect_type(abs_search_date_filter('Past month'), "character");
  expect_match(abs_search_date_filter('Past month'),
               "f\\.Date\\|d=d>(\\d{2}\\w{3}\\d{4})<(\\d{2}\\w{3}\\d{4})\\+::\\+Past\\+month");
  ## Past 3 months
  expect_type(abs_search_date_filter('Past 3 months'), "character");
  expect_match(abs_search_date_filter('Past 3 months'),
               "f\\.Date\\|d=d>(\\d{2}\\w{3}\\d{4})<(\\d{2}\\w{3}\\d{4})\\+::\\+Past\\+3\\+months");
  ## Past 6 months
  expect_type(abs_search_date_filter('Past 6 months'), "character");
  expect_match(abs_search_date_filter('Past 6 months'),
               "f\\.Date\\|d=d>(\\d{2}\\w{3}\\d{4})<(\\d{2}\\w{3}\\d{4})\\+::\\+Past\\+6\\+months");
  ## Past year
  expect_type(abs_search_date_filter('Past year'), "character");
  expect_match(abs_search_date_filter('Past year'),
               "f\\.Date\\|d=d>(\\d{2}\\w{3}\\d{4})<(\\d{2}\\w{3}\\d{4})\\+::\\+Past\\+year");
})


test_that("abs_search_date_filter returns valid calendar year",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_search_date_filter('2020'), "character");
  expect_match(abs_search_date_filter('2020'),
               "f\\.Date\\|d=d=\\d{4}\\+::\\+\\d{4}");
})

## ---------------------------------- EOF ----------------------------------- ##
