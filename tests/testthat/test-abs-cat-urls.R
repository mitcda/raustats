context("ABS Catalogue URL functions")

test_that("abs_urls returns valid URLs",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_urls()$base_url, "character");
  expect_type(abs_urls()$ausstats_path, "character");
  expect_type(abs_urls()$downloads_regex, "character");
  expect_type(abs_urls()$releases_regex, "character");
  expect_type(abs_urls()$statistics_path, "character");
})


test_that("abs_expressions return valid strings",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_expressions()$xpath_release_links_class, "character");
  expect_type(abs_expressions()$xpath_release_views_row, "character");
  expect_type(abs_expressions()$regex_release_type, "character");
  expect_type(abs_expressions()$regex_nbsp, "character");
})


