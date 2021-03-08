context("ABS Catalogue series function tests")

test_that("abs_cat_series check valid URL format",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_series(pattern = "wage price index", level = "title",
                                 ignore.case = TRUE),
                  "data.frame");

  expect_s3_class(abs_cat_series(pattern="national.*income.*expenditure.*product",
                                 include_urls = TRUE),
                  "data.frame");

  expect_s3_class(abs_cat_series(pattern="national.*income.*expenditure.*product",
                                 level=c('group', 'view', 'title'),
                                 include_urls = TRUE),
                  "data.frame");
})


## ---------------------------------- EOF ----------------------------------- ##
