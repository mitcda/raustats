context("ABS Catalogue select series function tests")

test_that("abs_cat_select check valid URL format",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_select(pattern = "wage price index", level = "topic",
                                 ignore.case = TRUE),
                  "data.frame");
})


## ---------------------------------- EOF ----------------------------------- ##
