context("ABS Catalogue release functions")

test_that("abs_cat_releases fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## Check error on invalid ABS Cat. no.
  bad_url <- "Invalid_Cat_no"
  expect_error(abs_cat_releases(bad_url));
  ## No ABS Cat. no.
  expect_error(abs_cat_releases());
})


test_that("abs_cat_releases returns a valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## ABS Catalogue releases - 5206.0
  series <- abs_cat_series(pattern="national.*income.*expenditure.*product",
                          level="title");
  expect_s3_class(abs_cat_releases(title=series$title), "data.frame");
  expect_s3_class(abs_cat_releases(cat_no="5206.0"), "data.frame");

  ## ABS Catalogue releases - 5206.0, with URLs
  expect_s3_class(abs_cat_releases(title=series$title, include_urls=TRUE), "data.frame");
  expect_s3_class(abs_cat_releases(cat_no="5206.0", include_urls=TRUE), "data.frame");

  ## ABS Catalogue tables - 6401.0
  series <- abs_cat_series(pattern="consumer.*price.*index.*australia", level="title");
  expect_s3_class(abs_cat_releases(title=series$title[1]), "data.frame");
  expect_s3_class(abs_cat_releases(cat_no="6401.0"), "data.frame");

  ## ABS Catalogue tables - 6401.0, with URLs
  expect_s3_class(abs_cat_releases(title=series$title[1], include_urls=TRUE), "data.frame");
  expect_s3_class(abs_cat_releases(cat_no="6401.0", include_urls=TRUE), "data.frame");
})

