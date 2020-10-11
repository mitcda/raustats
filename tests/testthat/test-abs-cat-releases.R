
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
  abs_release_5206 <- abs_cat_releases("5206.0");
  expect_s3_class(abs_release_5206, "data.frame");

  ## ABS Catalogue releases - 5206.0, with URLs
  abs_release_5206_url <- abs_cat_releases("5206.0", include_urls=TRUE);
  expect_s3_class(abs_release_5206_url, "data.frame");

  ## ABS Catalogue tables - 6401.0
  abs_release_6401 <- abs_cat_releases("6401.0");
  expect_s3_class(abs_release_6401, "data.frame");

  ## ABS Catalogue tables - 6401.0, with URLs
  abs_release_6401_url <- abs_cat_releases("6401.0", include_urls=TRUE);
  expect_s3_class(abs_release_6401_url, "data.frame");
})

