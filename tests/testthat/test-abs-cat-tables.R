
test_that("abs_get_old_site_tables returns a valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  url <- paste0("https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/",
                "6345.0Main+Features1Jun%202019?OpenDocument=")
  expect_s3_class(abs_get_old_site_tables(url), "data.frame");
})


test_that("abs_get_new_site_tables returns a valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  url <- paste0("https://www.abs.gov.au/statistics/economy/",
                "price-indexes-and-inflation/wage-price-index-australia/sep-2020");
  expect_s3_class(abs_get_new_site_tables(url), "data.frame");
})



test_that("abs_cat_tables fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  ## Test invalid title
  expect_error(abs_cat_tables(title="This is not a valid ABS filename"));
  ## Test invalid cat_no
  expect_error(abs_cat_tables(cat_no="5200.0"));
})


test_that("abs_cat_tables returns a valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## ABS Catalogue tables, by title (92, 94, 96, 99, 105, 108, 142, 166, 185, 206)
  series <- abs_cat_series(pattern="national.*income.*expenditure.*product",
                          level="title");
  expect_s3_class(abs_cat_tables(title=series$title), "cat_table");
  ## ABS Catalogue tables, by catalogue no. - 5206.0
  expect_s3_class(abs_cat_tables(cat_no="5206.0"), "cat_table");
  ## ABS Catalogue tables, by catalogue no. with URLs
  expect_s3_class(abs_cat_tables(cat_no="5206.0", releases="Latest", include_urls=TRUE),
                  "cat_table");
  ## ABS Catalogue tables, by catalogue no. time series
  expect_s3_class(abs_cat_tables(cat_no="6401.0", releases="Latest"),
                  "cat_table");
  ## ABS Catalogue tables, by catalogue no. data cube
  # expect_s3_class(abs_cat_tables(cat_no="1270.0.55.003", releases="Latest"),
  #                 "data.frame");
  ## ABS Catalogue tables - 8731
  expect_s3_class(abs_cat_tables(cat_no="8731.0", releases="Latest", include_urls=TRUE),
                  "cat_table");
  ## ABS Catalogue tables, by catalogue no. and specified release
  expect_s3_class(abs_cat_tables(cat_no="3218.0", releases="2005-06", include_urls=TRUE),
                  "cat_table");

  ## ABS Catalogue tables - 3218
  expect_s3_class(abs_cat_tables(cat_no="3218.0", releases="2005-06", include_urls=TRUE),
                  "data.frame");
})

