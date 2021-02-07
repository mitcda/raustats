
test_that("abs_get_old_site_tables returns a valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  url <- paste0("https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/",
                "6345.0Main+Features1Jun%202019?OpenDocument=")
  expect_s3_class(abs_get_old_site_tables(url), data.frame);
})


test_that("abs_get_new_site_tables returns a valid data frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  url <- paste0("https://www.abs.gov.au/statistics/economy/",
                "price-indexes-and-inflation/wage-price-index-australia/sep-2020");
  expect_s3_class(abs_get_new_site_tables(url), data.frame);
})



test_that("abs_cat_tables fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  invalid_title <- "No Proper Title, Australia";
  invalid_cat_no <- "5205.0";
  expect_error(abs_cat_tables(title=invalid_title));
  expect_error(abs_cat_tables(cat_no=invalid_cat_no));
})


test_that("abs_cat_tables returns a valid data.frame",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  ## ABS Catalogue releases - Wage Price Index, Australia (Cat no. 6345.0)
  expect_s3_class(abs_cat_tables(title="Wage Price Index, Australia"), "data.frame");
  expect_s3_class(abs_cat_tables(cat_no = "6345.0"), "data.frame");

  ## ABS Catalogue tables - 5206.0
  abs_tables_5206 <- abs_cat_tables("5206.0")
  expect_s3_class(abs_tables_5206, "data.frame");

  ## ABS Catalogue tables - 5206.0, with URLs
  abs_tables_5206_url <- abs_cat_tables("5206.0", releases="Latest", include_urls=TRUE);
  expect_s3_class(abs_tables_5206_url, "data.frame");

  ## ABS Catalogue tables - 6401.0, types="tss"
  abs_tables_6401 <- abs_cat_tables("6401.0", releases="Latest", types="tss");
  expect_s3_class(abs_tables_6401, "data.frame");

  ## ABS Catalogue tables - 1270.0.55.003, types="css" 
  abs_tables_1270.0.55.003 <- abs_cat_tables("1270.0.55.003", releases="Latest", types="css");
  expect_s3_class(abs_tables_1270.0.55.003, "data.frame");

  ## ABS Catalogue tables - 1292, types="pub"
  abs_tables_1292 <- abs_cat_tables("1292.0", releases="Latest", types="pub", include_urls=TRUE);
  expect_s3_class(abs_tables_1292, "data.frame");

  ## ABS Catalogue tables - 8731
  abs_tables_8731 <- abs_cat_tables("8731.0", releases="Latest", include_urls=TRUE);
  expect_s3_class(abs_tables_8731, "data.frame");

  ## ABS Catalogue tables - 3218
  abs_tables_3218 <- abs_cat_tables("3218.0", releases="2005-06", types=c("tss","css"), include_urls=TRUE);
  expect_s3_class(abs_tables_3218, "data.frame");

  abs_tables_3218_pub <- abs_cat_tables("3218.0", releases="2005-06", types=c("tss","css","pub"), include_urls=TRUE);
  expect_s3_class(abs_tables_3218_pub, "data.frame");
})

