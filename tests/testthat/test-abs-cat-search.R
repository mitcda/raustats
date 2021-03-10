context("ABS Catalogue search function tests")

test_that("abs_cat_search check valid URL format",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(abs_cat_search("gross domestic product", return_url=TRUE), "character");
  expect_match(abs_cat_search("gross domestic product", return_url=TRUE),
               "https://search.abs.gov.au/s/search.html?.+");
})

test_that("abs_cat_search simple search test",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_search("gross domestic product"), "data.frame");
})


test_that("abs_cat_search date filter tests",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Today"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Past week"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Past fortnight"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Past month"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Past 3 months"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Past 6 months"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="Past year"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="2020"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="2015"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="2010"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="2005"), "data.frame");
  expect_s3_class(abs_cat_search("gross domestic product", n_results=25,
                                 follow_links=1, date_range="2000"), "data.frame");
})


test_that("abs_cat_search advanced query tests",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_search(pattern=list(any="gross domestic product"),
                                 date_range="Past 3 months"), "data.frame");
  expect_s3_class(abs_cat_search(pattern=list(phrase="consumer price index"),
                                 date_range="Past 3 months"), "data.frame");

})


test_that("abs_cat_search function argument tests",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(abs_cat_search(pattern=list(any="gross domestic product"),
                                 n_results=10, follow_links=15), "data.frame");
  expect_s3_class(abs_cat_search(pattern=list(any="consumer price index"),
                                 start_date='2001-10-15', end_date='2015-06-30'),
                  "data.frame");
  expect_s3_class(abs_cat_search(pattern=list(any="consumer price index"),
                                 n_results=20, follow_links=1,
                                 sort_by='Newest'), "data.frame");
  expect_s3_class(abs_cat_search(pattern=list(any="consumer price index"),
                                 n_results=20, follow_links=1,
                                 sort_by='Oldest'), "data.frame");
  expect_s3_class(abs_cat_search(pattern=list(any="consumer price index"),
                                 n_results=20, follow_links=1, sort_by='A-Z'), "data.frame");
  expect_s3_class(abs_cat_search(pattern=list(any="consumer price index"),
                                 n_results=20, follow_links=1, sort_by='Z-A'), "data.frame");
})


## ---------------------------------- EOF ----------------------------------- ##
