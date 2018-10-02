## context("raustats")

test_that("rba_stats_url returns valid URL",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_type(rba_stats_url(), "character");
  expect_s3_class(rvest::html_session(rba_stats_url()), "session");
})


test_that("rba_table_cache returns data.frame class object",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(rba_table_cache(), "data.frame");
})


## -- UP TO HERE --

test_that("rba_search fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  DEBUG <- FALSE
    if (DEBUG) {
        library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
        table_code = "A1";
    }
  
  A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=4);
  B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=4);
  B.ann <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=1);
  expect_error(splice_series(A, c(NA,  NA,  NA, 100, 120, 150, 200, 225),
                             base.period=c(1991,4)))             ## y - Not ts
  expect_error(splice_series(A, B, base.period=4))               ## base.period of incorrect length
  expect_error(splice_series(A, B,
                             base.period=as.Date("1991-12-01"))) ## base.period not numeric
  expect_error(splice_series(A, B.ann, base.period=c(1991,4)))   ## x & y of different frequencies
  expect_error(splice_series(A, B, base.period=c(1993,1)))       ## base.period y index out of range
})


    ## DEBUG <- FALSE
    ## if (DEBUG) {
    ##     library(magrittr); library(dplyr); library(purrr); library(rvest); library(urltools);
    ##     table_code = "A1";
    ##     series_type="statistical tables";
    ##     update_cache=FALSE;
    ## }





test_that("splice_series works on time series (ts) objects",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=4);
  B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=4);

  expect_equal(typeof(splice_series(A, B, base.period=c(1991,4))), typeof(A)) 
  expect_equal(class(splice_series(A, B, base.period=c(1991,4))), class(B))
  expect_length(splice_series(A, B, base.period=c(1991,4)), 8);
}
)


test_that("splice_series.xts fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- xts(x=c(100, 95, 125, 150,  NA,  NA,  NA,  NA),
              order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
  B <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
           order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
  B.POSIXct <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
           order.by=seq(as.POSIXct("1991-03-01"), by="quarter", length.out=8));
  B.ann <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
               order.by=seq(as.Date("1991-06-01"), by="year", length.out=8));
  A.ts <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=4);
  B.ts <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=4);

  expect_error(splice_series(A, B.ts,
                             base.period=as.Date("1991-12-01")))    ## y - Not xts
  expect_error(splice_series(A, B.POSIXct,
                             base.period=as.Date("1991-12-01")))    ## Object time indexes of different class
  expect_error(splice_series(A, B,
                             base.period=as.POSIXct("1991-12-01"))) ## base.period not of same time class
  expect_error(splice_series(A, B, base.period=as.Date("1993-03-01"))) ## base.period y index out of range
  expect_error(splice_series(A, B, base.period=c(1993,1)))          ## x & y of different frequencies
})


test_that("splice_series works on extensible time series (xts) objects",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- xts(x=c(100, 95, 125, 150,  NA,  NA,  NA,  NA),
              order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
  B <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
            order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
 
  expect_equal(typeof(splice_series(A, B, base.period=as.Date("1991-12-01"))), typeof(A))
  expect_equal(class(splice_series(A, B, base.period=as.Date("1991-12-01"))), class(A))
  expect_equal(nrow(splice_series(A, B, base.period=as.Date("1991-12-01"))), 8);
}
)
