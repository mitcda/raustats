# raustats 0.20.1
---------------------------------------------------------------------

## Bug fixes:

* `abs_read_tss` modified function to fix errors in downloading ABS
   International Trade Price Indexes data sets.

* `rba_read_tss` now checks to include only named columns.

## Changes:

* `valid_url` added new internal function to ensure functions call only valid
  URLs.




# raustats 0.20.0
---------------------------------------------------------------------

* Major package revamp to handle changed ABS website schema and functionality


## Changes:

* `abs_cat_search` new function that enables users to query the ABS website
  search facilities.


## Bug fixes:

* `abs_read_tss` fixed missing column names in `names(metadata)` assign
   statement (which threw the following warning: The `value` argument of
   `names<-()` can't be empty as of tibble 3.0.0.) and moved file type check to
   new internal `is_abs_tss` function.




# raustats 0.15.1
---------------------------------------------------------------------

## Bug fixes:

* `abs_cat_tables` now lists all available publications and/or tables for some
  ABS Catalogue numbers for which the function previously failed.

* `abs_cat_stats` now avoids download errors causes by presence of all-NA
  columns in ABS data.


# raustats 0.15.0
---------------------------------------------------------------------

## Changes:

* `abs_cat_stats` includes new argument `na.rm` to provide option to remove rows
  with `NA` values.

* `abs_read_tss` includes new argument `na.rm` to provide option to remove rows
  with `NA` values.

* `abs_cat_download` now includes PDF files in set of downloadable ABS catalogue
  file types.

* `abs_cat_releases` a new function that returns the set of all available
  releases for a specified ABS catalogue number.

* `abs_cat_tables` includes internal changes that specify separate columns for
  Excel, Zip and PDF resource URLs.

* `abs_stats` includes new option `return_json` which enables return of data in
  raw JSON format.

* `rba_search` (and by extension `rba_stats`) now includes new option
  `series_type` which enables user to list only current *statistical tables*
  (the default), *historical data* or *discontinued data*.


## Bug fixes:

* `abs_cat_stats` now avoids multiple file downloads and applies `abs_cat_unzip`
  only to compressed files.

* `abs_cat_tables` includes revisions that correct errors thrown by
  `abs_cat_tables` and `abs_cat_stats` for some ABS catalogue numbers
  (e.g. 8731.0 and 3105.0.65.001).

* `abs_stats` now gracefully handles zero-length (empty) returns.

* `rba_stats` now downloads only current *statistical tables* by
  default. Previously, `rba_stats` would attempt to read all tables meeting
  search criteria, and fail in cases involving a mix of *statistical tables*,
  *historical data* and/or *discontinued data*. (Reported by David Stephan.)

* Added functionality to ensure URL calls fail gracefully with an informative
  message if the resource is not available.



# raustats 0.1.0
---------------------------------------------------------------------

* Initial package release
