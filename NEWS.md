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
