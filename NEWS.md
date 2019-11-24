# raustats 0.1.0.9050
---------------------------------------------------------------------

## Bug fixes:
* Fixed bug in `abs_cat_stats` function to avoid downloading the files multiple
  times and apply `abs_cat_unzip` function only to compressed files.
* Silently handle zero-length (empty) returns in `abs_stats`.

## Changes:
* Added `na.rm` argument to `abs_cat_stats` and `abs_read_tss` functions.
* Added `abs_cat_releases` function.
* Internal changes to `abs_cat_tables` function clarifying column names for
  Excel, Zip and PDF file format URLs. 
* Added `return_json` argument to `abs_stats` function.
* Added OpenXml Spreadsheet files to list of valid ABS catalogue statistics file
  types.
* Added PDF files to valid list of downloadable ABS catalogue file types.



# raustats 0.1.0
---------------------------------------------------------------------

* Initial package release
