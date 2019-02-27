### =========================================================================
### Filename:     build-data.R
### Created:      2017-08-10
### Updated:      <2019-02-26 21:25:38 david at grover>
### Author:       David Mitchell <david.p.mitchell@homemail.com.au>
### Description:  Builds package data sets
### =========================================================================

######  Section 0 - Libraries & settings
## Nil

######  Section 1 - Import exports and imports table structure
devtools::load_all(".");
rba_tablecache <- rba_table_cache();
abs_tablecache <- read.csv(file.path("data-raw", "ABS-TSS-Catalogue-Numbers.csv"));
aus_state_codes <- read.csv(file.path("data-raw", "Australian-States-Territories.csv"));
abs_cachelist <- abs_datasets();
## abs_cachelist <- abs_cache(lang="en", progress=5);

## Write data sets files
usethis::use_data(rba_tablecache, overwrite=TRUE);
usethis::use_data(abs_tablecache, aus_state_codes, overwrite=TRUE);
usethis::use_data(abs_cachelist, overwrite=TRUE);

### =============================== EOF =====================================
