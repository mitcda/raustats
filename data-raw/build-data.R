### =========================================================================
### Filename:     build-data.R
### Created:      2017-08-10
### Updated:      <2019-03-06 09:55:33 david at grover>
### Author:       David Mitchell <david.p.mitchell@homemail.com.au>
### Description:  Builds package data sets
### =========================================================================

######  Section 0 - Libraries & settings
## Nil

######  Section 1 - Import exports and imports table structure
devtools::load_all(".");
rba_cachelist <- rba_table_cache();
abs_cat_cachelist <- read.csv(file.path("data-raw", "ABS-TSS-Catalogue-Numbers.csv"));
abs_cachelist <- abs_datasets();
aus_state_codes <- read.csv(file.path("data-raw", "Australian-States-Territories.csv"));

## Write data sets files
usethis::use_data(rba_cachelist, overwrite=TRUE);
usethis::use_data(abs_cat_cachelist, overwrite=TRUE);
usethis::use_data(abs_cachelist, overwrite=TRUE);
usethis::use_data(aus_state_codes, overwrite=TRUE);

### =============================== EOF =====================================
