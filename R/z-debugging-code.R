#' # Debugging code
#'
#' ## Debugging `abs_stats` function
#' 

#' ### Testing empty returns
## Regional Statistics by ASGS 2016
##  - Economy and Industry
##    + Number of businesses
## library(magrittr)
## abs_id <- abs_search("regional statistics") %>%
##   filter(grepl("regional\\s*statistics.*asgs\\s*2016", name, ignore.case=TRUE));
## abs_meta <- abs_metadata(abs_id$id);
## abs_fltr <- abs_search("^total\\s*number.+business\\s*entries", dataset=abs_id$id, code_only=TRUE);

## ## Debugging settings
## dataset <- "ABS_REGIONAL_ASGS2016"
## filter <- list(MEASURE="CABEE_6",
##                # MEASURE="CABEE_10",
##                REGIONTYPE="STE",
##                ASGS_2016=1:8);
## start_date <- 2011
## end_date <- 2018
## dimensionAtObservation <- "AllDimensions"
## detail <- "Full"
## enforce_api_limits <- TRUE

if (FALSE) {
  abs_ds <- abs_search(pattern="ERP.*SA2.*ASGS\\s2016") %>% .[3,] %>%
    select(id) %>% unlist;
  
  abs_metadata(abs_ds)
  abs_dimensions(abs_ds)
  abs_search(dataset=abs_ds)
  
  ds_filter <- abs_search(pattern=c("^\\d{9}$"),
                          dataset=abs_ds, code_only=TRUE)
  
  erp_sa2_2015_2017 <- abs_stats(abs_ds,
                                 filter=ds_filter,
                                 start_date="2015", end_date="2017", return_url=TRUE);
  conn <- file(file.path("/tmp/Rtmpnf8xwG/", "abs-search.txt"));
  erp_sa2_2015_2017 %>% writeLines(con=conn);
  close(conn);
  
  ## Create filter 
  ds_filter <- abs_search(pattern=c("Estimated Resident Population", "Males|Females|Persons",
                                    "sa2","^\\d{1,9}$", "Jun-2017"),
                          dataset=abs_ds, code_only=TRUE)
  
  ## Download Jun 2017 ERP
  erp_st_age_sex_2017 <- abs_stats(abs_ds,
                                   filter=ds_filter,
                                   start_date="2017-Q2", end_date="2017-Q2");
}
