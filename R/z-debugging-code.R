#' # Debugging code
#'
#' ## Debugging `abs_stats` function
#' 

#' ### Testing empty returns
## Regional Statistics by ASGS 2016
##  - Economy and Industry
##    + Number of businesses
abs_id <- abs_search("regional statistics") %>%
  filter(grepl("regional\\s*statistics.*asgs\\s*2016", name, ignore.case=TRUE));
abs_meta <- abs_metadata(abs_id$id);
abs_fltr <- abs_search("^total\\s*number.+business\\s*entries", dataset=abs_id$id, code_only=TRUE);

## Debugging settings
dataset <- "ABS_REGIONAL_ASGS2016"
filter <- list(MEASURE="CABEE_6",
               # MEASURE="CABEE_10",
               REGIONTYPE="STE",
               ASGS_2016=1:8);
start_date <- 2011
end_date <- 2018
dimensionAtObservation <- "AllDimensions"
detail <- "Full"
enforce_api_limits <- TRUE

