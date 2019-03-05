## =========================================================================
## Filename:    
## Created: 
## Updated:     <2018-11-19 09:42:28 david at grover>
## Author:      
## Description: 
##              
##
## =========================================================================

#'
#'  Table - ABS ANA Series IDs and series names
#' 
#'  ABS Series ID  | Series abb    | Series name                                     
#' :---------------|:--------------|:--------------------------------------
#'  A2304402X      | gdp_cv_sa     | GDP Chain Volume measures: Seasonally Adjusted   
#'  A2304340C      | gdp_cv_tr     |           "              : Trend
#'  A2302459A      | gdp_cv_or     |           "              : Original
#'  A2304408L      | gdi_cv_sa     | Gross Domestic Income, Chain Volume measures: Seasonally Adjusted
#'  A2304342J      | gdi_cv_tr     |           "              : Trend
#'  A2302463T      | gdi_cv_or     |           "              : Original
#'  A2304412C      | gni_cv_sa     | GNI Chain Volume measures: Seasonally Adjusted
#'  A2304344L      | gni_cv_tr     |           "              : Trend
#'  A2302464V      | gni_cv_or     |           "              : Original
#'  A2304414J      | nndi_cv_sa    | NNDI Chain Volume measures: Seasonally Adjusted
#'  A2304346T      | nndi_cv_tr    |           "              : Trend
#'  A2302465W      | nndi_cv_or    |           "              : Original
#'  A2304404C      | gdppc_cv_sa   | GDP per capita Chain volume measures: Seasonally Adjusted
#'  A2304336L      | gdppc_cv_tr   |           "              : Trend
#'  A2302459A      | gdppc_cv_or   |           "              : Original
#'  A2304113C      | gne_cv_sa     | GNE Chain Volume measures: Seasonally Adjusted
#'  A2304237F      | gne_cv_tr     |           "              : Trend
#'  A2302514F      | gne_cv_or     |           "              : Original
#'  A2304111X      | dfd_cv_sa     | DFD Chain Volume measures: Seasonally Adjusted
#'  A2304235A      | dfd_cv_tr     |           "              : Trend
#'  A2302519T      | dfd_cv_or     |           "              : Original
#'  A2304114F      | exp_cv_sa     | Exports Chain Volume measures: Seasonally Adjusted
#'  A2304238J      | exp_cv_tr     |           "              : Trend
#'  A2302520A      | exp_cv_or     |           "              : Original
#'  A2304115J      | imp_cv_sa     | Imports Chain Volume measures: Seasonally Adjusted
#'  A2304239K      | imp_cv_sa     |           "              : Trend
#'  A2302521C      | imp_cv_sa     |           "              : Original
#'
#'  Notes
#'    GDI = GDP - ToT effects       (GDI - Gross Domestic Income)
#'    NNDI = ??                     (NNDI - Net National Disposable Income)
#'    DFD = GNE - Inventory change  (DFD - Domestic Final Demand)
#'
#' 

#### Add human-readable series model names (abbreviations)
## -- TO DO - INCLUDE IN ABS data package GENERAL FUNCTIONS
## ana_series_abb <- function(x) {
##   x %>%
##     ## Series abbreviations
##     mutate(series_abb = ifelse(grepl("^gross domestic product", data_item_description, ignore.case=TRUE),
##                                "gdp", "")) %>%
##     mutate(series_abb = ifelse(grepl("^gdp", data_item_description, ignore.case=TRUE),
##                                "gdp", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("^gross value added", data_item_description, ignore.case=TRUE),
##                                "gva", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("^net domestic product", data_item_description, ignore.case=TRUE),
##                                "ndp", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("^net domestic product", data_item_description, ignore.case=TRUE),
##                                "ndp", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("gross domestic income", data_item_description, ignore.case=TRUE),
##                                "gdi", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("gross national income", data_item_description, ignore.case=TRUE),
##                                "gni", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("net national disposable income", data_item_description, ignore.case=TRUE),
##                                "ndi", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("terms of trade", data_item_description, ignore.case=TRUE),
##                                "tot", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("gross national expenditure", data_item_description, ignore.case=TRUE),
##                                "gne", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("exports of goods and services", data_item_description, ignore.case=TRUE),
##                                "exp", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("imports of goods and services", data_item_description, ignore.case=TRUE),
##                                "imp", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("final consumption expenditure", data_item_description, ignore.case=TRUE),
##                                "fce", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("gross fixed capital formation", data_item_description, ignore.case=TRUE),
##                                "gfcf", series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("state final demand", data_item_description, ignore.case=TRUE),
##                                "sfd", series_abb)) %>%
##     ##
##     ## Per capita series 
##     mutate(series_abb = ifelse(grepl("per capita", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "pc"), series_abb)) %>%
##     ##
##     ## Households, government, private, public
##     mutate(series_abb = ifelse(grepl("general government", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_gov"), series_abb)) %>%
##     ## -- General government options
##     mutate(series_abb = ifelse(grepl("general government.+national",
##                                      data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_nat"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("general government.+national.+defence",
##                                      data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_def"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("general government.+national.+non-defence",
##                                      data_item_description, ignore.case=TRUE),
##                                sub("_def", "_ndf", series_abb), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("general government.+state and local",
##                                      data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_stl"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("households", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_hhld"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("all sectors", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_tot"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("private", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_priv"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("public", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_pub"), series_abb)) %>%
##     ##
##     ## States/territories
##     mutate(series_abb = ifelse(grepl("new south wales", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_nsw"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("victoria", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_vic"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("queensland", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_qld"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("south australia", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_sa"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("western australia", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_wa"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("tasmania", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_tas"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("northern territory", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_nt"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("australian capital territory", data_item_description, ignore.case=TRUE),
##                                paste0(series_abb, "_act"), series_abb)) %>%
##     ##
##     ## Chain volume/current prices
##     mutate(series_abb = ifelse(grepl("chain volume measures", data_item_description, ignore.case=TRUE) |
##                                grepl("chain volume measures", table_title, ignore.case=TRUE),
##                                paste0(series_abb, "_cv"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("current prices", data_item_description, ignore.case=TRUE) |
##                                grepl("current prices", table_title, ignore.case=TRUE),
##                                paste0(series_abb, "_cp"), series_abb)) %>%
##     ##
##     ## Original/seasonally adjusted/trend/index
##     mutate(series_abb = ifelse(grepl("original", series_type, ignore.case=TRUE),
##                                paste0(series_abb, "_or"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("seasonally adjusted", series_type, ignore.case=TRUE),
##                                paste0(series_abb, "_sa"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("trend", series_type, ignore.case=TRUE),
##                                paste0(series_abb, "_tr"), series_abb)) %>%
##     ##
##     ## Percentage change/ratio/index
##     mutate(series_abb = ifelse(grepl("percent", unit, ignore.case=TRUE),
##                                paste0(series_abb, "_pc"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("\\$.*million", unit, ignore.case=TRUE),
##                                paste0(series_abb, "_aud"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("index", unit, ignore.case=TRUE),
##                                paste0(series_abb, "_ix"), series_abb)) %>%
##     mutate(series_abb = ifelse(grepl("proportion", unit, ignore.case=TRUE),
##                                paste0(series_abb, "_rt"), series_abb))
## }


## =============================== EOF =====================================
