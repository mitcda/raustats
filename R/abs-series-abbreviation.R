## =========================================================================
## Filename:    
## Created: 
## Updated:     <2019-06-19 10:46:32 david at grover>
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

ana_series_abb <- function(x) {
  x %>%
    ## Table abbreviations
    mutate(series_abb =
             case_when(grepl("^key national accounts aggregates", table_title, ignore.case=TRUE)
                       ~ "ana",
                       grepl("^expenditure.+GDP", table_title, ignore.case=TRUE)
                       ~ "gdpe",
                       grepl("^income from.+GDP", table_title, ignore.case=TRUE)
                       ~ "gdpi",
                       grepl("^gross value added.+industry", table_title, ignore.case=TRUE)
                       ~ "gva",
                       grepl("^gross value added.+industry.+current price", table_title, ignore.case=TRUE)
                       ~ "gvacp",
                       grepl("^household.*final.*consumption.*expenditure", table_title, ignore.case=TRUE)
                       ~ "hfce",
                       TRUE ~ ""),
           ## Series abbreviations
           series_abb =
             paste0(series_abb,
                    case_when(grepl("^gross domestic product", data_item_description, ignore.case=TRUE)
                              ~ "_gdp",
                              grepl("^gdp", data_item_description, ignore.case=TRUE)
                              ~ "_gdp",
                              grepl("^gross value added", data_item_description, ignore.case=TRUE)
                              ~ "_gva",
                              grepl("^net domestic product", data_item_description, ignore.case=TRUE)
                              ~ "_ndp",
                              grepl("^net domestic product", data_item_description, ignore.case=TRUE)
                              ~ "_ndp",
                              grepl("gross domestic income", data_item_description, ignore.case=TRUE)
                              ~ "_gdi",
                              grepl("gross national income", data_item_description, ignore.case=TRUE)
                              ~ "_gni",
                              grepl("net national disposable income", data_item_description, ignore.case=TRUE)
                              ~ "_ndi",
                              grepl("terms of trade", data_item_description, ignore.case=TRUE)
                              ~ "_tot",
                              grepl("gross national expenditure", data_item_description, ignore.case=TRUE)
                              ~ "_gne",
                              grepl("exports of goods and services", data_item_description, ignore.case=TRUE)
                              ~ "_exp",
                              grepl("imports of goods and services", data_item_description, ignore.case=TRUE)
                              ~ "_imp",
                              grepl("domestic final demand", data_item_description, ignore.case=TRUE)
                              ~ "_dfd",
                              grepl("change.+inventories", data_item_description, ignore.case=TRUE)
                              ~ "_chinv",
                              grepl("final consumption expenditure", data_item_description, ignore.case=TRUE)
                              ~ "_fce",
                              grepl("gross fixed capital formation", data_item_description, ignore.case=TRUE)
                              ~ "_gfcf",
                              grepl("state final demand", data_item_description, ignore.case=TRUE)
                              ~ "_sfd",
                              grepl("hours worked market sector", data_item_description, ignore.case=TRUE)
                              ~ "_hrsmk",
                              grepl("hours worked", data_item_description, ignore.case=TRUE)
                              ~ "_hrstl",
                              grepl("real unit.*labour cost.*non.*farm", data_item_description, ignore.case=TRUE)
                              ~ "_rulcnf",
                              grepl("real unit.*labour cost.*", data_item_description, ignore.case=TRUE)
                              ~ "_rulc",
                              grepl("household saving ratio", data_item_description, ignore.case=TRUE)
                              ~ "_hsr",
                              grepl("net saving", data_item_description, ignore.case=TRUE)
                              ~ "_netsav",
                              grepl("statistical discrepancy", data_item_description, ignore.case=TRUE)
                              ~ "_statdis",
                              ## Industry gross value added
                              ## grepl(sprintf("\\(%s\\)\\s*;", paste(letters, collapse="|")),
                              ##       data_item_description, ignore.case=TRUE)
                              ## ~ sub(sprintf(".+\\((%s)\\)\\s*;.*", paste(letters, collapse="|")),
                              ##       tolower("_div\\1"), data_item_description, ignore.case=TRUE),
                              ## Division A - Agriculture, forestry & fishing
                              grepl("\\(a\\).+Agriculture", data_item_description, ignore.case=TRUE)
                              ~ "_diva_ag",
                              grepl("\\(a\\).+Forestry.*fishing", data_item_description, ignore.case=TRUE)
                              ~ "_diva_ff",
                              grepl("\\(a\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_diva_tot",
                              ## Division B - Mining
                              grepl("\\(b\\).+coal.*mining", data_item_description, ignore.case=TRUE)
                              ~ "_divb_cl",
                              grepl("\\(b\\).+oil.*gas", data_item_description, ignore.case=TRUE)
                              ~ "_divb_og",
                              grepl("\\(b\\).+iron.*ore", data_item_description, ignore.case=TRUE)
                              ~ "_divb_fe",
                              grepl("\\(b\\).+other.*mining", data_item_description, ignore.case=TRUE)
                              ~ "_divb_ot",
                              grepl("\\(b\\).+mining.*excluding.*exploration", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_divb_mn",
                              grepl("\\(b\\).+exploration.*support", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_divb_es",
                              grepl("\\(b\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divb_tot",
                              ## Division C - Manufacturing
                              grepl("\\(c\\).+food.*beverage", data_item_description, ignore.case=TRUE)
                              ~ "_divc_fb",
                              grepl("\\(c\\).+petroleum.*coal", data_item_description, ignore.case=TRUE)
                              ~ "_divc_pc",
                              grepl("\\(c\\).+metal.*products", data_item_description, ignore.case=TRUE)
                              ~ "_divc_mt",
                              grepl("\\(c\\).+machinery.*equipment", data_item_description, ignore.case=TRUE)
                              ~ "_divc_mc",
                              grepl("\\(c\\).+other.*manufacturing", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_divc_ot",
                              grepl("\\(c\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divc_tot",
                              ## Division D - Utilities
                              grepl("\\(d\\).+electricity", data_item_description, ignore.case=TRUE)
                              ~ "_divd_el",
                              grepl("\\(d\\).+gas", data_item_description, ignore.case=TRUE)
                              ~ "_divd_gs",
                              grepl("\\(d\\).+water.*supply", data_item_description, ignore.case=TRUE)
                              ~ "_divd_wt",
                              grepl("\\(d\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divd_tot",
                              ## Division E - Construction
                              grepl("\\(e\\).+building.*construction", data_item_description, ignore.case=TRUE)
                              ~ "_dive_bc",
                              grepl("\\(e\\).+civil.*engineering", data_item_description, ignore.case=TRUE)
                              ~ "_dive_ce",
                              grepl("\\(e\\).+construction.*services", data_item_description, ignore.case=TRUE)
                              ~ "_dive_cs",
                              grepl("\\(e\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_dive_tot",
                              ## Division F - Wholesale trade
                              grepl("\\(f\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divf_tot",
                              ## Division G - Retail trade
                              grepl("\\(g\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divg_tot",
                              ## Division H - Accommodation & food services
                              grepl("\\(h\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divh_tot",
                              ## Division I - Transport
                              grepl("\\(i\\).+road", data_item_description, ignore.case=TRUE)
                              ~ "_divi_rd",
                              grepl("\\(i\\).+air.*space", data_item_description, ignore.case=TRUE)
                              ~ "_divi_as",
                              grepl("\\(i\\).+rail.*pipeline", data_item_description, ignore.case=TRUE)
                              ~ "_divi_rl",
                              grepl("\\(i\\).+postal.*storage", data_item_description, ignore.case=TRUE)
                              ~ "_divi_ps",
                              grepl("\\(i\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divi_tot",
                              ## Division J - Telecommunications
                              grepl("\\(j\\).+telecommunications", data_item_description, ignore.case=TRUE)
                              ~ "_divj_tl",
                              grepl("\\(j\\).+other.*information", data_item_description, ignore.case=TRUE)
                              ~ "_divj_ot",
                              grepl("\\(j\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divj_tot",
                              ## Division K - Finance & insurance
                              grepl("\\(k\\).+finance", data_item_description, ignore.case=TRUE)
                              ~ "_divk_fn",
                              grepl("\\(k\\).+other.*financial", data_item_description, ignore.case=TRUE)
                              ~ "_divk_ot",
                              grepl("\\(k\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divk_tot",
                              ## Division L - Rental, hiring & real estate
                              grepl("\\(l\\).+rental.*hiring", data_item_description, ignore.case=TRUE)
                              ~ "_divl_rh",
                              grepl("\\(l\\).+real.*estate", data_item_description, ignore.case=TRUE)
                              ~ "_divl_re",
                              grepl("\\(l\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divl_tot",
                              ## Division M - Professional and scientific services
                              grepl("\\(m\\).+computer.*system", data_item_description, ignore.case=TRUE)
                              ~ "_divm_cs",
                              grepl("\\(m\\).+other.*professional", data_item_description, ignore.case=TRUE)
                              ~ "_divm_op",
                              grepl("\\(m\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divm_tot",
                              ## Division N - Administrative & support services
                              grepl("\\(n\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divn_tot",
                              ## Division O - Public administration & safety
                              grepl("\\(o\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divo_tot",
                              ## Division P - Education and training
                              grepl("\\(p\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divp_tot",
                              ## Division Q - Health care & social assistance
                              grepl("\\(q\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divq_tot",
                              ## Division R - Arts and recreation services
                              grepl("\\(r\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divr_tot",
                              ## Division S - Other services
                              grepl("\\(s\\).+;$", data_item_description, ignore.case=TRUE)
                              ~ "_divs_tot",
                              ## Ownership of dwellings
                              grepl("ownership.*dwellings", data_item_description, ignore.case=TRUE)
                              ~ "_dwell",
                              grepl("taxes less subsidies", data_item_description, ignore.case=TRUE)
                              ~ "_nettax",
                              grepl("gross value added at basi prices taxes less", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_nettax",
                              ## Household Final Consumption Expenditure items
                              grepl("Food", data_item_description, ignore.case=TRUE)
                              ~ "_01_food",
                              grepl("Alcoholic.*beverage.*cigarettes.*tobacco", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_02_albt",
                              grepl("Cigarettes.*tobacco", data_item_description, ignore.case=TRUE)
                              ~ "_02a_tabc",
                              grepl("Alcoholic.*beverage", data_item_description, ignore.case=TRUE)
                              ~ "_02b_abev",
                              grepl("Clothing.*footwear", data_item_description, ignore.case=TRUE)
                              ~ "_03_clft",
                              grepl("Housing.*water.*electricity.*gas", data_item_description, ignore.case=TRUE)
                              ~ "_04_hhsv",
                              grepl("Rent.*other.*dwelling.*services", data_item_description, ignore.case=TRUE)
                              ~ "_04a_rnts",
                              grepl("Actual.*imputed.*rent", data_item_description, ignore.case=TRUE)
                              ~ "_04b_rent",
                              grepl("Electricity.*gas.*other.*fuel", data_item_description, ignore.case=TRUE)
                              ~ "_04c_util",
                              grepl("Water.*sewerage.*charges", data_item_description, ignore.case=TRUE)
                              ~ "_04d_watr",
                              grepl("Furnishings.*household.*equip", data_item_description, ignore.case=TRUE)
                              ~ "_05_furn",
                              grepl("Furniture.*floor.*coverings", data_item_description, ignore.case=TRUE)
                              ~ "_05a_furn",
                              grepl("Household.*appliances", data_item_description, ignore.case=TRUE)
                              ~ "_05b_appl",
                              grepl("Household.*tools", data_item_description, ignore.case=TRUE)
                              ~ "_05c_tool",
                              grepl("Health", data_item_description, ignore.case=TRUE)
                              ~ "_06_hlth",
                              grepl("Medicines", data_item_description, ignore.case=TRUE)
                              ~ "_06a_hlth",
                              grepl("Total.*health.*services", data_item_description, ignore.case=TRUE)
                              ~ "_06b_hlth",
                              grepl("Purchase.*vehicles", data_item_description, ignore.case=TRUE)
                              ~ "_07a_vcpx",
                              grepl("Operation.*vehicles", data_item_description, ignore.case=TRUE)
                              ~ "_07b_vopx",
                              grepl("Transport.*services", data_item_description, ignore.case=TRUE)
                              ~ "_07c_tran",
                              grepl("Transport", data_item_description, ignore.case=TRUE)
                              ~ "_07_tran",
                              grepl("Communications", data_item_description, ignore.case=TRUE)
                              ~ "_08_comm",
                              grepl("Goods.*for.*recreation.*culture", data_item_description, ignore.case=TRUE)
                              ~ "_09a_recg",
                              grepl("^Recreational.*cultural.*services", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_09b_recs",
                              grepl("Sporting.*recreational.*cultural.*services", data_item_description,
                                    ignore.case=TRUE)
                              ~ "_09c_sprt",
                              grepl("Net.*losses.*gambling", data_item_description, ignore.case=TRUE)
                              ~ "_09d_gamb",
                              grepl("Newspapers.*books.*stationery", data_item_description, ignore.case=TRUE)
                              ~ "_09e_news",
                              grepl("Recreation.*culture", data_item_description, ignore.case=TRUE)
                              ~ "_09_recc",
                              grepl("Education.*services", data_item_description, ignore.case=TRUE)
                              ~ "_10_educ",
                              grepl("Hotels.*cafes.*restaurants", data_item_description, ignore.case=TRUE)
                              ~ "_11_acrs",
                              grepl("Catering.*services", data_item_description, ignore.case=TRUE)
                              ~ "_11a_cats",
                              grepl("Accommodation.*services", data_item_description, ignore.case=TRUE)
                              ~ "_11b_accs",
                              grepl("Miscellaneous.*goods.*services", data_item_description, ignore.case=TRUE)
                              ~ "_12_misc",
                              grepl("Other.*goods", data_item_description, ignore.case=TRUE)
                              ~ "_12a_othg",
                              grepl("Insurance.*financial.*services", data_item_description, ignore.case=TRUE)
                              ~ "_12b_fins",
                              grepl("Other.*services", data_item_description, ignore.case=TRUE)
                              ~ "_12c_oths",
                              grepl("Net.*expenditure.*overseas", data_item_description, ignore.case=TRUE)
                              ~ "_neo",
                              grepl("Final.*consumption.*expenditure", data_item_description, ignore.case=TRUE)
                              ~ "_totc",
                              TRUE ~ "")),
           ##
           ## Per capita/hour worked series 
           series_abb = paste0(series_abb,
                               case_when(grepl("per capita", data_item_description, ignore.case=TRUE)
                                         ~ "pc",
                                         grepl("per hour", data_item_description, ignore.case=TRUE)
                                         ~ "ph",
                                         TRUE ~ "")),
           ##
           ## Households, government, private, public
           series_abb = paste0(series_abb,
                               case_when(grepl("general government", data_item_description, ignore.case=TRUE)
                                         ~ "_gov",
                                         TRUE ~ "")),
           ## -- General government options
           series_abb = paste0(series_abb,
                               case_when(grepl("general government",
                                               data_item_description, ignore.case=TRUE) ~ "_gov",
                                         grepl("general government.+national",
                                               data_item_description, ignore.case=TRUE) ~ "_nat",
                                         grepl("general government.+national.+non-defence",
                                               data_item_description, ignore.case=TRUE) ~ "_ndf",
                                         grepl("general government.+national.+defence",
                                               data_item_description, ignore.case=TRUE) ~ "_def",
                                         grepl("general government.+state and local",
                                               data_item_description, ignore.case=TRUE) ~ "_stl",
                                         grepl("households",
                                               data_item_description, ignore.case=TRUE) ~ "_hhld",
                                         grepl("all sectors",
                                               data_item_description, ignore.case=TRUE) ~ "_tot",
                                         grepl("private",
                                               data_item_description, ignore.case=TRUE) ~  "_priv",
                                         grepl("public", data_item_description, ignore.case=TRUE)
                                         ~  "_pub",
                                         TRUE ~ "")),
           ##
           ## States/territories
           series_abb = paste0(series_abb,
                               case_when(grepl("new south wales",
                                               data_item_description, ignore.case=TRUE)
                                         ~ "_nsw",
                                         grepl("victoria", data_item_description, ignore.case=TRUE)
                                         ~ "_vic",
                                         grepl("queensland", data_item_description, ignore.case=TRUE)
                                         ~ "_qld",
                                         grepl("south australia", data_item_description, ignore.case=TRUE)
                                         ~ "_sa",
                                         grepl("western australia", data_item_description, ignore.case=TRUE)
                                         ~ "_wa",
                                         grepl("tasmania", data_item_description, ignore.case=TRUE)
                                         ~ "_tas",
                                         grepl("northern territory", data_item_description, ignore.case=TRUE)
                                         ~ "_nt",
                                         grepl("australian capital territory", data_item_description, ignore.case=TRUE)
                                         ~ "_act",
                                         TRUE ~ "")),
           ##
           ## Chain volume/current prices
           series_abb = paste0(series_abb,
                               case_when(grepl("chain volume measures", data_item_description, ignore.case=TRUE) |
                                         grepl("chain volume measures", table_title, ignore.case=TRUE)
                                         ~ "_cv",
                                         grepl("current prices", data_item_description, ignore.case=TRUE) |
                                         grepl("current prices", table_title, ignore.case=TRUE)
                                         ~ "_cp",
                                         grepl("price indexes", data_item_description, ignore.case=TRUE) |
                                         grepl("price indexes", table_title, ignore.case=TRUE)
                                         ~ "_ix",
                                         grepl("implicit price deflators", data_item_description, ignore.case=TRUE) |
                                         grepl("implicit price deflators", table_title, ignore.case=TRUE)
                                         ~ "_pd",
                                         TRUE ~ "")),
           ##
           ## Original/seasonally adjusted/trend/index 
           series_abb = paste0(series_abb,
                               case_when(grepl("original", series_type, ignore.case=TRUE)
                                         ~ "_or",
                                         grepl("seasonally adjusted", series_type, ignore.case=TRUE)
                                         ~ "_sa",
                                         grepl("trend", series_type, ignore.case=TRUE)
                                         ~ "_tr",
                                         TRUE ~ "")),
           ##
           ## Percentage change/ratio/index
           series_abb = paste0(series_abb,
                               case_when(grepl("percent", unit, ignore.case=TRUE)
                                         ~ "_pc",
                                         grepl("\\$.*(million)*", unit, ignore.case=TRUE)
                                         ~ "_aud",
                                         grepl("index", unit, ignore.case=TRUE)
                                         ~ "_ix",
                                         grepl("proportion", unit, ignore.case=TRUE)
                                         ~ "_rt",
                                         TRUE ~ ""))
           );
}

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




## ppi_series_abb <- function(x) {
##   x %>%
##     ## Publication abbreviations
##     mutate(series_abb = ifelse(grepl("^producer price indexes", publication_title, ignore.case=TRUE),
##                                "ppi", "")) %>%
##     ## Sector abbreviations
##     mutate(series_abb = ifelse(grepl("transport.+warehousing", table_title, ignore.case=TRUE),
##                                paste0(series_abb, "_tr"), series_abb)) %>%
##     ## Series abbreviations
##     mutate(series_abb =
##              paste0(series_abb,
##                     case_when(grepl("road freight", data_item_description, ignore.case=TRUE) ~ "_rdfrt",
##                               grepl("urban bus", data_item_description, ignore.case=TRUE) ~ "_ubus",
##                               grepl("taxi", data_item_description, ignore.case=TRUE) ~ "_taxi",
##                               grepl("rail freight", data_item_description, ignore.case=TRUE) ~ "_rlfrt",
##                               grepl("water freight", data_item_description, ignore.case=TRUE) ~ "_wtfrt",
##                               grepl("pipeline", data_item_description, ignore.case=TRUE) ~ "_pipe",
##                               grepl("postal and courier", data_item_description, ignore.case=TRUE) ~ "_pstl",
##                               grepl("courier pick-up", data_item_description, ignore.case=TRUE) ~ "_cour",
##                               grepl("water transport support", data_item_description, ignore.case=TRUE) ~ "_wtspt",
##                               grepl("stevedoring", data_item_description, ignore.case=TRUE) ~ "_wtstv",
##                               grepl("port and water transport", data_item_description, ignore.case=TRUE) ~ "_wtprt",
##                               grepl("other water", data_item_description, ignore.case=TRUE) ~ "_wtoth",
##                               grepl("airport operations", data_item_description, ignore.case=TRUE) ~ "_arprt",
##                               grepl("customs agency", data_item_description, ignore.case=TRUE) ~ "_svcust",
##                               grepl("warehousing and storage", data_item_description, ignore.case=TRUE) ~ "_whgen",
##                               grepl("grain storage", data_item_description, ignore.case=TRUE) ~ "_whgrn",
##                               grepl("other warehousing", data_item_description, ignore.case=TRUE) ~ "_whoth",
##                               TRUE ~ ""))
##            ) %>%
##     ## Percentage change/ratio/index
##     mutate(series_abb =
##              paste0(series_abb,
##                     case_when(grepl("percent", unit, ignore.case=TRUE) ~ "_pc",
##                               grepl("\\$.*million", unit, ignore.case=TRUE) ~ "_aud",
##                               grepl("index", unit, ignore.case=TRUE) ~ "_ix",
##                               grepl("proportion", unit, ignore.case=TRUE) ~ "_rt",
##                               TRUE ~ ""))
##            );
## }


## =============================== EOF =====================================
