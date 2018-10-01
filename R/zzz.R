.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Attaching package: 'raustats'");
}

.onLoad <- function(libname, pkgname) {
    options(raustats=c(abs_domain = "http://www.abs.gov.au/",
                       abs_ausstats_path = "ausstats/abs@.nsf/mf",
                       abs_downloads_regex = "Downloads",
                       abs_releases_regex = "Past.*Future.*Releases",
                       rba_domain = "http://www.rba.gov.au/",
                       rba_stats_path = "statistics",
                       rba_tables_path = "tables"));
}

.onDetach <- function(libname, pkgname) {
    options(RecRep.options = NULL);
}

## .Last <- function()
## {
##   my.options <- options()$rausstats
##   save(my.options, file="~/.Rausstats.Rdata")
## }

## .First <- function()
## {
##   tryCatch({
##     load("~/.Rausstats.Rdata")
##     do.call(options, my.options)
##     rm(my.options)
##   }, error=function(...){})
## }

