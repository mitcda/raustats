.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("Attaching package: 'raustats'");
}

.onDetach <- function(libname, pkgname)
{
  options(RecRep.options = NULL);
}

.Last <- function()
{
  my.options <- options()$rausstats
  save(my.options, file="~/.Rausstats.Rdata")
}

.First <- function()
{
  tryCatch({
    load("~/.Rausstats.Rdata")
    do.call(options, my.options)
    rm(my.options)
  }, error=function(...){})
}

