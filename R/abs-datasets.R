#' @name abs_datasets
#' @title Download ABS.Stat datasets
#' @description This function returns a list of all datasets available from ABS.Stat.
#' @importFrom xml2 as_list read_xml read_html xml_name xml_find_all
#' @param lang Preferred language (default 'en' - English).
#' @param include_notes Include ABS annotation information for each series.
#' @return data frame in long format
#' @export
#' @family ABS.Stat functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     datasets <- abs_datasets()
#'     datasets <- abs_datasets(include_notes=TRUE)
#'   }
abs_datasets <- function(lang="en", include_notes=FALSE)
{
  ## Return xml document of ABS indicators
  url <- abs_api_call(path=abs_api_urls()$datastr_path, args="all");
  x <- abs_call_api(url);
  ## Select node name for 
  no_ids <- table(xml_name(xml_find_all(x, "//*[@id]")));
  series_node_name <- names(no_ids[no_ids == max(no_ids)])
  ## Extract Series ID information
  xpath_str <- sprintf("//*[name() = '%s']", series_node_name);
  name_fld <- "Name"
  ## The following code extracts the relevant ABS series information from the returned
  ## XML document by first saving the relevant part of the XML document to an R list and
  ## then explicitly extracting the relevant information from specific nodes by name.
  ## A more general recursive process, impervious to name changes would be preferred,
  ## however, it is more complex than simply revising the following code in response to
  ## potential future server-side changes.
  y <- as_list(xml_find_all(x, xpath_str));
  z <- lapply(y,
              function(m)
                list(agencyID = attr(m, "agencyID"),
                     id = attr(m, "id"),
                     name = unlist(
                         if (length(m[names(m) == name_fld]) == 1) {
                           m[[name_fld]]
                         } else {
                           m[names(m) == name_fld][sapply(m[names(m) == name_fld],
                                                        function(p) attributes(p)) == lang]
                         }),
                     notes = paste(unlist(m$Annotations), collapse=": "))
                  );
  z <- as.data.frame(do.call(rbind, z));
  z <- z[, c("id","agencyID","name","notes")];
  if (!include_notes)
    z <- z[, c("id","agencyID","name")];
  return(z)
}

## ----------------------------------- EOF ---------------------------------- ##
