#' @name abs_metadata
#' @title Download dataset metadata from the ABS API
#' @description This function queries and returns all metadata associated with a specified dataset
#'   from ABS.Stat.
#' @importFrom xml2 xml_name xml_children xml_child xml_length xml_attrs xml_attr xml_ns_strip
#'   xml_text xml_find_all xml_parent
#' @param id ABS dataset ID.
#' @param lang Preferred language (default 'en' - English).
#' @return data frame in long format
#' @export
#' @family ABS.Stat functions
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     datasets <- abs_datasets();
#'     x <- abs_metadata("CPI");
#'     x <- abs_metadata(grep("cpi", datasets$id, ignore.case=TRUE, value=TRUE));
#'     names(x)
#'     y <- abs_metadata(datasets$id[1]);
#'     names(y)
#'   }
abs_metadata <- function(id, lang="en")
{
  ## Return xml document of ABS indicators
  url <- abs_api_call(path=abs_api_urls()$datastr_path, args=id);
  x <- abs_call_api(url);
  ##
  ## Return all codelists
  i_codelist <- grep("codelist", xml_name(xml_children(x)), ignore.case=TRUE);
  n_codelists <- xml_length(xml_child(x, i_codelist));
  ## Dataset dimensions and codes
  codelists_attrs <- as.data.frame(
    do.call(rbind,
            lapply(seq_len(n_codelists),
                   function(i)
                     xml_attrs(xml_child(xml_child(x, 2),i))
                   )),
    stringsAsFactors = FALSE); # <= Required for R (< 4.0.0)
  ## Codelist content
  codelists <- lapply(seq_len(n_codelists),
                      function(i) {
                        ## Note 'xml_ns_strip' essential to extracting Description
                        y <- xml_ns_strip(xml_child(xml_child(x, i_codelist), i));
                        codelist <- data.frame(
                          Code = xml_text(xml_find_all(xml_children(y), "@value")),
                          Description = xml_text(xml_find_all(y,
                                                              sprintf(".//Code//Description[@xml:lang='%s']",
                                                                      lang))),
                          stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
                      });
  ## Return components
  i_keyfamilies <- grep("keyfamilies", xml_name(xml_children(x)), ignore.case=TRUE);
  z <- xml_parent(xml_find_all(xml_children(xml_child(x, i_keyfamilies)),
                               ".//@codelist"));
  components <- data.frame(codes = xml_text(xml_find_all(z, ".//@codelist")),
                           conceptRef = xml_text(xml_find_all(z, ".//@conceptRef")),
                           type = xml_name(z),
                           stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
  ## Return concepts
  i_concepts <- grep("concepts", xml_name(xml_children(x)), ignore.case=TRUE);
  w <- xml_children(xml_child(x, i_concepts));
  concepts <- data.frame(concept = xml_attr(xml_find_all(w, "."), "id"),
                         agencyID = xml_attr(xml_find_all(w, "."), "agencyID"),
                         conceptRef=xml_text(xml_find_all(w, sprintf(".//Name[@xml:lang='%s']", lang))),
                         stringsAsFactors=FALSE); # <= Required for R (< 4.0.0)
  ## Set names/attributes
  names(codelists) <- components$codes;
  ## Add dataset and dataset_desc attributes
  attr(codelists, "concept") <- components$conceptRef;
  attr(codelists, "description") <- concepts$conceptRef[match(components$conceptRef, concepts$concept)];
  attr(codelists, "type") <- components$type;
  return(codelists);
}

## ----------------------------------- EOF ---------------------------------- ##
