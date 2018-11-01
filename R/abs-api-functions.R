### ABS API functions

## @name abs_api_urls
## @title URL chunks to be used in API calls
## @description This function is called inside other functions in this package.
## @return a list with a base url and a url section for formatting the JSON API calls
## @author David Mitchell <david.mitchell@@infrastructure.gov.au>
abs_api_urls <- function()
{
  base_url <- "http://stat.data.abs.gov.au"
  datastr_path <- "restsdmx/sdmx.ashx/GetDataStructure"
  sdmx_json_path <- "SDMX-JSON/data"
  url_list <- list(base_url = base_url,
                   datastr_path = datastr_path,
                   sdmx_json_path = sdmx_json_path);
  return(url_list);
}


## @name abs_api_call
## @title Download updated indicator information from the ABS API
## @description TBC
## @importFrom xml2 read_xml read_html as_list
## @param path Character vector specifying one or more ABS collections or catalogue numbers to
##   download.
## @param args Named list of arguments to supply to call.
## @return data frame in long format
## @author David Mitchell <david.mitchell@@infrastructure.gov.au>
abs_api_call <- function(path, args)
{
    if (missing(path))
      stop("path missing.")

    if (missing(args))
      stop("Argument path missing.")

    url <- file.path(abs_api_urls()$base_url, path, args)

    return(url);
}


## @name abs_call_api
## @title Download specified URL
## @description TBC
## @importFrom xml2 read_xml
## @param url Character vector specifying one or more ABS collections or catalogue numbers to
##   download.
## @param args Named list of arguments to supply to call.
## @return data frame in long format
## @author David Mitchell <david.mitchell@@infrastructure.gov.au>
abs_call_api <- function(url)
{
  x <- xml2::read_xml(url);
  return(x);
}


#' @name abs_datasets
#' @title Download updated data series information from the ABS API
#' @description TBC
#' @importFrom xml2 read_xml read_html
#' @param lang Preferred language (default 'en' - English).
#' @param include_notes Include ABS annotation information for each series.
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'   datasets <- abs_datasets()
#'   datasets <- abs_datasets(include_notes=TRUE)
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
  #' The following code extracts the relevant ABS series information from the returned
  #' XML document by first saving the relevant part of the XML document to an R list and
  #' then explicitly extracting the relevant information from specific nodes by name.
  #' A more general recursive process, impervious to name changes would be preferred,
  #' however, it is more complex than simply revising the following code in response to
  #' potential future server-side changes.
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
  if (!include_notes)
    z <- z[,c("id","agencyID","name")];
  return(z)
}


#' @name abs_metadata
#' @title Download updated data series information from the ABS API
#' @description TBC
#' @import xml2
#' @param id ABS dataset ID.
#' @param lang Preferred language (default 'en' - English).
#' @param include_notes Include ABS annotation information for each series.
#' @return data frame in long format
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'   datasets <- abs_datasets();
#'   x <- abs_metadata("CPI");
#'   x <- abs_metadata(grep("cpi", datasets$id, ignore.case=TRUE, value=TRUE));
#'   names(x)
#'   y <- abs_metadata(datasets$id[1]);
#'   names(y)
abs_metadata <- function(id, lang="en")
{
  ## Return xml document of ABS indicators
  url <- abs_api_call(path=abs_api_urls()$datastr_path, args=id);
  x <- abs_call_api(url);

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
    stringsAsFactors = FALSE);
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
                          stringsAsFactors=FALSE);
                      });
  ## Return components
  i_keyfamilies <- grep("keyfamilies", xml_name(xml_children(x)), ignore.case=TRUE);
  z <- xml_parent(xml_find_all(xml_children(xml_child(x, i_keyfamilies)),
                               ".//@codelist"));
  components <- data.frame(codes = xml_text(xml_find_all(z, ".//@codelist")),
                           conceptRef = xml_text(xml_find_all(z, ".//@conceptRef")),
                           type = xml_name(z),
                           stringsAsFactors=FALSE);
  ## Return concepts
  i_concepts <- grep("concepts", xml_name(xml_children(x)), ignore.case=TRUE);
  w <- xml_children(xml_child(x, i_concepts));
  concepts <- data.frame(concept = xml_attr(xml_find_all(w, "."), "id"),
                         agencyID = xml_attr(xml_find_all(w, "."), "agencyID"),
                         conceptRef=xml_text(xml_find_all(w, sprintf(".//Name[@xml:lang='%s']", lang))),
                         stringsAsFactors=FALSE);
  ## Set names/attributes
  names(codelists) <- components$codes;
  ## Add dataset and dataset_desc attributes
  attr(codelists, "concept") <- components$conceptRef;
  attr(codelists, "description") <- concepts$conceptRef[match(components$conceptRef, concepts$concept)];
  attr(codelists, "type") <- components$type;
  return(codelists);
}


#' @name abs_cache
#' @title Download updated list of datasets and dimensions information from the ABS API
#' @description TBC
#' @param lang Language in which to return the results. If \code{lang} is unspecified, English
#'   ('en') is the default.
#' @param progress Report download progress. Arguments accepts integer, logical or NULL. Set
#'   \code{progress} to \code{NULL} (default) to disable progress
#'   reporting. Otherwise set progress equal to integer value frequency.
#' 
#' @return A list of available ABS data series each comprising a list of available data dimensions,
#'   typically containing:
#'   \itemize{
#'     \item \code{MEASURE}: Measurement units (e.g. Persons, $ million, Index, Percentage change, etc.)
#'     \item \code{REGION}: Australian region name
#'     \item \code{INDEX}: Data item code and description
#'     \item \code{TSEST}: Time series estimate type (e.g. Original, Seasonally Adjusted, etc.)
#'     \item \code{FREQUENCY}: Available data frequency (Monthly, Quarterly, Annual)
#'     \item \code{TIME}: Available observation period index
#'     \item \code{OBS_STATUS}: Observation status notes code and description
#'       (e.g. 'r' - revised, 'q' - not available, 'u' - not applicable)
#'     \item \code{TIME_FORMAT}: Available time format (e.g. Annual, Quarterly, Monthly, Daily).
#'   }
#' 
#' @note Saving the results of this function and using it as the cache parameter in \code{abs_stats}
#'   and \code{abs_search} replaces the default cached version \code{abs_cachelist} that comes with
#'   the package. Note, however, that this function can take a long time to extract metadata for all
#'   ABS datasets (e.g. approximately 20 minutes for 400 data sets), so use sparingly. For this
#'   reason, we also recommend specifying a progress update using the \code{progress} argument
#'   (default: 10).

#'   Not all data returns have support for languages other than english. If the specific
#'   return does not support your requested language by default it will return NA. The options for
#'   \code{lang} on the ABS API are presently:
#'   \itemize{
#'     \item en: English
#'     \item fr: French
#'   }
#' 
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'  \donotrun{
#'    z <- abs_cache(lang='en', progress=5)
#'  }
abs_cache <- function(lang="en", progress=10)
{
  ## DEBUG <- FALSE
  ## if (DEBUG) {
  ##   library(xml2); library(rvest);
  ##   lang="en"
  ##   progress <- 5
  ##   limit <- 15
  ##   x <-  abs_datasets(lang=lang)[21:25,];
  ## }
  ## TEST <- FALSE
  ## if (TEST) {
  ##   abs_cachelist <- abs_cache(progress=5)
  ## }
  x <- abs_datasets(lang=lang)
  if ( !is.null(progress) ) {
    t0 <- proc.time();
    i_report <- unique(c(seq(progress, nrow(x), by=progress), nrow(x)));
  }
  z <- lapply(seq_len(nrow(x)),
              function(i) {
                ## Download metadata
                y <- abs_metadata(x$id[i], lang=lang);
                ## Add dataset id & name information as attributes
                attr(y, "dataset") <- x$id[i];
                attr(y, "agency") <- x$agencyID[i];
                attr(y, "dataset_desc") <- x$name[i];
                ## Report progress
                if (!is.null(progress))
                  if (i %in% i_report)
                    cat(sprintf("Retrieved metadata for %d (of %d) datasets. Total time: %.2f",
                                i, nrow(x), (proc.time() - t0)["elapsed"]), "\n");
                return(y)
              });
  names(z) <- x$id;
  return(z);
}


#' @name abs_cachelist2table
#' @title Converts an abs_cachelist to abs_cachetable
#' @description This function converts an \code{abs_cachelist} to an \code{abs_cachetable} suitable
#'   for use with \code{\link{abs_search}}.
#' @param cache An existing cachelist of available ABS datasets created by \code{abs_cachelist}. If
#'   \code{NULL}, uses the stored package cachelist.
#'
#' @return A table containing three columns:
#'   \itemize{
#'     \item \code{dataset}: ABS API dataset identifier.
#'     \item \code{dataset_description}: ABS API dataset description.
#'     \item \code{measure}: ABS API dataset measure identifier.
#'     \item \code{measure_description}: ABS API dataset measure description
#'   }
#' 
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @note This is an internal library function and is not exported.
#' @examples
#'  \donotrun{
#'    abs_ct <- abs_cachelist2table(raustats::abs_cachelist)
#'  }
abs_cachelist2table <- function(cache)
{
  if (missing(cache)) 
    cache <- raustats::abs_cachelist;
  cache_table <-
    suppressWarnings(lapply(cache,
                            function(x) {
                              names(x) <- attr(x, "concept");
                              y <- setNames(
                                data.frame(attr(x, "dataset"),
                                           attr(x, "dataset_desc")##,
                                           ## if(is.null(x$MEASURE$Code)) "" else x$MEASURE$Code,
                                           ## if(is.null(x$MEASURE$Description)) "" else x$MEASURE$Description,
                                           ## if(is.null(x$INDEX$Code)) "" else x$INDEX$Code,
                                           ## if(is.null(x$INDEX$Description)) "" else x$INDEX$Description
                                           ),
                                c("dataset","dataset_description"##,
                                  ## "measure","measure_description",
                                  ## "index","index_description"
                                  ));
                              return(y)
                            })
                     );
  cache_table <- do.call(rbind, cache_table);
  row.names(cache_table) <- seq_len(nrow(cache_table))
  return(cache_table);
}


#' @name abs_search
#' @title Search dataset information from the ABS API
#' @description This function finds datasets that match a search term and returns a data
#'   frame of matching results.
#' @param pattern Character string or regular expression to be matched.
#' @param fields Character vector of column names through which to search.
#' @param extra If FALSE, only the indicator ID and short name are returned, if TRUE, all columns of
#'   the cache parameter's indicator data frame are returned.
#' @param cache List of data frames returned from \code{abs_cache}. If omitted, \code{abs_cachelist}
#'   is used.
#' @return A data frame with datasets and data items that match the search pattern.
#' @export
#' @note With acknowledgements to \code{wb_search} function.
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'   x <- abs_search(pattern = "consumer price index")
#'   x <- abs_search(pattern = "census")
#'   x <- abs_search(pattern = "labour force")
#'   # with regular expression operators
#'   # 'unemployment' OR 'employment'
#'   x <- abs_search(pattern = "unemployment|employment")
#'   # '^labour force$'
#'   x <- abs_search(pattern = "^labour force$")
abs_search <- function(pattern,
                       fields=c("dataset", "dataset_description"),
                       extra=FALSE, cache)
{
  DEBUG <- FALSE
  if (DEBUG) {
    pattern <- "consumer price index"
    fields <- c("dataset", "dataset_description")
    extra <- FALSE
    load(file.path("data", "abs_cachelist.rda"))
    cache <- abs_cachelist
  }
  if (missing(cache)) 
    cache <- raustats::abs_cachelist;
  cache_table <- abs_cachelist2table(cache);
  ## -- UP TO HERE --
  ##  CHECK FUNCTION
  ## NOT REQUIRED FOR abs_search:  ind_cache <- cache$indicators;
  match_index <- sapply(fields,
                        function(i) grep(pattern, 
                                         cache_table[, i], ignore.case=TRUE),
                        USE.NAMES = FALSE);
  match_index <- sort(unique(unlist(match_index)));
  if (length(match_index) == 0)
    warning(sprintf("No matches were found for the search term %s. Returning an empty data frame.", 
                    pattern));
  if (extra) {
    match_df <- unique(cache_table[match_index, ])
  } else {
    match_df <- unique(cache_table[match_index, c("dataset", "dataset_description")])
  }
  return(match_df);
}


#' @name abs_dimensions
#' @title Return available dimensions of ABS series
#' @description This function returns the available dimeninsions for a specified ABS API dataset.
#' @param dataset Character vector of dataset codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}.
#' @param cache An existing cachelist of available ABS datasets created by \code{abs_cachelist}. If
#'   \code{NULL}, uses the stored package cachelist.
#' @return a data frame with available dataset dimensions.
#' @note @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'   # CPI - Consumer Price Index
#'   x <- abs_dimensions("CPI");
#'   # LF - Labour Force
#'   x <- abs_dimensions("LF");
abs_dimensions <- function(dataset, cache)
{
  ## DEBUG <- FALSE
  ## if (DEBUG) {
  ##   library(xml2); library(rvest);
  ##   dataset <- "CPI";
  ##   lang <- "en";
  ##   metadata <- abs_metadata("CPI")
  ##       metadata <- abs_metadata("ABS_ERP_LGA2017")
  ## }
  ## Check dataset present and valid 
  if (missing(dataset))
    stop("No dataset supplied.");
  if (!dataset %in% abs_datasets()$id)
    stop(sprintf("%s not valid dataset name.", dataset));
  ## Return metadata
  if (missing(cache)) {
    metadata <- raustats::abs_cachelist[[dataset]];
  } else {
    metadata <- cache[[dataset]];
  }
  ## Return data frame of dataset dimensions:
  ## z <- attr(metadata, "concept")[grepl("^dimension$", attr(metadata, "type"), ignore.case=TRUE)]
  z <- data.frame(name = attr(metadata, "concept"),
                  type = attr(metadata, "type"));
  return(z)
}


#' @name abs_stats
#' @title Download data from the ABS API
#' @description This function downloads the specified ABS data series from the ABS API.
#' @importFrom xml2 read_xml read_html
#' @importFrom jsonlite fromJSON
#' @param dataset Character vector of indicator codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}.
#' @param filter A list that contains filter of dimensions available in the specified \code{series}
#'   to use in the API call. If NULL, no filter is set and the query tries to return all dimensions
#'   of the dataset. Valid dimensions to include in the list supplied to filter include: MEASURE,
#'   REGION, INDEX, TSEST and FREQUENCY.
#' @param start_date Numeric or character. If numeric it must be in %Y form (i.e. four digit
#'   year). For data at the sub-annual granularity the API supports a format as follows: Monthly
#'   data -- "2016-M01", Quarterly data -- "2016-Q1", Semi-annual data -- 2016-B2, Financial year
#'   data -- 2016-17.
#' @param end_date Numeric or character (refer to \code{startdate}).
#' @param lang Language in which to return the results. If \code{lang} is unspecified, english is
#'   the default.
#' @param check_query If \code{TRUE} then the query length and estimated number of observations
#'   returned are first checked to ensure they're within the ABS-specified API limits (see
#'   \link{Notes} section).
#' @param remove_na If \code{TRUE}, remove blank or NA observations. If \code{FALSE}, no blank or NA
#'   values are removed from the return.
#' @param include_dec 
#' @param include_unit If \code{TRUE}, the column unit is not removed from the return. If
#'   \code{FALSE}, this column is removed.
#' @param include_obsStatus If \code{TRUE}, the column obsStatus is not removed from the return. If
#'   \code{FALSE}, this column is removed= FALSE.
#' @param include_lastUpdated = FALSEtables A character vector of regular expressions denoting
#'   tables to download. The default ('All') downloads all time series spreadsheet tables for each
#'   specified catalogue. Use a list to specify different table sets for each specified ABS
#'   catalogue number.
#' @param return_url If \code{TRUE}, return the generated request URL only.
#' @param cache An existing cachelist of available ABS datasets created by \code{abs_cachelist}. If
#'   \code{NULL}, uses the stored package cachelist.
#' @return Returns a data frame of the selected series from specified ABS dataset.
#' @note The data query submitted by this function uses the ABS RESTful API based on the SDMX-JSON
#'   standard. It has a maximum allowable character limit of 1000 characters allowed in the data
#'   URL.
#'
#'   Further limitations known at this time include:
#'   \itemize{
#'     \item Only anonymous queries are supported, there is no authentication
#'     \item Each response is limited to 1 million observations
#'     \item Errors are not returned in the JSON format but HTTP status codes and messages are
#'       set according to the Web Services Guidelines
#'     \item The lastNObservations parameter is not supported
#'     \item Observations follow the time series (or import-specific) order even if
#'       \code{dimensionAtObservation=AllDimensions} is used.
#'   }
#' 
#' @export
#' @author David Mitchell <david.mitchell@@infrastructure.gov.au>
#' @examples
#'    x <- abs_stats("CPI");
#'    x <- abs_stats(dataset="CPI", filter="all", return_url=TRUE);
#'    x <- abs_stats(dataset="CPI", filter = list(MEASURE=1, REGION=c(1:8,50), INDEX=10001, TSEST=10, FREQUENCY="Q"))
abs_stats <- function(dataset, filter, start_date, end_date, lang=c("en","fr"),
                      remove_na=TRUE, include_obsStatus=FALSE,
                      dimensionAtObservation=c("AllDimensions","TimeDimension","MeasureDimension"),
                      detail=c("Full","DataOnly","SeriesKeysOnly","NoData"),
                      return_url=FALSE, cache)
{
  DEBUG <- FALSE
  if (DEBUG) {
    library(xml2); library(rvest); library(jsonlite);
    dataset <- "CPI";
    lang <- "en";
    metadata <- abs_metadata("CPI")
    filter <- "all"
    filter <- list(MEASURE=c(1), REGION=c(1:8,50), INDEX=c(10001), TSEST=10, FREQUENCY="Q")
    filter <- list(MEASURE=c(1), INDEX=c(10001), TSEST=10, FREQUENCY="Q")
    filter <- list(MEASURE=c(1), INDEX=c(10001), REGION=c(1:8,50), FREQUENCY="Q", TSEST=10)
    load(file.path("data", "abs_cachelist.rda"));
    cache <- abs_cachelist
    dimensionAtObservation <- "AllDimensions"
    detail <- "Full"
  }
  ## Check dataset present and valid 
  if (missing(dataset))
    stop("No dataset supplied.");
  if (!dataset %in% abs_datasets()$id)
    stop(sprintf("%s not a valid ABS dataset.", dataset));
  ## Check if filter provided
  if (missing(filter))
    stop(sprintf("filter argument not supplied. Needs to be a valid list with following dimensions: %s",
                 paste(metadata_dims, collapse=", ")));
  ## Return metadata
  if (missing(cache)) {
    metadata <- raustats::abs_cachelist[[dataset]];
  } else {
    metadata <- cache[[dataset]];
  }
  ## Get list of Dimension name:
  metadata_names <- abs_dimensions(dataset)
  metadata_dims <- as.character(metadata_names[grepl("^dimension$", metadata_names$type, ignore.case=TRUE),
                                               "name"]);
  #### ## Return agency name
  #### agency_name <- unlist(attr(cache[[dataset]], "agency"));
  ## Check the  dimensions supplied in 'filter''
  if (filter == "all") {
    .filter <- metadata;
    names(.filter) <- metadata_names$name;
    filter <- lapply(.filter,
                     function(x) x$Code ## x[,1]
                     )
    filter <- filter[names(filter) %in% metadata_dims]
  } else if (class(filter) == "list") {
    if (any(!metadata_dims %in% names(filter)))
      stop(sprintf("Following dataset dimensions not listed in filter: %s",
                   paste(metadata_dims[!metadata_dims %in% names(filter)], collapse=", ")));
    filter <- filter[metadata_dims];
    for (name in names(filter)) {
      if(grepl("all", filter[[name]], ignore.case=TRUE))
        filter[[name]] <- metadata[[name]]
    }
  } else {
    stop("Argument filter must be either character string: 'all' or a valid list");
  }
  n_filter <- prod(lengths(filter));
  ## Create ABS URL and open session 
  url <- file.path(abs_api_urls()$base_url, abs_api_urls()$sdmx_json_path,
                   dataset,
                   paste(lapply(filter,
                                function(x) paste(x, collapse="+")),
                         collapse="."),
                   "all");
  ## dimensionAtObservation
  dimensionAtObservation <- match.arg(dimensionAtObservation);
  if (!dimensionAtObservation %in% c("AllDimensions","TimeDimension","MeasureDimension"))
    stop("dimensionAtObservation argument invalid!")
  detail <- match.arg(detail);
  if (!detail %in% c("Full","DataOnly","SeriesKeysOnly","NoData"))
    stop("detail argument invalid!")
  ## 
  url <- paste0(url, "?",
                "detail", detail, "&",
                "dimensionAtObservation=", dimensionAtObservation);
  ## Add start/end dates
  if (!missing(start_date))
    url <- paste0(url, "&", start_date)
  if (!missing(end_date))
    url <- paste0(url, "&", end_date);
  
  if (return_url) {
    return(url)
  } else {
    ## Check URL length - ABS.Stat limit: 1000 characters
    if (nchar(url) > 1000)
      stop(sprintf(paste("URL query length (%i) exceeds maximum request URL limit (1000 characters).",
                         "Filter query in one or more dimensions."),
                   nchar(url)));
    ## Check number of observations (N) - ABS.Stat limit: 1 million observations
    ##  -- TO DO -- Do this earlier in the process
    ## N <- n_filter * Length of time dimension
    ## 
    ## if (nchar(url) > 10^6)
    ##   stop(sprintf(paste("URL query length (%i) exceeds maximum request URL limit (1000 characters).",
    ##                      "Filter query in one or more dimensions.",
    ##                      nchar(url)));
    
    ## Download data
    x_json <- fromJSON(url)
    ## Convert JSON format to long (tidy) data frame
    x_obs <- x_json$dataSets$observation;
    x_str <- x_json$structure$dimensions$observation;
    y <- setNames(data.frame(do.call(rbind, unlist(x_obs, recursive=FALSE))),
                  c("values","obs_status","unknown"))
    y <- cbind(setNames(data.frame(do.call(rbind, strsplit(row.names(y), ":"))),
                        tolower(sub("\\s+","_", x_str$name))),
               y);
    ## Rename
    for (name in tolower(sub("\\s+","_", x_str$name)))
      y[,name] <- as.integer(as.character(y[,name])) + 1;
    names_y <- setNames(lapply(seq_len(nrow(x_str)),
                               function(j) unlist(x_str[j,"values"], recursive=FALSE)
                               ),
                        tolower(sub("\\s+","_", x_str$name)));
    ## Substitute dimension IDs for Names
    for (name in names(names_y))
      y[,name] <- names_y[[name]]$name[y[,name]]
    ## Dataset name
    y$dataset_name <- x_json$structure$name;
    
    ## Return data
    row.names(y) <- seq_len(nrow(y));
    return(y);
  }
}
