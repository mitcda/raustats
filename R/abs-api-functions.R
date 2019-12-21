### ABS API functions

#' @name abs_api_urls
#' @title ABS URL addresses and paths used in ABS.Stat API calls
#' @description This function returns a list of URLs and data paths used to construction ABS.Stat
#'   API call. It is used in other functions in this package and need not be called directly.
#' @return a list with a base url and a url section for formatting the JSON API calls
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_api_urls <- function()
  list(base_url = "http://stat.data.abs.gov.au",
       datastr_path = "restsdmx/sdmx.ashx/GetDataStructure",
       sdmx_json_path = "SDMX-JSON/data")


#' @name abs_api_call
#' @title Create ABS.Stat API URL call
#' @description The function created the ABS.Stat API call URL
#' @param path Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @param args Named list of arguments to supply to call.
#' @return data frame in long format
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_api_call <- function(path, args)
{
    if (missing(path))
      stop("path missing.")

    if (missing(args))
      stop("Argument path missing.")

    url <- file.path(abs_api_urls()$base_url, path, args)

    return(url);
}


#' @name abs_call_api
#' @title Submit API call to ABS.Stat
#' @description This function submits the specified API call to ABS.Stat
#' @importFrom xml2 read_xml
#' @importFrom httr http_error
#' @param url Character vector specifying one or more ABS collections or catalogue numbers to
#'   download.
#' @return data frame in long format
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
abs_call_api <- function(url)
{
  if (http_error(url))
    stop(sprintf("HTTP error returned by url: %s", url))
  
  x <- read_xml(url)
  return(x);
}


#' @name abs_datasets
#' @title Download ABS.Stat datasets
#' @description This function returns a list of all datasets available from ABS.Stat.
#' @importFrom xml2 as_list read_xml read_html xml_name xml_find_all
#' @param lang Preferred language (default 'en' - English).
#' @param include_notes Include ABS annotation information for each series.
#' @return data frame in long format
#' @export
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


#' @name abs_dimensions
#' @title Return available dimensions of ABS series
#' @description This function returns the available dimeninsions for a specified ABS API dataset.
#' @param dataset Character vector of dataset codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}.
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available
#'   ABS.Stat datasets, if TRUE, update the list of available datasets.
#' @return a data frame with available dataset dimensions.
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     ## CPI - Consumer Price Index
#'     x <- abs_dimensions("CPI");
#'     str(x)
#'     ## LF - Labour Force
#'     x <- abs_dimensions("LF");
#'     str(x)
#'   }
abs_dimensions <- function(dataset, update_cache=FALSE)
{
  ## Check dataset present and valid 
  if (missing(dataset))
    stop("No dataset name supplied.");
  if (update_cache) {
    cache <- abs_datasets();
  } else {
    cache <- raustats::abs_cachelist;
  }
  if (!dataset %in% cache$id)
    stop(sprintf("%s not valid dataset name.", dataset));
  metadata <- abs_metadata(dataset)
  ## Return data frame of dataset dimensions:
  z <- data.frame(name = attr(metadata, "concept"),
                  type = attr(metadata, "type"));
  return(z)
}


#' @name abs_search
#' @title Search dataset information from the ABS.Stat API
#' @description This function finds datasets or dimensions within a specific that match a specified
#'   regular expresion and returns matching results.
#' @param pattern Character string or regular expression to be matched.
#' @param dataset Character vector of ABS.Stat dataset codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}. If NULL (default), then function
#'   undertakes a dataset mode search. If not NULL, function searches all dimensions of specified
#'   dataset.
#' @param ignore.case Case senstive pattern match or not.
#' @param code_only If FALSE (default), all column/fields are returned. If TRUE, only the dataset
#'   identifier or indicator code are returned.
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available
#'   ABS.Stat datasets, if TRUE, update the list of available datasets.
#' @return A data frame with datasets and data items that match the search pattern.
#' @export
#' @note With acknowledgements to \code{wb_search} function.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'  ## ABS dataset search
#'  x <- abs_search(pattern = "consumer price index")
#'  x <- abs_search(pattern = "census")
#'  x <- abs_search(pattern = "labour force")
#'
#'  ## ABS indicator search
#'  x <- abs_search(pattern = "all groups", dataset="CPI")
#'  x <- abs_search(pattern = c("all groups", "capital cities"), dataset="CPI")
#' 
abs_search <- function(pattern, dataset=NULL, ignore.case=TRUE, code_only=FALSE, update_cache=FALSE)
{
  if (missing(pattern))
    stop("No regular expression provided.")
  if (update_cache) {
    cache <- abs_datasets();
  } else {
    cache <- raustats::abs_cachelist;
  }
  ## 
  if (is.null(dataset)) {
    ## 1. If dataset not specified, search through list of datasets
    ## Return list of matching ABS.Stat datasets
    match_index <- sapply(names(cache), ## cache_table
                          function(i) grep(pattern, cache[, i], ignore.case=ignore.case), ## cache_table[, i]
                          USE.NAMES = FALSE);
    match_index <- sort(unique(unlist(match_index)));
    if (length(match_index) == 0)
      warning(sprintf("No matches were found for the search term %s. Returning an empty data frame.", 
                      pattern));
    match_df <- unique(cache[match_index, ])  ## unique(cache_table[match_index, ])
    rownames(match_df) <- seq_len(nrow(match_df));
    if (code_only)
      match_df <- as.character(match_df[,"id"]);
    return(match_df);
  } else {
    ## 2. If dataset specified, search through list of datasets
    if (!dataset %in% cache$id)
      stop(sprintf("Dataset: %s not available on ABS.Stat", dataset))
    .cachelist <- abs_metadata(dataset);
    names(.cachelist) <- attr(.cachelist, "concept");
    ## Return list of all dataset dimensions with matching elements
    filter_index <- lapply(.cachelist,
                           function(x) {
                             i <- grep(sprintf("(%s)", paste(pattern, collapse="|")),
                                       x$Description, ignore.case=ignore.case);
                             z <- x[i,];
                             return(z);
                           });
    filter <- filter_index[sapply(filter_index, nrow) > 0]
    if (code_only)
      filter <- lapply(filter, function(x) as.character(x$Code));
    return(filter)
  }
}


#' @name abs_stats
#' @title Download data from the ABS API
#' @description This function queries and returns data for a specified ABS dataset from the ABS API.
#' @importFrom xml2 read_xml read_html
#' @importFrom httr content GET http_error http_status http_type progress status_code
#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames
#' @param dataset Character vector of ABS.Stat dataset codes. These codes correspond to the
#'   \code{indicatorID} column from the indicator data frame of \code{abs_cache} or
#'   \code{abs_cachelist}, or the result of \code{abs_indicators}.
#' @param filter A list that contains filter of dimensions available in the specified \code{series}
#'   to use in the API call. If NULL, no filter is set and the query tries to return all dimensions
#'   of the dataset. Valid dimensions to include in the list supplied to filter include: MEASURE,
#'   REGION, INDEX, TSEST and FREQUENCY.
#' @param start_date Numeric or character. If numeric it must be in %Y form (i.e. four digit
#'   year). For data at the sub-annual granularity the API supports a format as follows: Monthly
#'   data -- '2016-M01', Quarterly data -- '2016-Q1', Semi-annual data -- '2016-B2', Financial year
#'   data -- '2016-17'.
#' @param end_date Numeric or character (refer to \code{startdate}).
#' @param lang Language in which to return the results. If \code{lang} is unspecified, english is
#'   the default.  ## @param remove_na If \code{TRUE}, remove blank or NA observations. If
#'   \code{FALSE}, no blank or NA ## values are removed from the return.  ## @param include_unit If
#'   \code{TRUE}, the column unit is not removed from the return. If ## \code{FALSE}, this column is
#'   removed.  ## @param include_obsStatus If \code{TRUE}, the column obsStatus is not removed from
#'   the return. If ## \code{FALSE}, this column is removed.
#' @param dimensionAtObservation The identifier of the dimension to be attached at the observation
#'   level. The default order is: 'AllDimensions', 'TimeDimension' and 'MeasureDimension'.
#'   AllDimensions results in a flat list of observations without any grouping.
#' @param detail This argument specifies the desired amount of information to be returned. Possible
#'   values are:
#' 
#'   \itemize{
#'     \item Full: all data and documentation, including annotations (default)
#'     \item DataOnly: attributes – and therefore groups – will be excluded
#'     \item SeriesKeysOnly: only the series elements and the dimensions that make up the series keys
#'     \item NoData: returns the groups and series, including attributes and annotations, without observations (all values = NA)
#'   }
#' 
#' @param return_json Logical. Default is \code{FALSE}. If \code{TRUE}, the function returns the
#'   result in raw sdmx-json.
#' @param return_url Default is \code{FALSE}. If \code{TRUE}, the function returns the generated
#'   request URL and does not submit the request.
#' @param enforce_api_limits If \code{TRUE} (the default), the function enforces the ABS.Stat
#'   RESTful API limits and will not submit the query if the URL string length exceeds 1000
#'   characters or the query would return more than 1 million records. If \code{FALSE}, the function
#'   submits the API call regardless and attempts to return the results.
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available
#'   ABS.Stat datasets, if TRUE, update the list of available datasets.
#' @return Returns a data frame of the selected series from the specified ABS dataset.
#' @note The data query submitted by this function uses the ABS RESTful API based on the SDMX-JSON
#'   standard. It has a maximum allowable character limit of 1000 characters allowed in the data
#'   URL.
#'
#'   Further limitations known at this time include:
#'   \itemize{
#'     \item Only anonymous queries are supported, there is no authentication
#'     \item Each response is limited to no more than 1 million observations
#'     \item Errors are not returned in the JSON format but HTTP status codes and messages are
#'       set according to the Web Services Guidelines
#'     \item The lastNObservations parameter is not supported
#'     \item Observations follow the time series (or import-specific) order even if
#'       \code{dimensionAtObservation=AllDimensions} is used.
#'   }
#' 
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     x <- abs_stats(dataset="CPI", filter="all", return_url=TRUE);
#'     x <- abs_stats(dataset="CPI", filter=list(MEASURE=1, REGION=c(1:8,50),
#'                                               INDEX=10001, TSEST=10, FREQUENCY="Q"));
#'     x <- abs_stats(dataset="CPI", filter=list(MEASURE="all", REGION=50,
#'                                               INDEX=10001, TSEST=10, FREQUENCY="Q"));
#'     x <- abs_stats(dataset="CPI", filter=list(MEASURE="all", REGION=50, INDEX=10001,
#'                                               TSEST=10, FREQUENCY="Q"), return_url=TRUE);
#'  }
abs_stats <- function(dataset, filter, start_date, end_date, lang=c("en","fr"),
                      dimensionAtObservation=c("AllDimensions","TimeDimension","MeasureDimension"),
                      detail=c("Full","DataOnly","SeriesKeysOnly","NoData"),
                      return_json=FALSE, return_url=FALSE,
                      enforce_api_limits=TRUE, update_cache=FALSE)
{
  ## Check dataset present and valid 
  if (missing(dataset))
    stop("No dataset supplied.");
  if (!dataset %in% abs_datasets()$id)
    stop(sprintf("%s not a valid ABS dataset.", dataset));
  ## Check if filter provided
  if (missing(filter)) {
    dataset_dim <- abs_dimensions(dataset)
    stop(sprintf("No filter argument. Should be either 'all' or valid list with dataset dimensions: %s",
                 paste(dataset_dim[grepl("^dimension$", dataset_dim$type,
                                         ignore.case=TRUE), "name"], collapse=", ")));
  }
  ## Check if start_date > end_date
  if (!missing(start_date) && !missing(end_date) && start_date > end_date)
    stop("start_date later than end_date, request not submitted.")
  ## Return metadata
  if (update_cache) {
    cache <- abs_datasets();
  } else {
    cache <- raustats::abs_cachelist;
  }
  ## Get list of Dimension name:
  metadata <- abs_metadata(dataset);
  metadata_names <- abs_dimensions(dataset, );
  metadata_dims <- as.character(metadata_names[grepl("^dimension$", metadata_names$type, ignore.case=TRUE),
                                               "name"]);
  names(metadata) <- metadata_names$name;
  ## Return agency name
  ## agency_name <- unlist(attr(cache[[dataset]], "agency"));
  ## -- Check the set of dimensions supplied in 'filter' --
  if (length(filter) == 1 && filter == "all") {
    ##  If filter='all', replace with detailed filter list including all dimensions
    .filter <- metadata;
    filter <- lapply(.filter, function(x) x$Code);
    filter <- filter[names(filter) %in% metadata_dims];
  } else if (class(filter) == "list") {
    ## If filter is a list:
    if (any(!metadata_dims %in% names(filter))) {
      ## Check if any filter dimensions missing, and append missing elements (set to 'all')
      message(sprintf("Filter dimension(s): %s not in filter, dimensions added and set to 'all'.",
                      paste(metadata_dims[!metadata_dims %in% names(filter)], collapse=", ")));
      for (name in metadata_dims[!metadata_dims %in% names(filter)])
        filter[[name]] <- "all"
    }
    filter <- filter[metadata_dims];
    for (name in names(filter))
      if( length(filter[[name]]) == 1 && grepl("all", filter[[name]], ignore.case=TRUE) )
        filter[[name]] <- metadata[[name]]$Code;
  } else {
    stop("Argument filter must be either the single character string: 'all' or a valid filter list.");
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
  ## Append 'detail' and 'dimensionAtObservation' values to URL query
  url <- sprintf("%s?detail=%s&dimensionAtObservation=%s",
                 url, detail, dimensionAtObservation);
  ## Add start/end dates, and check validity
  if (!missing(start_date))
    url <- paste0(url, "&startPeriod=", start_date)
  if (!missing(end_date))
    url <- paste0(url, "&endPeriod=", end_date);
  ## Return URL if specified
  if (return_url) {
    return(url)
  } else {
    ## Check URL length - ABS.Stat limit: 1000 characters
    if (enforce_api_limits) {
      if (nchar(url) > 1000)
        stop(sprintf(paste("URL query length (%i) exceeds maximum request URL limit (1000 characters).",
                           "Filter query in one or more dimensions."),
                     nchar(url)));
      ## Check number of observations - ABS.Stat limit: 1 million observations
      time_filter <- metadata$TIME$Code;
      if(!missing(start_date))
        time_filter <- time_filter[time_filter >= start_date]
      if(!missing(end_date))
        time_filter <- time_filter[time_filter >= end_date]
      ## Count approximate number of records to be returned
      n_time <- sum(c(ifelse("A" %in% filter$FREQUENCY,
                             length(grep("^\\d{4}$", time_filter)),
                             NA_integer_)),
                    c(ifelse("S" %in% filter$FREQUENCY,
                             length(grep("^\\d{4}-B\\d+$", time_filter)),
                             NA_integer_)),
                    c(ifelse("Q" %in% filter$FREQUENCY,
                             length(grep("^\\d{4}-Q\\d+$", time_filter)),
                             NA_integer_)),
                    c(ifelse("M" %in% filter$FREQUENCY,
                             length(grep("^\\d{4}-M\\d+$", time_filter)),
                             NA_integer_)),
                    na.rm = TRUE);
      if (n_filter * n_time > 10^6)
        stop(sprintf(paste("Estimated number of records (%i) exceeds ABS.Stat limit (1 million).",
                           "Filter query in one or more dimensions."),
                     n_filter * n_time));
    }

    ## Download data
    ## cat(sprintf("API query submitted: %s...\n", substr(url, 30)));
    ## Error check URL call
    raustats_check_url_available(url)
    resp <- GET(url, raustats_ua(), progress())
    ## ## Error check URL call
    ## if (http_error(resp)) {
    ##   stop(
    ##     sprintf(
    ##       "ABS.Stat API request failed [%s]\n%s\n<%s>", 
    ##       status_code(resp),
    ##       http_status(resp)$message,
    ##       http_status(resp)$reason,
    ##       ),
    ##     call. = FALSE
    ##   )
    ## }
    ## Check content type
    if (!grepl("draft-sdmx-json", http_type(resp))) {
      stop("ABS.Stat API did not return SDMX-JSON format", call. = FALSE)
    }

    if (return_json) {
      ## Return results as sdmx-json text format
      return(content(resp, as="text"))
    } else {
      cat("Converting query output to data frame ... \n");
      ## Convert JSON to list
      x_json <- fromJSON(content(resp, as="text")) ## , simplifyVector = FALSE)
      ## Check whether data contains any observations
      if (ncol(x_json$dataSets$observation) == 0)
        stop(paste("API call returns no observations.",
                   "Check ABS.Stat or inspect JSON object with `return_json=TRUE`"), call. = FALSE);
      ## Convert JSON format to long (tidy) data frame
      x_obs <- x_json$dataSets$observation;
      x_str <- x_json$structure$dimensions$observation;
      y <- data.frame(do.call(rbind, unlist(x_obs, recursive=FALSE)));
      ## Set names of returned records
      y <- if (detail == "Full") {
             setNames(y, c("values","obs_status","unknown"))
           } else if (detail == "SeriesKeysOnly") {
             setNames(y, c("series_key"));
           } else if (detail == "DataOnly") {
             setNames(y, c("values"));
           } else { ## if (detail == NoData) {
             setNames(y, c("values","obs_status","unknown"))
           }
      y <- cbind(setNames(data.frame(do.call(rbind, strsplit(row.names(y), ":"))),
                          tolower(sub("\\s+","_", x_str$name))),
                 y);
      ## Re-index dimension IDs from 0-based to 1-based
      for (name in tolower(sub("\\s+","_", x_str$name)))
        y[,name] <- as.integer(as.character(y[,name])) + 1;
      names_y <- setNames(lapply(seq_len(nrow(x_str)),
                                 function(j) unlist(x_str[j,"values"], recursive=FALSE)
                                 ),
                          tolower(sub("\\s+","_", x_str$name)));
      ## Substitute dimension IDs for Names
      for (name in names(names_y))
        y[,name] <- names_y[[name]]$name[y[,name]]
      ## Insert dataset_name
      y$agency_id <- x_json$header$sender$id;
      y$agency_name <- x_json$header$sender$name;
      y$dataset_name <- x_json$structure$name;
      ## Re-index rows
      row.names(y) <- seq_len(nrow(y));
      ## cat("completed.\n");
      ## Return data
      return(y);
    } ## End: return_json
  }
}
