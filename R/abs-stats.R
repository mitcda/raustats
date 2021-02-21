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
#' @param return_json Logical. Default is \code{FALSE}. If \code{TRUE}, the function returns
#'   results in a list of raw sdmx-json.
#' @param return_url Default is \code{FALSE}. If \code{TRUE}, the function returns the generated
#'   request URL and does not submit the request.
#' @param enforce_api_limits [DEPRECATED] If \code{TRUE} (the default), the function enforces the
#'   ABS.Stat RESTful API limits and will not submit the query if the URL string length exceeds 1000
#'   characters or the query would return more than 1 million records. If \code{FALSE}, the function
#'   submits the API call regardless and attempts to return the results.
#' @param requery_limit [NEW - EXPERIMENTAL] If query string exceeds 1000 characters in length, the
#'   function automatically splits the query into multiple parts and submits the parts separately
#'   and meres the results. This argument limits the number of parts the function will submit by
#'   default, in order to avoid inadvertantly sending a large number of requests to the ABS.Stat
#'   server. By default, the sequential query limit is 20, but this may be changed by the user.
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
#' @family ABS.State functions
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
                      enforce_api_limits=TRUE, requery_limit=20, update_cache=FALSE)
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
  if (!exists("abs_stat_list", envir=.raustats_cache, inherits=FALSE)) {
    message("Scanning all ABS.Stat catalogue (called on first use each session).")
    assign("abs_stat_list", abs_datasets(), envir=.raustats_cache);
  } else {
    cache <- get("abs_stat_list", envir=.raustats_cache);
  }
  ## if (update_cache) {
  ##   cache <- abs_datasets();
  ## } else {
  ##   cache <- raustats::abs_cachelist;
  ## }
  ## Get list of Dimension name:
  metadata <- abs_metadata(dataset);
  metadata_names <- abs_dimensions(dataset);
  metadata_dims <- as.character(metadata_names[grepl("^dimension$", metadata_names$type,
                                                     ignore.case=TRUE), "name"]);
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
    # if (enforce_api_limits) {
    if (nchar(url) > 1000) {
      url <- split_absstat_query(url)
      if (length(url) > requery_limit) {
        stop(paste("Query length exceeds ABS.Stat request limit (1000 characters) and",
                   sprintf("split query length (%d) exceeds 'requery_limit'.",
                           length(url)),
                   "Increase requery_limit or refine filter set."))
      } else {
        warning(paste("Query length exceeds ABS.Stat request limit (1000 characters).",
                      sprintf("Submitting query in %d parts.", length(url))));
      }
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
    resp <- lapply(url,
                   function(x) {
                     raustats_check_url_available(x);
                     resp <- GET(url, raustats_ua(), progress());
                     if (!grepl("draft-sdmx-json", http_type(resp))) {
                       stop("ABS.Stat API did not return SDMX-JSON format", call. = FALSE)
                     }
                     return(content(resp, as="text"));
                   });
    ## Check content type
    if (return_json) {
      ## Return results as list of sdmx-json text format output
      return(resp)
    } else {
      cat("Converting query output to data frame ... \n");
      ## Convert JSON to list
      x_json <- lapply(resp,
                       function(x) {
                         x_json <- fromJSON(x) ## , simplifyVector = FALSE)
                         ## Check whether data contains any observations
                         if (ncol(x_json$dataSets$observation) == 0)
                           stop(paste("API call returns no observations.",
                                      "Check ABS.Stat or inspect JSON object with `return_json=TRUE`"),
                                call. = FALSE);
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
                       });
      z <- do.call(rbind, x_json);
      ## Return data
      return(z);
    } ## End: return_json
  }
}
