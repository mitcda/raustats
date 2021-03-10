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
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @examples
#'   \donttest{
#'     z <- abs_cache(lang='en', progress=5)
#'   }
abs_cache <- function(lang="en", progress=10)
{
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
#' @importFrom stats setNames
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
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @note This is an internal library function and is not exported.
#' @examples
#'  \donttest{
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
                                           stringsAsFactors=FALSE),
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
