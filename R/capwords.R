### Function: capwords
#' @name capwords
#' @title Capitalise words in character vectors or string
#' @description Function to capitalise strings.
#' @param s Character vector or string
#' @param split Specifies base month for first quarter. Can be a scalar: 1,2,3 or character object: Jan,Feb,Mar.
#' @param strict The input date format. Default is "\%Y-Q\%q".
#' @return Function returns a character vector object.
#' @note This function 
#' @examples
#' ## TO DO
#' @export
capwords <- function (s, split = c("\\W"), strict = FALSE) 
{
  if (length(s) == 0) 
    stop("Cannot capitalise a blank string.")
  if (!is.character(s)) 
    s <- as.character(s)
  replace(s, is.na(s), "")
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
    if (strict) 
      tolower(s)
    else s
  }, sep = "", collapse = " ")
  sapply(strsplit(s, split = split), cap, USE.NAMES = !is.null(names(s)))
}


### Function: capwords2
#' @name capwords2
#' @title Capitalise words in character vectors or string
#' @description Function to capitalise strings
#' @param s Character vector or string
#' @param split Specifies base month for first quarter. Can be a scalar: 1,2,3 or character object: Jan,Feb,Mar.
#' @param strict The input date format. Default is "\%Y-Q\%q".
#' @return Function returns a character vector object.
#' @note This
#' @examples
#' ## TO DO
#' @export
capwords2 <-
  function (s, split = c("\\W"), strict = FALSE) 
{
  if (length(s) == 0) 
    stop("Cannot capitalise a blank string.")
    if (!is.character(s)) 
        s <- as.character(s)
    replace(s, is.na(s), "")
    cap <- function(s) {
        s <- unlist(strsplit(s, split = ""))
        .i <- c(1, which(grepl(split, s)) + 1)
        s[.i] <- toupper(s[.i])
        s <- paste0(s, collapse = "")
    }
    sapply(s, cap, USE.NAMES = !is.null(names(s)))
}
