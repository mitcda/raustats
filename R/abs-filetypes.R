#' @name abs_filetypes
#' @title Valid ABS file types
#' @description This function returns a vector of valid ABS file types used in filtering ABS
#'   Catalogue data access calls. It is used in other functions in this package and need not be
#'   called directly.
#' @return a vector containing a list of valid ABS file types.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
#' @family ABS helper functions
abs_filetypes <- function()
{
  c(zip_files = "application/x-zip",
    excel_files = "application/vnd.ms-excel",
    openxml_files = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    pdf_files = "application/pdf");
}

## ----------------------------------- EOF ---------------------------------- ##
