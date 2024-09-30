
#' Helps creating the name of the file to save and save it in the right folder
#' 
#' @param year the active year
#' @param ecoregion
#' @param name (name of the file)
#' @param ext (extention of the file, for example: csv, png...)
#' @param dir (directory where to save the file)
#'
#' @return A data frame
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{format_sag}} for formatting raw data from the ICES Stock Assessment database.
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' sid_format <- format_sid(x, "Celtic Seas")
#' }
#'
#' @export
file_name <- function(year, ecoregion, name, ext = "", dir = c("bootstrap", "data", "model", "report")) {
  name <- gsub(" ", "_", name)
  if (nzchar(ext)) ext <- paste0(".", ext)
  paste0(dir,"/", year, "_", ecoregion, "_", "FO_", name, ext)
  # sprintf("%s_%s_FO_%s%s",year,ecoregion,name, ext)
}
