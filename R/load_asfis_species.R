#' Download ASFIS Species list from FAO.
#'
#' Download 
#'
#' @return A data frame
#'
#' @seealso
#' \code{\link{load_sag}} for loading data from the ICES Stock Assessment database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' catches_hist_raw <- load_historical_catches()
#' catches_official_raw <- load_official_catches()
#' catches_prelim_raw <- load_preliminary_catches()
#' }

load_asfis_species <- function() {

  url <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"

  filename <- tempfile(fileext = ".zip")

  download.file(url, destfile = filename, mode = "wb")

  txtfile <- grep(".*txt", unzip(filename, list = TRUE)$Name, value = TRUE)
  unzip(filename, files = txtfile, exdir = tempdir())

  species <- 
    read.csv(file.path(tempdir(), txtfile), 
             na.strings = "", stringsAsFactors = FALSE)

  dplyr::select(species, English_name, Scientific_name, X3A_CODE)
}
