#' Download catches and effort by country data from STECF dissemantion data web.
#' 
#'This is a new format from 2015
#' 
#' @param year the year for which data is required.
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
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
#'
#' @references
#'
#' STECF data dissemination site \url{https://stecf.jrc.ec.europa.eu/dd/fdi}
#'
#' @rdname load_catches
#' @name load_catches
NULL

#' @rdname load_catches
#' @export
load_STECF<- function(){
        url <- "https://stecf.jrc.ec.europa.eu/documents/43805/2703037/2020_FDI_data.zip/c0cfadb8-3a57-45a5-ba4b-e715de6f1a27"
        tmpFileCatch <- tempfile(fileext = ".zip")
        download.file(url, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
        out <- read.csv(unz(tmpFileCatch,
                        grep("FDI_catches-by-country.csv", unzip(tmpFileCatch,
                             list = TRUE)$Name,
                             value = TRUE)),
                             stringsAsFactors = FALSE,
                             header = TRUE,
                             fill = TRUE)
        out <- Filter(function(x)!all(is.na(x)), out)
}


