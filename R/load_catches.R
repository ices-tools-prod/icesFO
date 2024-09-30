#' Download catch data from ICES web services.
#'
#' Download historical and current catches data from the ICES web services
#' also preliminary catches can be downloaded for the current year
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
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @rdname load_catches
#' @name load_catches
NULL

#' @rdname load_catches
#' @export
load_historical_catches<- function(){
        url <- "http://ices.dk/data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
        tmpFileHistoric <- tempfile(fileext = ".zip")
        download.file(url, destfile = tmpFileHistoric, mode = "wb", quiet = FALSE)
        out <- read.csv(unz(tmpFileHistoric, "HistoricalLandings1950-2010/ICES_1950-2010.csv"),
                                      stringsAsFactors = FALSE,
                                      header = TRUE,
                                      fill = TRUE,
                                      na.strings = c("...", "-", "ns", "."))
}


#' @rdname load_catches
#' @export
load_official_catches<- function(){
        url <- "http://ices.dk/data/Documents/CatchStats/OfficialNominalCatches.zip"
        tmpFileCatch <- tempfile(fileext = ".zip")
        download.file(url, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
        out <- read.csv(unz(tmpFileCatch,
                        grep("ICESCatchDataset.*.csv", unzip(tmpFileCatch,
                             list = TRUE)$Name,
                             value = TRUE)),
                             stringsAsFactors = FALSE,
                             header = TRUE,
                             fill = TRUE)
        out <- Filter(function(x)!all(is.na(x)), out)
}


#' @rdname load_catches
#' @export
#


# change this
# https://data.ices.dk/rec12/api/getPreliminaryCatchStatistics?year=20
load_preliminary_catches <- function (year){
        url<- paste0("http://data.ices.dk/rec12/download/", year, "preliminaryCatchStatistics.zip")
        tmpFilePrelimCatch <- tempfile(fileext = ".zip")
        download.file(url, destfile = tmpFilePrelimCatch, mode = "wb", quiet = TRUE)
        out <- read.csv(unz(tmpFilePrelimCatch,
                            grep("preliminaryCatchStatistics.*.csv", unzip(tmpFilePrelimCatch,
                                                                           list = TRUE)$Name,
                                 value = TRUE)),
                        stringsAsFactors = FALSE,
                        header = TRUE,
                        fill = TRUE)
        out <- out %>% rename(Year = ï..Year)
        out
}
