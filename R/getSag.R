#' Download data from the ICES Stock Assessment database.
#'
#' Download data from the ICES Stock Assessment database for the year in
#' which you are producing the Fisheries Overviews. From SAG
#'
#' @param year the year for which data is required.
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{load_sid}} for loading data from the ICES Stock Information database.
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' sag_summary_raw <- load_sag_summary(2019)
#' sag_refpts_raw <- load_sag_refpts(2019)
#' sag_status_raw <- load_sag_status(2019)
#' }
#'
#' @references
#'
#' The ICES stock assessment graphs Database web sevices: \url{http://standardgraphs.ices.dk/stockList.aspx}
#'
#' @rdname getSAG
#' @name getSag
NULL


#'@rdname getSag
#' @export


#This gets all last assessments for all stocks in sid

getSAG_complete <- function(year){
        years <- ((year-3):year)
        sid <- icesSD::getSD(NULL,year)
        out <- data.frame()
        res <- data.frame()
        for(n in 1:4){
                x <- years[n]
                url <- paste0("https://sag.ices.dk/SAG_API/api/SAGDownload?year=", x)
                tmpSAG <- tempfile(fileext = ".zip")
                download.file(url, destfile = tmpSAG, mode = "wb", quiet = FALSE)
                names <-unzip(tmpSAG, list = TRUE)
                res <- read.csv(unz(tmpSAG, names$Name[1]),
                                stringsAsFactors = FALSE,
                                header = TRUE,
                                fill = TRUE)
                res<- unique(res)
                out <- rbind(out, res)
        }
        out <- dplyr::filter(out, Purpose == "Advice")
        out <- data.table::as.data.table(out) 
        out <- out[out[, .I[AssessmentKey == max(AssessmentKey)], by=FishStock]$V1]
        out <- dplyr::filter(out,out$FishStock %in% sid$StockKeyLabel)
        out <- as.data.frame(out)
}



getSAG_ecoregion <- function(year, ecoregion){
        years <- ((year-4):year)
        ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
        sid <- icesSD::getSD(NULL,year)
        out <- data.frame()
        res <- data.frame()
        for(n in 1:5){
                x <- years[n]
                url <- paste0("https://sag.ices.dk/SAG_API/api/SAGDownload?year=", x, "&EcoRegion=", ecoreg)
                tmpSAG <- tempfile(fileext = ".zip")
                download.file(url, destfile = tmpSAG, mode = "wb", quiet = FALSE)
                names <-unzip(tmpSAG, list = TRUE)
                res <- read.csv(unz(tmpSAG, names$Name[1]),
                                stringsAsFactors = FALSE,
                                header = TRUE,
                                fill = TRUE)
                res<- unique(res)
                out <- rbind(out, res)
        }
        out <- dplyr::filter(out, Purpose == "Advice")
        out <- data.table::as.data.table(out) 
        # out <- out[out[, .I[AssessmentKey == max(AssessmentKey)], by=FishStock]$V1]
        out <- out[out[, .I[AssessmentYear == max(AssessmentYear)], by=FishStock]$V1]
        out <- as.data.frame(out)
        out <- dplyr::filter(out,out$FishStock %in% sid$StockKeyLabel)
}


#Now status loads whatever is in sag

#' @rdname load_sag
#' @export

getSAG_status <- function(x) {
        stocks <- unique(x[c("AssessmentKey","FishStock")])
        status <- icesSAG::getStockStatusValues(stocks$AssessmentKey)
        status <- do.call(rbind.data.frame, status)
        stocks$AssessmentKey <- as.character(stocks$AssessmentKey)
        status <- dplyr::left_join(status, stocks)
        status <- dplyr::mutate(status, StockKeyLabel= FishStock)
        status <- subset(status, select = -c(FishStock))
        status <- dplyr::relocate(status, StockKeyLabel, .before = lineNumber)
        status
}



