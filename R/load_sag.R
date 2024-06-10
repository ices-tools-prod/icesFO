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
#' @rdname load_sag
#' @name load_sag
NULL


#'@rdname load_sag
#' @export


load_sag_complete <- function(year){
        years <- ((year-3):year)
        # ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
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
        out <- as.data.frame(out)
}






#' Title
#'
#' @param year 
#' @param ecoregion 
#'
#' @return
#' @export
#'
#' @examples
load_sag <- function(year, ecoregion){
        years <- ((year-3):year)
        ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
        out <- data.frame()
        res <- data.frame()
        for(n in 1:4){
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
        out <- out[out[, .I[AssessmentKey == max(AssessmentKey)], by=FishStock]$V1]
        out <- as.data.frame(out)
}


#' load_sag_summary <-  function(year){
#'         years <- ((year-3):year)
#'         out <- icesSAG::getSAG(stock = NULL,
#'                                 years,
#'                                 data = "summary",
#'                                 purpose = "Advice",
#'                                 combine = TRUE)
#'         sid<-load_sid(year)
#'         sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
#'         sid <- dplyr::select(sid,StockKeyLabel,
#'                                 YearOfLastAssessment, PreviousStockKeyLabel)
#'         colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
#'         old <- dplyr::filter(sid, AssessmentYear < 2017)
#'         out1 <- merge(out, sid, by = c("fishstock", "AssessmentYear"),all = FALSE)
#'         out2 <- merge(out, old, by.x = c("fishstock", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
#'         out2$fishstock <- out2$fishstock.y
#'         out2 <- subset(out2,select = -fishstock.y)
#'         out <- merge(out1,out2, all = TRUE)
#'         out <- subset(out,select = -PreviousStockKeyLabel)
#'         unique(out)
#'         }
#'         
#' 

#' load_sag_refpts <- function(year){
#'         years <- ((year-3):year)
#'         out <- icesSAG::getSAG(stock = NULL,
#'                                 years ,
#'                                 purpose = "Advice",
#'                                 data = "refpts",
#'                                 combine = TRUE)
#'         sid<-load_sid(year)
#'         sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
#'         sid <- dplyr::select(sid,StockKeyLabel,
#'                              YearOfLastAssessment, PreviousStockKeyLabel)
#'         colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
#'         old <- dplyr::filter(sid, AssessmentYear < 2017)
#'         out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
#'         out2 <- merge(out, old, by.x = c("StockKeyLabel", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
#'         out2$StockKeyLabel <- out2$StockKeyLabel.y
#'         out2 <- subset(out2,select = -StockKeyLabel.y)
#'         out <- merge(out1,out2, all = TRUE)
#'         out <- subset(out,select = -PreviousStockKeyLabel)
#'         unique(out)
#' }


#' Title
#'
#' @param sag 
#'
#' @return
#' @export
#'
#' @examples
load_sag_status_new <- function(sag) {
        stocks <- unique(sag[c("AssessmentKey","FishStock")])
        status <- icesSAG::getStockStatusValues(stocks$AssessmentKey)
        status <- do.call(rbind.data.frame, status)
        stocks$AssessmentKey <- as.character(stocks$AssessmentKey)
        status <- left_join(status, stocks)
        status <- dplyr::mutate(status, StockKeyLabel= FishStock)
        status <- subset(status, select = -c(FishStock))
        status <- dplyr::relocate(status, StockKeyLabel, .before = lineNumber)
        status
}



#' Title
#'
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
load_sag_status <- function(year) {
        years <- ((year-3):year)
        out <- do.call("rbind", lapply(years,function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                                       year = x,
                                                                                       full = TRUE)[, c("AssessmentYear",
                                                                                                        "AssessmentKey",
                                                                                                        "StockKeyLabel", "Purpose")]))
        out <- dplyr::filter(out,Purpose =="Advice")
        out <- out[,-4]
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel, EcoRegion, AdviceCategory)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel", "Ecoregion", "AdviceCategory")
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("StockKeyLabel", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = TRUE)
        out2$StockKeyLabel <- out2$StockKeyLabel.y

        out2 <- subset(out2,select = -StockKeyLabel.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        out <-out[!duplicated(out$StockKeyLabel),]

        get_stock_status <- function(assessmentKey) {
                dat <- icesSAG::getStockStatusValues(assessmentKey)[[1]]
                if(is.null(dat)) stop(paste0("NULL value returned for assessmentKey = ", assessmentKey))
                dat
        }
        out <- dplyr::filter(out, !is.na(out$AssessmentKey))
        out2 <- dplyr::mutate(out, stock_status = purrr::map(.x = AssessmentKey, purrr::possibly(get_stock_status, otherwise = NA_real_)))
        out2 <- dplyr::filter(out2, !is.na(stock_status))
        out2 <- dplyr::select(out2, -AssessmentKey)
        out2 <- tidyr::unnest(out2, stock_status)
        out2 <- unique(out2)
        # out3 <- subset(out, !(StockKeyLabel %in% out2$StockKeyLabel))
}
