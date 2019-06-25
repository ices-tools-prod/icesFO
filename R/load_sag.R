#' Download data from the ICES Stock Assessment database.
#'
#' Download data from the ICES Stock Assessment database for the year in
#' which you are producing the Fisheries Overviews. Form SAG
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
#' @export
#' 
#' 

load_sag_summary <-  function(year){
        years <- ((year-3):year)
        out <- icesSAG::getSAG(stock = NULL,
                                years,
                                data = "summary",
                                purpose = "Advice",
                                combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                                YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("fishstock", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("fishstock", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
        out2$fishstock <- out2$fishstock.y
        out2 <- subset(out2,select = -fishstock.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        unique(out)
        }
        
#' @export

load_sag_refpts <- function(year){
        years <- ((year-3):year)
        out <- icesSAG::getSAG(stock = NULL,
                                years ,
                                purpose = "Advice",
                                data = "refpts",
                                combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("StockKeyLabel", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
        out2$StockKeyLabel <- out2$StockKeyLabel.y
        out2 <- subset(out2,select = -StockKeyLabel.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        unique(out)
}


#for each StockKeyLabel, should only keep last year, maybe here maybe later 
#Check the issue with old stock codes for stocks last assessed before 2016

#' @export

load_sag_status <- function(year) {
        years <- ((year-3):year)
        out <- do.call("rbind", lapply(years,function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                                       year = x,
                                                                                       full = TRUE)[, c("AssessmentYear",
                                                                                                        "AssessmentKey",
                                                                                                        "StockKeyLabel", "Purpose")]))
        out <- dplyr::filter(out,Purpose =="Advice")
        out <<- out[,-4]
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
        out2 <- dplyr::mutate(out, stock_status = purrr::map(.x = AssessmentKey, purrr::possibly(get_stock_status, otherwise = NA_real_)))
        out2<- dplyr::filter(out2,!is.na(stock_status)) 
        out2<- tidyr::unnest(out2,stock_status)
        out2 <- unique(out2)
        # out3 <- subset(out, !(StockKeyLabel %in% out2$StockKeyLabel))
   
        
}
