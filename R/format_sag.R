#' Format the data from the ICES Stock Assessment Database.
#'
#' Format the data from the ICES Stock Assessment Database for the downloaded year and the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#'
#' @param x a dataframe output from load_sag_summary() required.

# CHECK what to do with this
#' @param y a dataframe output from webservice required.
#' @param year the year for which data is required.
#' @param ecoregion an identifier of the Ecoregion of interest
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{format_sid}} for formatting raw data from the ICES Stock Information database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' sag_format <- format_sag(x,y, "Celtic Seas")
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

#Saving all variables with some value, so we keep also custom values

#this function only removes empty variables and adds SID, which is only used to find the Guild.


format_sag <- function(x,y){
        # sid <- load_sid(year)
        sid <- dplyr::filter(y,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,AssessmentKey,
                             YearOfLastAssessment, EcoRegion, FisheriesGuild)
        colnames(sid) <- c("StockKeyLabel", "AssessmentKey", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        sag <- dplyr::mutate(x, StockKeyLabel= FishStock)
        df1 <- left_join(sag, sid, by = c("StockKeyLabel", "AssessmentYear"), all = TRUE)
        df1 <-as.data.frame(df1)
        # df1 <- df1 %>% filter(AssessmentKey %in% sag$AssessmentKey)
        # df1 <- dplyr::filter(df1,(grepl(pattern = ecoregion, Ecoregion)))
        # df1 <- df1 %>% filter(FishStock != NA)
        df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        # df1 <- dplyr::select(df1,Year,
        #        StockKeyLabel,
        #        FisheriesGuild,
        #        Purpose,
        #        F,
        #        SSB,
        #        fishingPressureDescription,
        #        stockSizeDescription,
        #        landings,
        #        catches,
        #        discards)
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        df1 <- subset(df1, select = -c(FishStock))
        # df2 <- merge(y, sid, by = c("StockKeyLabel", "AssessmentYear"), all = TRUE)
        # df2 <- dplyr::filter(df2,(grepl(pattern = ecoregion, Ecoregion)))
        # df2 <- dplyr::select(df2,StockKeyLabel,
        #        AssessmentYear,
        #        Flim = FLim,
        #        Fpa,
        #        Bpa,
        #        Blim,
        #        FMSY,
        #        MSYBtrigger)
        # 
        # out <- dplyr::left_join(df1,df2)
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        out <- dplyr::anti_join(df1, check)
}
