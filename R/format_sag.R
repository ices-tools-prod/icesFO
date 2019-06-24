#' Format the data from the ICES Stock Assessment Database.
#'
#' Format the data from the ICES Stock Assessment Database for the downloaded year and the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#'
#' @param x a dataframe output from load_sag_summary() required.
#' @param y a dataframe output from load_sag_refpts() required.
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

#other variables to keep?
format_sag <- function(x,y,year,ecoregion){
        sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, EcoRegion, FisheriesGuild)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        df1 <- dplyr::mutate(x, StockKeyLabel= fishstock)
        df1 <- merge(df1, sid, by = c("StockKeyLabel", "AssessmentYear"), all = TRUE)
        df1 <- dplyr::filter(df1,(grepl(pattern = ecoregion, Ecoregion)))
        df1 <- dplyr::select(df1,Year,
               StockKeyLabel,
               FisheriesGuild,
               F,
               SSB,
               fishingPressureDescription,
               stockSizeDescription,
               landings,
               catches,
               discards)
        df2 <- merge(y, sid, by = c("StockKeyLabel", "AssessmentYear"), all = TRUE)
        df2 <- dplyr::filter(df2,(grepl(pattern = ecoregion, Ecoregion)))
        df2 <- dplyr::select(df2,StockKeyLabel,
               AssessmentYear,
               Flim = FLim,
               Fpa,
               Bpa,
               Blim,
               FMSY,
               MSYBtrigger)
        
        out <- dplyr::left_join(df1,df2)
}
