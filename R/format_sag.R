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
        y <- dplyr::filter(y,!is.na(YearOfLastAssessment))
        y <- dplyr::select(y,StockKeyLabel,FisheriesGuild)
        x <- dplyr::mutate(x, StockKeyLabel=FishStock)
        df1 <- merge(x,y, all.x = T, all.y = F)
        # df1 <- left_join(x, y)
        # df1 <- left_join(x, y, by = c("StockKeyLabel", "AssessmentYear"))
        df1 <-as.data.frame(df1)
        
        df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        df1 <- subset(df1, select = -c(FishStock))
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        # check <-unique(df1[c("StockKeyLabel", "FisheriesGuild")])
        out <- dplyr::anti_join(df1, check)
}
