#' Format the data from the ICES Stock Information Database.
#'
#' Format the data from the ICES Stock Information Database for the downloaded year and the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#'
#' @param x a dataframe output from load_sid() required.
#' @param ecoregion an identifier of the Ecoregion of interest
#'
#' @return A data frame..
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
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

#other variables to keep?
format_sid <- function(x, ecoregion)
{
        df<- dplyr::filter(x,!is.na(YearOfLastAssessment))
        df<- dplyr::filter(df,(grepl(pattern = ecoregion, EcoRegion)))
        df <- dplyr::select(df,StockKeyLabel,
               Description = StockKeyDescription,
               SpeciesScientificName,
               EcoRegion,
               DataCategory,
               YearOfLastAssessment,
               YearOfNextAssessment,
               AdviceCategory,
               FisheriesGuild)
}
