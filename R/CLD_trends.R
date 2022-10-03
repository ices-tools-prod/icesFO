#' Catch, discards, and landings by stock 
#'
#' Returns a data frame of reference points, catch, discards, and landings by stock over time.
#'
#' @param x a dataframe output of format_sag function
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{stock_trends}} for formatting data from the ICES Stock Assessment database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' CLD_2018 <- catch_landings_discards(sag_formatted)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

#I need to do the means without dplyr, lapply?lapply(years,function(x)
#Need to add a parameter to be able to have the output for EO plots by ecoregion


CLD_trends <- function(x){
        df<- dplyr::select(x,Year,
                       StockKeyLabel,
                       FisheriesGuild,
                       FishingPressure,
                       FMSY,
                       StockSize,
                       MSYBtrigger,
                       Catches,
                       Landings,
                       Discards)
        df <- dplyr::bind_rows(
                        # all present and accounted for
                                dplyr::filter(df,!is.na(Catches),
                                       !is.na(Landings),
                                       !is.na(Discards)), 
                         # Missing discards, but catches == landings
                         dplyr::filter(df, is.na(Discards),
                                       Catches == Landings),
                         dplyr::mutate(df, discards = 0), 
                       # Missing catches, but have landings and discards
                       dplyr::filter(df,is.na(Catches),
                                       !is.na(Landings),
                                       !is.na(Discards)), 
                       dplyr::mutate(df,Catches = Landings + Discards), 
                        # missing catches, but have landings
                       dplyr::filter(df, is.na(Catches),
                                       !is.na(Landings),
                                       is.na(Discards)),
                       dplyr::mutate(df,Catches = NA,
                                       Discards = NA),
                        # missing everything
                       dplyr::filter(df, is.na(Catches),
                                       is.na(Landings),
                                       is.na(Discards)), 
                       dplyr::mutate(df,Catches = NA,
                                       Discards = NA,
                                       Landings = NA),
                        # missing landings and discards
                       dplyr::filter(df, !is.na(Catches),
                                       is.na(Landings),
                                       is.na(Discards)),
                       dplyr::mutate(df,Landings = NA,
                                       Discards = NA),
                        # landings and catches
                       dplyr::filter(df, is.na(Catches),
                                       is.na(Landings),
                                       !is.na(Discards)),
                       dplyr::mutate(df,Catches = NA,
                                       Landings = NA),
                        # Missing discards, but have landings and catches
                       dplyr::filter(df, !is.na(Catches),
                                       !is.na(Landings),
                                       is.na(Discards),
                                       Landings != Catches),
                       dplyr::mutate(df,Discards = Catches - Landings),
                        # Missing landings, but have catches and discards
                       dplyr::filter(df, !is.na(Catches),
                                       is.na(Landings),
                                       !is.na(Discards)),
                       dplyr::mutate(df,Landings = Catches - Discards)
                )
        return(df)
}

globalVariables(c('Year', 'StockKeyLabel', 'FisheriesGuild', 'AssessmentYear', 'FMSY', 'SSB', 'MSYBtrigger', 'catches', 'landings', 'discards'))
