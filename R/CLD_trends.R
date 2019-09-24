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
                       AssessmentYear,
                       F,
                       FMSY,
                       SSB,
                       MSYBtrigger,
                       catches,
                       landings,
                       discards)
        df <- dplyr::bind_rows(
                        # all present and accounted for
                                dplyr::filter(df,!is.na(catches),
                                       !is.na(landings),
                                       !is.na(discards)), 
                         # Missing discards, but catches == landings
                         dplyr::filter(df, is.na(discards),
                                       catches == landings),
                         dplyr::mutate(df, discards = 0), 
                       # Missing catches, but have landings and discards
                       dplyr::filter(df,is.na(catches),
                                       !is.na(landings),
                                       !is.na(discards)), 
                       dplyr::mutate(df,catches = landings + discards), 
                        # missing catches, but have landings
                       dplyr::filter(df, is.na(catches),
                                       !is.na(landings),
                                       is.na(discards)),
                       dplyr::mutate(df,catches = NA,
                                       discards = NA),
                        # missing everything
                       dplyr::filter(df, is.na(catches),
                                       is.na(landings),
                                       is.na(discards)), 
                       dplyr::mutate(df,catches = NA,
                                       discards = NA,
                                       landings = NA),
                        # missing landings and discards
                       dplyr::filter(df, !is.na(catches),
                                       is.na(landings),
                                       is.na(discards)),
                       dplyr::mutate(df,landings = NA,
                                       discards = NA),
                        # landings and catches
                       dplyr::filter(df, is.na(catches),
                                       is.na(landings),
                                       !is.na(discards)),
                       dplyr::mutate(df,catches = NA,
                                       landings = NA),
                        # Missing discards, but have landings and catches
                       dplyr::filter(df, !is.na(catches),
                                       !is.na(landings),
                                       is.na(discards),
                                       landings != catches),
                       dplyr::mutate(df,discards = catches - landings),
                        # Missing landings, but have catches and discards
                       dplyr::filter(df, !is.na(catches),
                                       is.na(landings),
                                       !is.na(discards)),
                       dplyr::mutate(df,landings = catches - discards)
                )
        return(df)
}
