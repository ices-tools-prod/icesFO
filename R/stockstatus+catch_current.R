#' Stock status relative to reference points
#' 
#' returns a data frame of stock status relative to reference points and
#' catch, discards, and landings by stock for the most recent assessment.
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


#need to find a better name for this function

stockstatus_CLD_current <- function(x) {
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
        df$F <- as.numeric(df$F)
        df$SSB <- as.numeric(df$SSB)
        df$FMSY <- as.numeric(df$FMSY)
        df$MSYBtrigger <- as.numeric(df$MSYBtrigger)
        df2<-dplyr::full_join(df %>%
                                 dplyr::group_by(StockKeyLabel) %>%
                                 dplyr::filter(Year == AssessmentYear - 1) %>%
                                 dplyr::mutate(F_FMSY =  ifelse(!is.na(FMSY),
                                                                F / FMSY,
                                                                NA)) %>%
                                 dplyr::select(StockKeyLabel,
                                               FisheriesGuild,
                                               F_FMSY,
                                               catches,
                                               landings,
                                               discards,
                                               FMSY,
                                               F),
                         df %>%
                                 dplyr::group_by(StockKeyLabel) %>%
                                 dplyr::filter(Year == AssessmentYear ) %>%
                                 dplyr::mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                                        SSB / MSYBtrigger,
                                                                        NA)) %>%
                                 dplyr::select(StockKeyLabel,
                                               FisheriesGuild,
                                               SSB_MSYBtrigger,
                                               SSB,
                                               MSYBtrigger)) %>%
        dplyr::mutate(Status = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                                      "GREY",
                                      if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                              "GREEN",
                                              "RED",
                                              "GREY")))
        df2
}
