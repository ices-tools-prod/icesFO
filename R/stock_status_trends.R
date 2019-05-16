#' Wrangling of format_sag output
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of format_sag function
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{format_sag}} for formatting data from the ICES Stock Assessment database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' trends <- stock_trends(sag_formatted)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

#I need to do the means without dplyr, lapply?lapply(years,function(x)
#Need to add a parameter to be able to have the output for EO plots by ecoregion

stock_trends <- function(x){
        df <- dplyr::mutate(x,FMEAN = mean(F, na.rm = TRUE),
                       SSBMEAN = mean(SSB, na.rm = TRUE),
                       FMEAN = ifelse(!grepl("F|F(ages 3-6)", fishingPressureDescription),
                                      NA,
                                      FMEAN),
                       SSBMEAN = ifelse(!grepl("SSB", stockSizeDescription),
                                        NA,
                                        SSBMEAN))
        df <- dplyr::mutate(df,F_FMSY = ifelse(!is.na(FMSY),
                                       F / FMSY,
                                       NA),
                       SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                SSB / MSYBtrigger,
                                                NA))
        df <- dplyr::mutate(df,F_FMEAN = ifelse(!is.na(FMEAN),
                                        F / FMEAN, 
                                        NA),
                       SSB_SSBMEAN = ifelse(!is.na(SSBMEAN),
                                            SSB / SSBMEAN,
                                            NA))
        df<- dplyr::select(df,Year,
               StockKeyLabel,
               FisheriesGuild,
               F_FMSY,
               SSB_MSYBtrigger,
               F_FMEAN,
               SSB_SSBMEAN) 
        df2 <-tidyr::gather(df,Metric, Value, -Year, -StockKeyLabel, -FisheriesGuild) 
        df2 <- dplyr::filter(df2,!is.na(Year))
        
        df3 <- dplyr::group_by(df2,StockKeyLabel, FisheriesGuild,Metric, Year) %>%
                summarize(Value = mean(Value, na.rm = TRUE)) %>%
                select(FisheriesGuild,
                       StockKeyLabel,
                       Year,
                       Metric,
                       Value) %>%
                filter(!is.na(Value))
        
        means <- dplyr::group_by(df2,FisheriesGuild, Metric, Year) %>%
                summarize(Value = mean(Value, na.rm = TRUE),
                          StockKeyLabel = "MEAN") %>%
                select(FisheriesGuild,
                       StockKeyLabel,
                       Year,
                       Metric,
                       Value) %>%
                filter(!is.na(Value))
        
        df4 <- bind_rows(df3,means) %>%
                distinct(.keep_all = TRUE)
        df4
}

