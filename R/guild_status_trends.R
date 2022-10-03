#' Wrangling of format_sag output
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each guild in the Ecoregion, according to the last assessment (relative to the set year)
#' This guild grouping is used in the Ecosystem Overviews. 
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
#' trends <- guild_trends(sag_formatted)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

#I need to do the means without dplyr, lapply?lapply(years,function(x)
#Need to add a parameter to be able to have the output for EO plots by ecoregion

guild_trends <- function(x){
        x$FishingPressure <- as.numeric(x$FishingPressure)
        x$StockSize <- as.numeric(x$StockSize)
        x$FMSY <- as.numeric(x$FMSY)
        x$MSYBtrigger <- as.numeric(x$MSYBtrigger)
        df <- dplyr::group_by(x,FisheriesGuild)
        df <- dplyr::mutate(df,FMEAN = mean(FishingPressure, na.rm = TRUE),
                       SSBMEAN = mean(StockSize, na.rm = TRUE),
                       FMEAN = ifelse(!grepl("F|F(ages 3-6)", FishingPressureDescription),
                                      NA,
                                      FMEAN),
                       SSBMEAN = ifelse(!grepl("SSB", StockSizeDescription),
                                        NA,
                                        SSBMEAN))
        df <- dplyr::mutate(df,F_FMSY = ifelse(!is.na(FMSY),
                                       FishingPressure / FMSY,
                                       NA),
                       SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                StockSize / MSYBtrigger,
                                                NA))
        df <- dplyr::mutate(df,F_FMEAN = ifelse(!is.na(FMEAN),
                                        FishingPressure / FMEAN, 
                                        NA),
                       SSB_SSBMEAN = ifelse(!is.na(SSBMEAN),
                                            StockSize / SSBMEAN,
                                            NA))
        df<- dplyr::select(df,Year,
               FisheriesGuild,
               F_FMSY,
               SSB_MSYBtrigger,
               F_FMEAN,
               SSB_SSBMEAN) 
        df2 <-tidyr::gather(df,Metric, Value, -Year, -FisheriesGuild) 
        df2 <- dplyr::filter(df2,!is.na(Year))
        
        df3 <- dplyr::group_by(df2,FisheriesGuild,Metric, Year)
        df3 <- dplyr::summarize(df3,Value = mean(Value, na.rm = TRUE))
        df3 <- dplyr::select(df3,FisheriesGuild,
                       Year,
                       Metric,
                       Value) 
        df3 <- dplyr::filter(df3, !is.na(Value))
        
        means <- dplyr::group_by(df2,Metric, Year) 
        means <- dplyr::summarize(means, Value = mean(Value, na.rm = TRUE),
                          FisheriesGuild = "MEAN")
        means <- dplyr::select(means, FisheriesGuild,
                       Year,
                       Metric,
                       Value)
        means <- dplyr::filter(means, !is.na(Value))
        
        df4 <- dplyr::bind_rows(df3,means) 
        df4 <- dplyr::distinct(df4,.keep_all = TRUE)
        df4
}

