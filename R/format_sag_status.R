#' Format the stock status data as is in the ICES Stock Assessment Database.
#'
#' Format the data from the ICES Stock Assessment Database for the downloaded year and the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#'
#' @param x a dataframe output from load_sag_status() required.
#' @param year the year required
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
#' sag_status <- format_sag_status(x,"Celtic Seas")
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export



format_sag_status <- function(x,year, ecoregion) {
        df <- x
        df <- dplyr::filter(df,(grepl(pattern = ecoregion, Ecoregion)))
        df <- dplyr::mutate(df,status = case_when(status == 0 ~ "UNDEFINED",
                                                  status == 1 ~ "GREEN",
                                                  status == 2 ~ "qual_GREEN", #qualitative green
                                                  status == 3 ~ "ORANGE",
                                                  status == 4 ~ "RED",
                                                  status == 5 ~ "qual_RED", #qualitative red
                                                  status == 6 ~ "GREY",
                                                  status == 7 ~ "qual_UP",
                                                  status == 8 ~ "qual_STEADY",
                                                  status == 9 ~ "qual_DOWN",
                                                  TRUE ~ "OTHER"),
                            fishingPressure = case_when(fishingPressure == "-" &
                                                                type == "Fishing pressure" ~ "FQual",
                                                        TRUE ~ fishingPressure),
                            stockSize = case_when(stockSize == "-" &
                                                          type == "Stock Size" ~ "SSBQual",
                                                  TRUE ~ stockSize),
                            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
                            variable = case_when(type == "Fishing pressure" ~ fishingPressure,
                                                 type == "Stock Size" ~ stockSize,
                                                 TRUE ~ type),
                            variable = case_when(lineDescription == "Management plan" &
                                                         type == "Fishing pressure" ~ "FMGT",
                                                 lineDescription == "Management plan" &
                                                         type == "Stock Size" ~ "SSBMGT",
                                                 TRUE ~ variable),
                            variable = case_when(
                                    grepl("Fpa", variable) ~ "FPA",
                                    grepl("Bpa", variable) ~ "BPA",
                                    grepl("^Qual*", variable) ~ "SSBQual",
                                    grepl("-", variable) ~ "FQual",
                                    grepl("^BMGT", variable) ~ "SSBMGT",
                                    grepl("MSYBtrigger", variable) ~ "BMSY",
                                    grepl("FMSY", variable) ~ "FMSY",
                                    TRUE ~ variable
                            )) 
        df <- dplyr::filter(df,variable != "-")
        
        df <- dplyr::filter(df, lineDescription != "Management plan")
        df <- dplyr::filter(df, lineDescription != "Qualitative evaluation")
        df <- dplyr::mutate(df,key = paste(StockKeyLabel, lineDescription, type))
        df<- df[order(-df$year),]
        df <- df[!duplicated(df$key), ]
        df<- subset(df, select = -key)
        df<- subset(df, select = c(StockKeyLabel, AssessmentYear, AdviceCategory, lineDescription, type, status))
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        colnames(df2) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize" )
        df2 <-dplyr::mutate(df2, SBL = case_when(FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                                                 FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                                                 FishingPressure == "ORANGE"  |  StockSize == "ORANGE" ~ "RED",
                                                 TRUE ~ "GREY"))
        df2<- subset(df2, select = c(StockKeyLabel, SBL))
        df <- dplyr::left_join(df, df2)
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        colnames(df) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize", "SBL" )
        sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, EcoRegion, FisheriesGuild)
        sid$FisheriesGuild <- tolower(sid$FisheriesGuild)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        df <- merge(df, sid, all = FALSE)
        df
}
