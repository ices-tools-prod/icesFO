#' Format the SAG status data for its use as an annex table.
#'
#' It provides an excel file with color names per status, as well as
#' other information that might be required for an annex table
#'
#' @param df a dataframe output from format_sag_status.
#' @param year the active year
#'
#' @return A data frame
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
#' @export

format_annex_table <- function(df, year) {
  sid <- load_sid(year)
  sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
  sid <- dplyr::select(sid,
                       StockKeyLabel,
                       StockKeyDescription,
                       SpeciesScientificName,
                       SpeciesCommonName,
                       FisheriesGuild,
                       DataCategory)
  sid <- sid %>% filter(StockKeyLabel %in% df$StockKeyLabel)
  df <- dplyr::left_join(df, sid, by = "StockKeyLabel")
  df <- df[c(1,10,11,12,13,14,2,3,4,5,8,6,7)]
  df <- dplyr::mutate(df,
                      D3C1 = FishingPressure,
                      D3C2 = StockSize,
                      GES = dplyr::case_when(
                          FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                          FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                          FishingPressure == "GREY" | StockSize == "GREY" ~ "GREY",
                          TRUE ~ "GREY"))


  df$StockKeyDescription <- gsub("\\s*\\([^\\)]+\\)","",df$StockKeyDescription, perl = TRUE)

  df
}
