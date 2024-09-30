#' It provides an html file with color symbols corresponding to fishstock status, as well as
#' other information that might be required for an annex table
#' After saving the table in html, open the file in browser. press ctrl+a and ctrl+c, and 
#' then paste the table either in a word document or in an xls spreadsheet. 
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
#' html_annex_table <- html_annex_table(df, ecoregion, year)
#' }
#'
#' @export

library(DT)
library(webshot)
library(dplyr)
library(tidyverse)




format_annex_table_html <- function(df, ecoregion, year) {


  createLink <- function(StockKeyLabel, AssessmentYear) {
  paste0("<a href='","https://www.ices.dk/sites/pub/Publication%20Reports/Advice/",AssessmentYear,"/", AssessmentYear,"/", StockKeyLabel,".pdf","'>", StockKeyLabel,"</a>")
}
  # The paths to the icons
  green_icon <- '<img src="D:/Profile/Documents/R_Projects/icesFO/inst/symbols/green_check.png" width=30 height=30>'
  grey_icon <- '<img src="D:/Profile/Documents/R_Projects/icesFO/inst/symbols/grey_q.png" width=30 height=30>'
  orange_icon <- '<img src="D:/Profile/Documents/R_Projects/icesFO/inst/symbols/orange_oh.png" width=30 height=30>'
  red_icon <- '<img src="D:/Profile/Documents/R_Projects/icesFO/inst/symbols/red_cross.png" width=30 height=30>'

  df <- df %>%
    mutate_all(funs(str_replace(., "GREEN", green_icon))) %>%
    
    mutate_all(funs(str_replace(., "GREY", grey_icon))) %>%
    
    mutate_all(funs(str_replace(., "ORANGE", orange_icon))) %>%
    
    mutate_all(funs(str_replace(., "RED", red_icon)))

  df <- select(df, -"Ecoregion")

  df$url <- createLink(df$StockKeyLabel, df$AssessmentYear) 
  df <- select(df, -"StockKeyLabel")
  df <- df %>% relocate(url, .before = StockKeyDescription)
  df <- df %>% rename(StockKeyLabel = url)

  dtable <- datatable(
    df,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      dom = "t",
      pageLength = 100,
      width = "100%"
    )
  )
  html <- sprintf("%s_%s_FO_SAG_annex_table.html",year, ecoregion )
  saveWidget(dtable, html)
  # return(df)
}
