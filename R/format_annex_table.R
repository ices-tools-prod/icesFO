#' Format the SAG status data for its use as an annex table.
#'
#' It provides an excel file with symbols per status, as well as 
#' other information that might be required for an annex table
#'
#' @param x a dataframe output from format_sag_status.
#' @param year the active year
#' @param return_data logical whether to return the data behind
#'        the table
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

format_annex_table <- function(x, year, return_data = FALSE){
        df <-x
        sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,StockKeyDescription, SpeciesScientificName,SpeciesCommonName,
                             FisheriesGuild, DataCategory)
        df <- merge(df, sid, all = FALSE)
        df <- df[c(1,10,11,12,13,2,3,4,5,8,6,7)]
        df <- dplyr::mutate(df,D3C1 = FishingPressure,
                            D3C2 = StockSize,
                            GES = dplyr::case_when(FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                                            FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                                            FishingPressure == "GREY"  |  StockSize == "GREY" ~ "GREY",
                                            TRUE ~ "GREY"))
        
        
        df$StockKeyDescription <- gsub("\\s*\\([^\\)]+\\)","",df$StockKeyDescription, perl=TRUE)
        
        
        # grey.path <- "~/git/ices-dk/fisheryO/inst/symbols/grey_q.png"
        # red.path <- "~/git/ices-dk/fisheryO/inst/symbols/red_cross.png"
        # green.path <- "~/git/ices-dk/fisheryO/inst/symbols/green_check.png"
        # orange.path <- "~/git/ices-dk/fisheryO/inst/symbols/orange_oh.png"
        
        
        grey.path <- system.file("symbols", "grey_q.png", package = "icesFO")
        red.path <- system.file("symbols", "red_cross.png", package = "icesFO")
        green.path <- system.file("symbols", "green_check.png", package = "icesFO")
        # orange.path <- system.file("symbols", "orange_oh.png", package = "icesFO")
        
        merge_custom <- function(ft, x, columns){
                z <- rle(x)
                rows_at <- cumsum(z$lengths) - z$lengths + 1
                
                for(i in seq_along(rows_at)){
                        for(j in columns)
                                ft <- flextable::merge_at(x = ft, i = seq( rows_at[i], rows_at[i] + z$lengths[i] - 1), j = j)
                }
                
                ft
        }
        
        tab <- flextable::flextable(df) %>% merge_custom(x = df$StockKeyLabel, columns = 1:8)
        
        
        #pending: change display with compose
        tab <- flextable::display(tab, i = ~ D3C1 == "GREEN", col_key = "D3C1", pattern = "{{add_icon}}",
                           formatters = list(add_icon ~ flextable::as_image(D3C1, src = green.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ D3C1 == "RED", col_key = "D3C1", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(D3C1, src = red.path, width = .15, height = .15))) 
                # tab <-   flextable::display(tab, i = ~ D3C1 == "ORANGE", col_key = "D3C1", pattern = "{{add_icon}}",
                #                    formatters = list(add_icon ~ flextable::as_image(D3C1, src = orange.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ D3C1 == "GREY", col_key = "D3C1", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(D3C1, src = grey.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ D3C2 == "GREEN", col_key = "D3C2", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(D3C2, src = green.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ D3C2 == "RED", col_key = "D3C2", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(D3C2, src = red.path, width = .15, height = .15))) 
                # tab <-  flextable::display(tab, i = ~ D3C2 == "ORANGE", col_key = "D3C2", pattern = "{{add_icon}}",
                #                    formatters = list(add_icon ~ flextable::as_image(D3C2, src = orange.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ D3C2 == "GREY", col_key = "D3C2", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(D3C2, src = grey.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ SBL == "GREEN", col_key = "SBL", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(SBL, src = green.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ SBL == "RED", col_key = "SBL", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(SBL, src = red.path, width = .15, height = .15))) 
                # tab <-  flextable::display(tab, i = ~ SBL == "ORANGE", col_key = "SBL", pattern = "{{add_icon}}",
                #                    formatters = list(add_icon ~ flextable::as_image(SBL, src = orange.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ SBL == "GREY", col_key = "SBL", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(SBL, src = grey.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ FishingPressure == "GREEN", col_key = "FishingPressure", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(FishingPressure, src = green.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ FishingPressure == "RED", col_key = "FishingPressure", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(FishingPressure, src = red.path, width = .15, height = .15))) 
                # tab <-  flextable::display(tab, i = ~ FishingPressure == "ORANGE", col_key = "FishingPressure", pattern = "{{add_icon}}",
                #                    formatters = list(add_icon ~ flextable::as_image(FishingPressure, src = orange.path, width = .15, height = .15))) 
                tab <- flextable::display(tab, i = ~ FishingPressure == "GREY", col_key = "FishingPressure", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(FishingPressure, src = grey.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ StockSize == "GREEN", col_key = "StockSize", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(StockSize, src = green.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ StockSize == "RED", col_key = "StockSize", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(StockSize, src = red.path, width = .15, height = .15))) 
                # tab <-  flextable::display(tab, i = ~ StockSize == "ORANGE", col_key = "StockSize", pattern = "{{add_icon}}",
                #                    formatters = list(add_icon ~ flextable::as_image(StockSize, src = orange.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ StockSize == "GREY", col_key = "StockSize", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(StockSize, src = grey.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ GES == "GREEN", col_key = "GES", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(GES, src = green.path, width = .15, height = .15)))
                tab <-  flextable::display(tab, i = ~ GES == "RED", col_key = "GES", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(GES, src = red.path, width = .15, height = .15)))
                # tab <-  flextable::display(tab, i = ~ GES == "ORANGE", col_key = "GES", pattern = "{{add_icon}}",
                #                    formatters = list(add_icon ~ flextable::as_image(GES, src = orange.path, width = .15, height = .15))) 
                tab <-  flextable::display(tab, i = ~ GES == "GREY", col_key = "GES", pattern = "{{add_icon}}",
                                   formatters = list(add_icon ~ flextable::as_image(GES, src = grey.path, width = .15, height = .15)))
                tab <- flextable::autofit(tab)
                tab <- flextable::align(tab, align = "center")
                std_border = officer::fp_border(color="gray")
                tab <- flextable::hline(tab, part="all", border = std_border )
                
                doc <- officer::read_docx()
                doc <- flextable::body_add_flextable(doc, value = tab)
        
                
                if(return_data == T){
                        df
                }else{
                        doc
                }
}
