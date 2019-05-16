#' Returns an ordered plot of catch bars colored according to 
#' F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion 
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of format_sag_status.R
#' @param cap_month  the month to be shown in the figure caption, the accession date to SAG usually
#' @param cap_year the year to be shown in the figure caption
#' @param return_data a parameter indicating if the data behind the plot should be returned as a dataframe
#'
#' @return A plot
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{plot_CLD_bar}} Stock status relative to reference points. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' plot1 <- plot_CLD_bar(CLDstatus, guild="Demersal", return_data = TRUE)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export


#find a way to set caption(cap_year, cap_month) being conditional

plot_status_prop_pies <- function(x, cap_month = "November",
                         cap_year = "2018",
                         return_data = FALSE) {
        df <- x
        cap_lab <- ggplot2::labs(title = "", x = "", y = "",
                        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen",
                                          cap_month,
                                          cap_year))
        colList <- c("GREEN" = "#00B26D",
                     "GREY" = "#d3d3d3",
                     "ORANGE" = "#ff7f00",
                     "RED" = "#d93b1c",
                     "qualRED" = "00000")
        
        
        df_stock <- df %>%
                select(StockKeyLabel,
                       FisheriesGuild,
                       lineDescription,
                       FishingPressure,
                       StockSize,
                       SBL) %>%
                tidyr::gather(Variable, Colour, FishingPressure:SBL, factor_key = TRUE) 
        df2 <- df_stock %>% group_by(FisheriesGuild, lineDescription, Variable, Colour) %>%
                summarize(COUNT = n())%>%
                tidyr::spread(Colour, COUNT)
        df2[is.na(df2)] <- 0
        df3 <- subset(df2,select =-c(FisheriesGuild))
        df3 <- df3%>% group_by(lineDescription, Variable)%>% summarise_each(funs(sum))
        df3$FisheriesGuild <- "total"
        df2 <- rbind(df2,df3)
        df2$lineDescription <- gsub("Maximum sustainable yield","MSY", df2$lineDescription)
        df2$lineDescription <- gsub("Precautionary approach", "PA", df2$lineDescription)
        df2$header <- paste0(df2$Variable, "\n" , df2$lineDescription)
        
        df2 <- df2 %>% tidyr::gather(colour, value,GREEN:RED, factor_key = TRUE)
        df2 <- df2 %>% filter(value > 0)
        
        tot <- df2 %>% filter(FisheriesGuild == "total")
        tot <- tot %>% group_by(header) %>% mutate(tot = sum(value))
        max <- unique(tot$tot)
        df2 <- df2 %>% group_by(FisheriesGuild, header) %>% mutate(sum = sum(value))
        df2$fraction <- df2$value*max/df2$sum
        
        p1 <- ggplot(data = df2, aes(x = "", y = fraction, fill = colour)) +
                geom_bar(stat = "identity", width = 1) +
                geom_text(aes(label = value),
                          position = position_stack(vjust = 0.5),
                          size = 3) +
                scale_fill_manual(values = colList) +
                theme_bw(base_size = 9) +
                theme(panel.grid = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      legend.position="none") +
                theme(axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      strip.background = element_blank(),
                      plot.caption = element_text(size = 6)) +
                cap_lab +
                coord_polar(theta = "y", direction = 1) +
                facet_grid(FisheriesGuild ~ header)
        
        p1
}
