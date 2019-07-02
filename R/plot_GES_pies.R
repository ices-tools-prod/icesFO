#' Returns an ordered plot of catch bars colored according to 
#' F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion 
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of format_sag_status.R
#' @param y a dataframe output of stockstatus_CLD_current.R
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

plot_GES_pies <- function(x, y, cap_month = "November",
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
                     "qual_RED" = "#d93b5c",
                     "qual_GREEN" = "#00B28F")
        
        
        df_stock <- df %>%filter (lineDescription == "Maximum sustainable yield")%>%
                select(StockKeyLabel,
                       FishingPressure,
                       StockSize) %>%
                tidyr::gather(Variable, Colour, FishingPressure:StockSize, factor_key = TRUE) 
        df2 <- df_stock %>% group_by(Variable, Colour) %>%
                summarize(COUNT = n())%>%
                tidyr::spread(Colour, COUNT)
        df2[is.na(df2)] <- 0
        
        df3 <-y %>% filter(StockKeyLabel %in% df_stock$StockKeyLabel) %>%
                mutate(CATCH = ifelse(is.na(catches) & !is.na(landings),
                                         landings,
                                         catches)) %>%
                select(c(StockKeyLabel, CATCH))
        df4 <- df_stock %>% left_join(df3)
        df4[is.na(df4)] <- 0
        df4 <- df4%>% group_by(Variable, Colour) %>%
                summarize(CATCH = sum(CATCH))%>%
                tidyr::spread(Colour, CATCH)
        df4 <- df4 %>% tidyr::gather(Color, Catch, GREEN:RED, factor_key = TRUE)
        df2 <- df2 %>% tidyr::gather(Color, Stocks, GREEN:RED, factor_key = TRUE)
        df5 <- merge(df2,df4)
        df5[is.na(df5)] <- 0
        tot <- sum(df5$Catch)/2
        stocks <- sum(df5$Stocks)/2
        df5 <- df5 %>% tidyr::gather(Metric, Value, Stocks:Catch)
        df5 <- df5 %>% group_by(Metric) %>% mutate(sum = sum(Value)/2)
        # df5 <- df5 %>% group_by(Metric) %>% mutate(max = max(Value)/2)
        
        df5$fraction <- ifelse(df5$Metric == "Stocks", (df5$Value*tot)/stocks, df5$Value)
        
        p1 <- ggplot2::ggplot(data = df5, ggplot2::aes(x = "", y = fraction, fill = Color)) +
                ggplot2::geom_bar(stat = "identity", width = 1) +
                ggplot2::geom_text(ggplot2::aes(label = scales::comma(Value)),
                          position = ggplot2::position_stack(vjust = 0.5),
                          size = 3) +
                ggplot2::geom_text(ggplot2::aes(label = paste0("total = ", sum) ,x = 0, y = 0), size = 2)+
                ggplot2::scale_fill_manual(values = colList) +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(panel.grid = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      legend.position="none") +
                ggplot2::theme(axis.text = ggplot2::element_blank(),
                      axis.ticks = ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 6)) +
                cap_lab +
                ggplot2::coord_polar(theta = "y") +
                ggplot2::facet_grid(Metric ~ Variable)
        
        if(return_data == T){
                df5
        }else{
                p1
        }
}
