#' Plotting time series of F/Fmsy and SSB/MSY B trigger for all stocks with data available by guild.
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of stock_trends function
#' @param guild an identifier of the Fisheries guild to plot
#' @param cap_month  the month to be shown in the figure caption, the accession date to SAG usually
#' @param cap_year the year to be shown in the figure caption
#' @param return_data a parameter indicating if the data behind the plot should be returned as a dataframe
#' 
#'
#' @return A plot
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
#' plot1 <- plot_stock_trends(trends2018)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export


plot_stock_trends <- function(x, guild, cap_year, cap_month, return_data = FALSE){
        df<- dplyr::filter(x,FisheriesGuild == guild)
        adj_names <- sort(setdiff(unique(df$StockKeyLabel), "MEAN"))
        values <- ggthemes::tableau_color_pal('Tableau 20')(length(adj_names))
        legend_pos <- "bottom"
        names(values) <- adj_names
        values <- c(values, c(MEAN = "black"))
        plot_title <- guild
        cap_lab <- ggplot2::labs(title = plot_title, x = "Year", y = "",
                caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                  cap_month,
                                  cap_year))
        duplicates <- dplyr::group_by(df,Value)
        duplicates <- dplyr::filter(duplicates, dplyr::n()>1)
        duplicates <- dplyr::filter(duplicates,StockKeyLabel == "MEAN")
        df <- dplyr::anti_join(df,duplicates)
        df <- dplyr::filter(df,Metric %in% c("F_FMSY", "SSB_MSYBtrigger"))
        df$Metric[which(df$Metric == "F_FMSY")] <- "F/F[MSY]"
        df$Metric[which(df$Metric == "SSB_MSYBtrigger")] <- "SSB/MSY~B[trigger]"
        mean <- dplyr::filter(df, StockKeyLabel == "MEAN")
        df2 <- dplyr::filter(df,StockKeyLabel != "MEAN")
        
        plot <- ggplot2::ggplot(df2, ggplot2::aes(x = Year, y = Value,
                      color = StockKeyLabel,
                      fill = StockKeyLabel)) +
                ggplot2::geom_hline(yintercept = 1, col = "grey60") +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::scale_color_manual(values = values) +
                ggplot2::scale_fill_manual(values = values) +
                ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                ggplot2::guides(fill = FALSE) +
                ggplot2::theme(legend.position = legend_pos,
                        strip.text = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
                        strip.background = ggplot2::element_blank(),
                        strip.placement = "outside",
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        legend.key = ggplot2::element_rect(colour = NA),
                        legend.title = ggplot2::element_blank(),
                        plot.caption = ggplot2::element_text(size = 6)) +
                cap_lab +
                ggplot2::facet_wrap(~ Metric, scales = "free_y", labeller = ggplot2::label_parsed, strip.position = "left", ncol = 1, nrow = 2)
        plot <- plot + ggplot2::geom_line(data = df,alpha = 0.8)
        plot <- plot + ggplot2::geom_line(data = mean,
                               alpha = 0.9, size = 1.15)
        
        if(return_data == T){
                df
        }else{
                plot
        }
}
