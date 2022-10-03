#' Returns an ordered plot of catch bars colored according to 
#' F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion 
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of stockstatus+catch_current.R
#' @param guild an identifier of the Fisheries guild to plot
#' @param caption logical flag
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

plot_CLD_bar <- function(x, guild, caption = TRUE, cap_year, cap_month, return_data = FALSE){
        if(guild == "All"){
                df <-x
        }else(df <- dplyr::filter(x,FisheriesGuild %in% guild))
        df <- dplyr::mutate(df,total = ifelse(all(is.na(Catches) & is.na(Landings)),
                                      NA,
                                      max(Catches, Landings, na.rm = TRUE))) 
        df <- dplyr::ungroup (df)
        df <- dplyr::mutate(df,StockKeyLabel = forcats::fct_reorder(StockKeyLabel, total))
        
        plot <- ggplot2::ggplot(df, ggplot2::aes(x =StockKeyLabel, y = Catches/1000)) +
               ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Catches/1000,
                                 xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
               ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Landings/1000,
                                 xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
               ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Catches/1000,
                                                  fill = Status), color = "grey50",
                           shape = 24, size = 2, alpha = 0.8, na.rm = TRUE) +
               ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Landings/1000,
                                                  fill = Status), color = "grey50",
                           shape = 21, size = 2, alpha = 0.8, na.rm = TRUE) +
               ggplot2::scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                             "RED" = "#e41a1c",
                                             "GREY" = "#d3d3d3")) +
               ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                              "RED" = "#e41a1c",
                                              "GREY" = "#d3d3d3")) +
               ggplot2::coord_equal() +
               ggplot2::coord_flip() +
               ggplot2::theme_bw(base_size = 7) + 
               ggplot2::labs(y = expression("Catch and Landings(thousand tonnes)"))+
               ggplot2::theme(legend.position = 'none',
                      plot.caption = ggplot2::element_text(size = 6),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major.y = ggplot2::element_blank(),
                      panel.grid.major.x = ggplot2::element_line( size = 0.1, color = "grey80"))
        
        
        if(caption == T){
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                           cap_month,
                                                           cap_year))
                plot <- ggplot2::ggplot(df, ggplot2::aes(x =StockKeyLabel, y = Catches/1000)) +
                        ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Catches/1000,
                                                           xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
                        ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Landings/1000,
                                                           xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
                        ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Catches/1000,
                                                                            fill = Status), color = "grey50",
                                            shape = 24, size = 2, alpha = 0.8, na.rm = TRUE) +
                        ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Landings/1000,
                                                                            fill = Status), color = "grey50",
                                            shape = 21, size = 2, alpha = 0.8, na.rm = TRUE) +
                        ggplot2::scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                                              "RED" = "#e41a1c",
                                                              "GREY" = "#d3d3d3")) +
                        ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                                               "RED" = "#e41a1c",
                                                               "GREY" = "#d3d3d3")) +
                        ggplot2::coord_equal() +
                        ggplot2::coord_flip() +
                        ggplot2::theme_bw(base_size = 7) + 
                        ggplot2::labs(y = expression("Catch and Landings(thousand tonnes)"))+
                        ggplot2::theme(legend.position = 'none',
                                       plot.caption = ggplot2::element_text(size = 6),
                                       panel.grid.minor = ggplot2::element_blank(),
                                       panel.grid.major.y = ggplot2::element_blank(),
                                       panel.grid.major.x = ggplot2::element_line( size = 0.1, color = "grey80"))+
                        cap_lab
        }
        
        
        if(return_data == T){
                df
        }else{
                plot
        }
}

