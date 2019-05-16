#' Returns a scatter plot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion 
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of CLD_status
#' @param guild an identifier of the Fisheries guild to plot
#' @param cap_month  the month to be shown in the figure caption, the accession date to SAG usually
#' @param cap_year the year to be shown in the figure caption
#' @param return_data a parameter indicating if the data behind the plot should be returned as a dataframe
#
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
#' plot1 <- plot_CLD_kobe(CLDstatus, guild="Demersal", return_data = TRUE)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export


#find a way to set caption(cap_year, cap_month) being conditional

plot_CLD_kobe <- function(x, guild, caption = T, cap_year, cap_month, return_data = T){
        df <- dplyr::filter(x,FisheriesGuild %in% guild,
                       !is.na(F_FMSY),
                       !is.na(SSB_MSYBtrigger)) 
        df <-dplyr::mutate(df, max_bar = max(catches, landings, discards, na.rm = TRUE),
                       catch_width = ifelse(is.na(catches),
                                            0,
                                            round((catches/(max_bar/1.25) * 100))),
                       landings_width = ifelse(is.na(landings),
                                               0,
                                               round((landings/(max_bar/1.25) * 100))),
                       discards_width = ifelse(is.na(discards),
                                               0,
                                               round((discards/(max_bar/1.25) * 100))),
                       total = ifelse(all(is.na(catches) & is.na(landings)),
                                      NA,
                                      max(catches, landings, na.rm = TRUE))) %>%
                distinct(.keep_all = TRUE)
        labs <- seq(0, max(df$F_FMSY, df$SSB_MSYBtrigger, na.rm = TRUE) + 1)
        plot <- ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                                 data_id = StockKeyLabel)) +
                        ggplot2::geom_point(ggplot2::aes(color = Status ), size = 2,
                                   alpha = 0.7, na.rm = TRUE) +
                        ggplot2::geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
                        ggplot2::geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
                        ggrepel::geom_text_repel(ggplot2::aes(label = StockKeyLabel),
                                                 segment.size = .25,
                                                 force = 5,
                                                 size = 2) +
                        ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                                      "RED" = "#e41a1c",
                                                      "GREY" = "#d3d3d3")) +
                        ggplot2::scale_y_continuous(breaks = labs) +
                        ggplot2::scale_x_continuous(breaks = labs) +
                        ggplot2::coord_equal(xlim = range(labs), ylim = range(labs)) +
                        ggplot2::labs(x = expression(F/F[MSY]),
                             y = expression(SSB/MSY~B[trigger]),
                             caption = "") +
                        ggplot2::theme_bw(base_size = 7) +
                        ggplot2::theme(legend.position = 'none',
                              panel.grid.minor = ggplot2::element_blank(),
                              panel.grid.major = ggplot2::element_blank(),
                              plot.caption = ggplot2::element_text(size = 6))
       
        
        if(caption == T){
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                  cap_month,
                                                  cap_year))
                plot<-ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                               data_id = StockKeyLabel)) +
                        ggplot2::geom_point(ggplot2::aes(color = Status ), size = 2,
                                    alpha = 0.7, na.rm = TRUE) +
                        ggplot2::geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
                        ggplot2::geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
                        ggrepel::geom_text_repel(ggplot2::aes(label = StockKeyLabel),
                                         segment.size = .25,
                                         force = 5,
                                         size = 2) +
                        ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                                       "RED" = "#e41a1c",
                                                       "GREY" = "#d3d3d3")) +
                        ggplot2::scale_y_continuous(breaks = labs) +
                        ggplot2::scale_x_continuous(breaks = labs) +
                        ggplot2::coord_equal(xlim = range(labs), ylim = range(labs)) +
                        ggplot2::labs(x = expression(F/F[MSY]),
                              y = expression(SSB/MSY~B[trigger]),
                              caption = "") +
                        ggplot2::theme_bw(base_size = 7) +
                        ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6))+
                cap_lab
        }
        
         return(plot)
                
        if(return_data == T){
                CLD_kobe_plot_data <-df
                return(CLD_kobe_plot_data)
        }
}
        
