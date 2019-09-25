#' Returns a scatterplot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
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
#' \code{\link{plot_kobe}} Stock status relative to reference points. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' plot1 <- plot_kobe(CLDstatus, guild="Demersal", return_data = TRUE)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export


#find a way to set caption(cap_year, cap_month) being conditional

plot_kobe <- function(x, guild, caption = FALSE, cap_year, cap_month, return_data = FALSE){
        if(guild == "All"){
                df <-x
        }else(df <- dplyr::filter(x,FisheriesGuild %in% guild))
        xmax = max(df$F_FMSY, na.rm = TRUE)
        ifelse(xmax < 3, xmax2 <- 3, xmax2 <- (xmax + 0.5))
        ymax = max(df$SSB_MSYBtrigger, na.rm = TRUE)
        ifelse(ymax < 3, ymax2 <- 3, ymax2 <- (ymax + 0.5))
        kobe <- ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                         data_id = StockKeyLabel)) +
                ggplot2::coord_cartesian(xlim = c(0, xmax2), ylim = c(0, ymax2))+
                ggplot2::geom_point(ggplot2::aes(color = Status), size = 2,
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
                ggplot2::labs(x = expression(F/F[MSY]),
                     y = expression(SSB/MSY~B[trigger]),
                     caption = "") +
                ggplot2::theme_bw(base_size = 7) +
                ggplot2::theme(legend.position = 'none',
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 6))
      
        
        if(return_data == T){
                df
        }else{
                kobe
        }
}
