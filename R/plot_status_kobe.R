#' Returns a scatterplot of F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion 
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of stockstatus+catch_current.R
#' @param guild an identifier of the Fisheries guild to plot
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

plot_kobe <- function(x, guild, caption = F, cap_year, cap_month, return_data = T){
        df <- dplyr::filter(x,FisheriesGuild %in% guild) 
        kobe <- ggplot(df, aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                         data_id = StockKeyLabel)) +
                geom_point(aes(color = Status), size = 2,
                           alpha = 0.7, na.rm = TRUE) +
                geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
                geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
                ggrepel::geom_text_repel(aes(label = StockKeyLabel),
                                         segment.size = .25,
                                         force = 5,
                                         size = 2) +
                scale_color_manual(values = c("GREEN" = "#4daf4a",
                                              "RED" = "#e41a1c",
                                              "GREY" = "#d3d3d3")) +
                labs(x = expression(F/F[MSY]),
                     y = expression(SSB/MSY~B[trigger]),
                     caption = "") +
                theme_bw(base_size = 7) +
                theme(legend.position = 'none',
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      plot.caption = element_text(size = 6))
      
        return(kobe)
        
        if(return_data == T){
                kobe_data <-df
                return(kobe_data)
        }
}

