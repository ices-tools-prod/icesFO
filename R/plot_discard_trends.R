#' Returns a plot of discard rate trends by fish category for an ecoregion.
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of CLD_trends
#' @param year year required
#' @param caption logial flag
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
#' \code{\link{plot_discard_current}} a plot of Landings and Discards by fish category in the last year for an ecoregion.
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' plot1 <- plot_discard_trends(CLDtrends, caption = T,cap_year, cap_month, return_data = TRUE)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export


#find a way to set caption(cap_year, cap_month) being conditional

plot_discard_trends <- function(x, year, caption = FALSE, cap_year, cap_month, return_data = FALSE){
        df <- dplyr::filter(x,Year %in% seq(2015, year -1))
        df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,FisheriesGuild))
        df <- dplyr::left_join(df,df2,
                          by = c("Year", "StockKeyLabel", "FisheriesGuild"))
        df3 <- dplyr::select(df, StockKeyLabel, Year, Discards)
        df3 <- unique(df3)
        df3 <- tibble::rowid_to_column(df3)
        df3 <- tidyr::spread(df3,Year, Discards)
        # df3<- dplyr::mutate(df3,`2017` = ifelse(AssessmentYear == 2017 &
        #                                        is.na(`2017`) &
        #                                        !is.na(`2016`),
        #                                `2016`,
        #                                `2017`))
        df3 <- tidyr::gather(df3,Year, Discards, 4:ncol(df3))
        df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                       Discards = as.numeric(Discards))

        df4<- dplyr::select(df,StockKeyLabel, Year, Landings)
        df4 <- unique(df4)
        df4 <- tibble::rowid_to_column(df4)
        df4 <- dplyr::group_by(df4,StockKeyLabel)
        df4 <- tidyr::spread(df4,Year, Landings)
        # df4 <- dplyr::mutate(df4,`2017` = ifelse(AssessmentYear == 2017 &
        #                                        is.na(`2017`) &
        #                                        !is.na(`2016`),
        #                                `2016`,
        #                                `2017`))
        df4 <- tidyr::gather(df4,Year, Landings, 4:ncol(df4))
        df4 <- dplyr::mutate(df4,Year = as.numeric(Year),
                       Landings = as.numeric(Landings))
        df5 <- dplyr::select(df,-Discards,
                       -Landings)
        df5 <- dplyr::left_join(df5,df3, by = c("Year", "StockKeyLabel"))
        df5 <- dplyr::left_join(df5,df4, by = c("Year", "StockKeyLabel"))
        df5 <- dplyr::group_by(df5,Year, FisheriesGuild)
        df5 <- dplyr::summarize(df5, guildLandings = sum(Landings, na.rm = TRUE)/ 1000,
                          guildDiscards = sum(Discards, na.rm = TRUE)/ 1000)

        df5 <- dplyr::mutate(df5,guildRate = guildDiscards/ (guildLandings + guildDiscards))
        df5 <- tidyr::gather(df5,variable, value, -Year, -FisheriesGuild)
        df5 <- dplyr::filter(df5,!variable %in% c("guildDiscards", "guildLandings"))
        df5 <- dplyr::filter(df5,!is.na(value))
        df5 <- dplyr::filter(df5, value > 0)
        df6 <- df5 %>% group_by(FisheriesGuild) %>% slice(which.max(Year))
        plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                               ggplot2::aes(x = Year,
                                   y = value,
                                   color = FisheriesGuild)) +
                ggplot2::geom_line() +
                ggrepel::geom_label_repel(data = df6,
                                          ggplot2::aes(label = FisheriesGuild,
                                              color = FisheriesGuild,
                                              fill = FisheriesGuild),
                                          nudge_x = 1,
                                          label.size = 0.2,
                                          segment.size = 0.25,
                                          size = 2,
                                          color = 'white',
                                          force = 2,
                                          segment.color = 'grey60') +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_x_continuous(breaks = seq(min(df5$Year, na.rm = TRUE),
                                                max(df5$Year, na.rm = TRUE), by = 1)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = max(df5$Year, na.rm = TRUE),
                                 y = -Inf, yend = -Inf), color = "grey50") +
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                 x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(df5$Year, na.rm = TRUE), year + 1)) +
                ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
                ggplot2::scale_fill_brewer(type = "qual", palette = "Set2") +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = "none",
                      plot.caption = ggplot2::element_text(size = 6),
                      panel.grid = ggplot2::element_blank(),
                      legend.key = ggplot2::element_rect(colour = NA)) +
                ggplot2::labs(x = "Year", y = "Discard rate", caption = "", title = "a)")

        if(caption == TRUE){
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                           cap_month,
                                                           cap_year))
                plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                                        ggplot2::aes(x = Year,
                                                     y = value,
                                                     color = FisheriesGuild)) +
                        ggplot2::geom_line() +
                        ggrepel::geom_label_repel(data = df6,
                                                  ggplot2::aes(label = FisheriesGuild,
                                                               color = FisheriesGuild,
                                                               fill = FisheriesGuild),
                                                  nudge_x = 1,
                                                  label.size = 0.2,
                                                  segment.size = 0.25,
                                                  size = 2,
                                                  color = 'white',
                                                  force = 2,
                                                  segment.color = 'grey60') +
                        ggplot2::scale_y_continuous(labels = scales::percent) +
                        ggplot2::scale_x_continuous(breaks = seq(min(df5$Year, na.rm = TRUE),
                                                                 max(df5$Year, na.rm = TRUE), by = 1)) +
                        ggplot2::geom_segment(ggplot2::aes(x = min(df5$Year), xend = max(df5$Year, na.rm = TRUE),
                                                           y = -Inf, yend = -Inf), color = "grey50") +
                        ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                                           x = min(df5$Year), xend = -Inf), color = "grey50")+
                        ggplot2::expand_limits(x = c(min(df5$Year, na.rm = TRUE), year + 1)) +
                        ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
                        ggplot2::scale_fill_brewer(type = "qual", palette = "Set2") +
                        ggplot2::theme_bw(base_size = 9) +
                        ggplot2::theme(legend.position = "none",
                                       plot.caption = ggplot2::element_text(size = 6),
                                       panel.grid = ggplot2::element_blank(),
                                       legend.key = ggplot2::element_rect(colour = NA)) +
                        ggplot2::labs(x = "Year", y = "Discard rate", caption = "", title = "a)")+
                        cap_lab

        }

        if(return_data == TRUE){
                df5
        }else{
                plot
        }
}
