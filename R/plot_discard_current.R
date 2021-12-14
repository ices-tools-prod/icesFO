#' Returns a plot of discard rate by fish category for an ecoregion in the last assessment year.
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of CLD_trends
#' @param year the year required
#' @param position_letter is this figure "a)", or "b)" etc.
#' @param caption logical for whether there should be a caption
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
#' \code{\link{plot_discard_current}} a plot of landings and discards by fish category in the last year for an ecoregion.
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

plot_discard_current <- function(x, year, position_letter = "c)",
                                 caption = TRUE, cap_year, cap_month,
                                 return_data = FALSE){
  df <- dplyr::filter(x,Year %in% seq(year-5, year -1))
  df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,AssessmentYear,FisheriesGuild))
  df <- dplyr::left_join(df,df2,
                         by = c("Year", "StockKeyLabel", "AssessmentYear","FisheriesGuild"))
  df3 <- dplyr::select(df, StockKeyLabel, Year, discards, AssessmentYear)
  df3 <- unique(df3)
  df3 <- tibble::rowid_to_column(df3)
  df3 <- dplyr::group_by(df3,StockKeyLabel)
  df3 <- tidyr::spread(df3,Year, discards)
  # df3<- dplyr::mutate(df3,`2017` = ifelse(AssessmentYear == 2017 &
  #                                                 is.na(`2017`) &
  #                                                 !is.na(`2016`),
  #                                         `2016`,
  #                                         `2017`))
  df3 <- tidyr::gather(df3,Year, discards, 4:ncol(df3))
  df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                       discards = as.numeric(discards))
  df5 <- dplyr::select(df,-discards,
                       -landings)
  df5 <- dplyr::left_join(df5,df3, by = c("Year", "StockKeyLabel", "AssessmentYear"))
  # df5 <- dplyr::left_join(df5,df4, by = c("Year", "StockKeyLabel", "AssessmentYear"))
  df5 <- dplyr::group_by(df5,Year, FisheriesGuild)
  df5 <- dplyr::summarize(df5,guildLandings = sum(catches, na.rm = TRUE)/ 1000,
                    guildDiscards = sum(discards, na.rm = TRUE)/ 1000)
  # df5 <- dplyr::summarize(df5,guildLandings = sum(catches, na.rm = TRUE),
   #                        guildDiscards = sum(discards, na.rm = TRUE))
   # 

  # df5 <- dplyr::mutate(df5,guildRate = guildDiscards/ (guildLandings + guildDiscards))
  df5 <- tidyr::gather(df5,variable, value, -Year, -FisheriesGuild)

  # df5 <- dplyr::filter(df5,!variable %in% c("guildDiscards", "guildLandings"))
  # df5 <- dplyr::filter(df5,Year == year - 1)

  # df5_order <- dplyr::group_by(df5,FisheriesGuild) %>%
  #         summarize(total = sum(value, na.rm = TRUE)) %>%
  #         arrange(-total) %>%
  #         dplyr::ungroup()
  # df5_order <- dplyr::mutate(df5_order,FisheriesGuild = factor(FisheriesGuild, FisheriesGuild))

  # df5$FisheriesGuild <- factor(df5$FisheriesGuild,
  #                                 levels = df5_order$FisheriesGuild[order(df5_order$total)])
  plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                          ggplot2::aes(x = reorder(FisheriesGuild, value), y = value, fill = variable)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
          ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
          ggplot2::coord_flip() +
          ggplot2::theme_bw(base_size = 8) +
          ggplot2::theme(legend.position = "none",
                plot.caption = ggplot2::element_text(size = 6),
                panel.grid = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(colour = NA)) +
          ggplot2::labs(x = "", y = "Discards and landings(thousand tonnes)",title = position_letter)

  if(caption == TRUE) {
    cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                               cap_month,
                                               cap_year))
    plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                            ggplot2::aes(x = reorder(FisheriesGuild, value), y = value, fill = variable)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
            ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw(base_size = 8) +
            ggplot2::theme(legend.position = "none",
                           plot.caption = ggplot2::element_text(size = 6),
                           panel.grid = ggplot2::element_blank(),
                           legend.key = ggplot2::element_rect(colour = NA)) +
            ggplot2::labs(x = "", y = "Discards and landings(thousand tonnes)",title = position_letter)+
            cap_lab
  }

  if(return_data == T){
          df5
  }else{
          plot
  }

}
