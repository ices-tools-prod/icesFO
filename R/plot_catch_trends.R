#' Returns an ordered plot of catch bars colored according to
#' F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
#' by fish category and ecoregion
#'
#' Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
#' each stock in the Ecoregion, according to the last assessment (relative to the set year)
#'
#' @param x a dataframe output of stockstatus+catch_current.R
#' @param type COMMON_NAME, COUNTRY or GUILD
#' @param line_count number of lines to show
#' @param plot_type either line or area
#' @param preliminary_catches logical flag
#' @param official_catches_year year required
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

plot_catch_trends <- function(x,type = c("COMMON_NAME", "COUNTRY", "GUILD"),
                              line_count = 10,
                              plot_type = c("line", "area"),
                              preliminary_catches = TRUE,
                              official_catches_year = cap_year-1,
                              return_data = FALSE) {
        capyear <- official_catches_year-1
        capyear <- as.character(capyear)
        cap_lab <-ggplot2::labs(x = "",
                       y = "Landings (thousand tonnes)",
                       caption = sprintf(paste0("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-",capyear, "\n ICES, Copenhagen.")))


        df <- dplyr::rename(x, type_var = setNames(type , "type_var"))
        df <- dplyr::rename(df, type_var= type_var...type_var)

        if(type == "COMMON_NAME"){
        df$type_var[which(df$type_var == "Angler(=Monk)")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Monkfishes nei")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Atlantic herring")] <- "herring"
        df$type_var[which(df$type_var == "Atlantic cod")] <- "cod"
        df$type_var[which(df$type_var == "Anglerfishes nei")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Megrims nei")] <- "Megrim"
        df$type_var[which(df$type_var == "Norway lobster")] <- "Nephrops"
        df <- dplyr::mutate(df,type_var = gsub("European ", "", type_var),
                              type_var = gsub("Sandeels.*", "sandeel", type_var),
                              type_var = gsub("Finfishes nei", "undefined finfish", type_var),
                              type_var = gsub("Blue whiting.*", "blue whiting", type_var),
                              type_var = gsub("Saithe.*", "saithe", type_var),
                              type_var = ifelse(grepl("Norway", type_var),
                                                type_var,
                                                tolower(type_var)))
                              }

        plot <- dplyr::group_by(df, type_var)
        plot <- dplyr::summarise(plot,typeTotal = sum(VALUE, na.rm = TRUE))
        plot <- dplyr::arrange(plot, -typeTotal)
        plot <- dplyr::filter(plot, typeTotal >= 1)
        plot <- dplyr::mutate(plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal)))
        plot <- dplyr::inner_join(plot,df, by = "type_var")

        plot$RANK<-as.numeric(plot$RANK)

        plot <- dplyr::mutate(plot, type_var = replace(type_var, RANK > line_count, "other"))
        plot <- dplyr::group_by(plot,type_var, YEAR)
        plot <- dplyr::summarise(plot, typeTotal= sum(VALUE, na.rm = TRUE) / 1000)
        plot <- dplyr::filter(plot,!is.na(YEAR))

        plot <- rbind(plot[!plot$type_var == "other",],
                           plot[plot$type_var == "other",])

        colList <- ggthemes::tableau_color_pal('Tableau 20')(line_count + 1)

        order <- dplyr::group_by(plot,type_var)
        order <- dplyr::summarise(order,total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order,-total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))

        plot$type_var <- factor(plot$type_var,
                                     levels = order$type_var[order(order$total)])

        myColors <- colList[1:length(unique(plot$type_var))]
        names(myColors) <- levels(plot$type_var)
        myColors["other"] <- "#7F7F7F"

        pl <- ggplot2::ggplot(dplyr::ungroup(plot), ggplot2::aes(x = YEAR, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(plot$YEAR),
                                                max(plot$YEAR), by = 10)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = max(plot$YEAR), y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(plot$YEAR), max(plot$YEAR) + 20)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                      plot.caption = ggplot2::element_text(size = 6),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank())

        if(plot_type == "area") {
                cumPlot <- dplyr::filter(plot,YEAR == max(YEAR, na.rm = TRUE))
                cumPlot <- dplyr::ungroup(cumPlot)
                cumPlot <- dplyr::arrange(cumPlot, dplyr::desc(type_var))
                cumPlot <- dplyr::mutate(cumPlot, cs = cumsum(as.numeric(typeTotal)), # cumulative sum
                               mp = lag(cs, order_by = dplyr::desc(type_var)), # midpoint
                               mp = ifelse(is.na(mp), 1, mp)) # midpoint
                cumPlot <- dplyr::ungroup(cumPlot)
                cumPlot <- dplyr::arrange(cumPlot, dplyr::desc(type_var))
                cumPlot <- dplyr::mutate(cumPlot, td = rowMeans(cumPlot[4:5]))

                pl <- pl + ggplot2::geom_area(ggplot2::aes(fill = type_var, color = type_var),
                                     alpha = .8,
                                     position = "stack")
                pl <- pl + ggrepel::geom_label_repel(data = cumPlot,
                                                     ggplot2::aes(y = td,
                                                         fill = type_var,
                                                         label = type_var),
                                                     nudge_x = 10,
                                                     label.size = 0.2,
                                                     segment.size = 0.25,
                                                     size = 2,
                                                     color = 'white',
                                                     force = 3,
                                                     segment.color = 'grey60')
        }

        if(plot_type == "line") {
                pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                     alpha = .8, position = "identity")
                plot2 <- dplyr::filter(plot, YEAR== max(YEAR, na.rm = TRUE))
                pl <- pl + ggrepel::geom_label_repel(data = plot2,
                                                     ggplot2::aes(label = type_var,
                                                         fill = type_var),
                                                     nudge_x = 10,
                                                     label.size = 0.2,
                                                     segment.size = 0.25,
                                                     size = 2,
                                                     color = 'white',
                                                     force = 3,
                                                     segment.color = 'grey60')

        }

        if (return_data == TRUE){
                plot
        } else {
                pl
        }
}
