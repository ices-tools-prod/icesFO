#' STECF Landings over time by country, guild, or species
#'
#' The \code{stecf_plot} function returns an area or line plot of landings (historic and official catch) for an ecoregion by country
#' guild, or species.
#'
#' @param x a dataframe resulting from format_stecf_effort function
#' @param type need description
#' @param variable need description
#' @param stecf_report need description
#' @param cap_month  the month to be shown in the figure caption, the accession date to SAG usually
#' @param cap_year the year to be shown in the figure caption
#' @param line_count indicates the number of lines to be shown in the graph
#' @param return_data a parameter indicating if the data behind the plot should be returned as a dataframe
#' 
#' 
#' @return A ggplot2 object 
#' 
#' @examples
#' \dontrun{
#' stecf_plot("Greater North Sea Ecoregion", 
#'            metric = "EFFORT", 
#'            type = "GEAR", 
#'            return_plot = TRUE, 
#'            line_count = 4)
#' }
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Landings over time by country, guild, or species #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                dat <- dplyr::rename_(x, "type_var" ="COUNTRY",
                              "VALUE" = "EFFORT")}
                if(variable=="GEAR"){
                        dat <- dplyr::rename_(x, "type_var" ="GEAR",
                                              "VALUE" = "EFFORT")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
                }
        if(type == "landings"){
                dat <- dplyr::rename_(x, "type_var" ="gear_class",
                                      "VALUE" = "LANDINGS")
                Label <- "Landings (thousand tonnes)"
                }
        
        dat$type_var <- as.factor(dat$type_var)
        
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        dat <- dat[complete.cases(dat), ]
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > line_count, "other"))
        dat <- dplyr::group_by(dat,type_var, YEAR) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(YEAR))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                           dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF %s. Accessed %s/%s.",
                             stecf_report,
                             cap_month,
                             cap_year)
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                        caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(line_count + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                                     levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
                }

        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = YEAR, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$YEAR, na.rm = TRUE),
                                                max(dat$YEAR, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2016, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$YEAR, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 6),
                      axis.line = ggplot2::element_blank())
     
        
                pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                     alpha = .9, position = "identity")
                dat2 <- dplyr::filter(dat,YEAR == max(YEAR, na.rm = TRUE))
                pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                                     ggplot2::aes(label = type_var,
                                                         fill = type_var),
                                                     nudge_x = 3,
                                                     label.size = 0.2,
                                                     segment.size = 0.25,
                                                     size = 2,
                                                     color = 'white',
                                                     force = 3,
                                                     segment.color = 'grey60')
       
        
                if(return_data == T){
                        dat
                }else{
                        pl
                }
}
