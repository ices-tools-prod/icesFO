#' STECF Landings over time by country, guild, or species
#'
#' The \code{stecf_plot} function returns an area or line plot of landings (historic and official catch) for an ecoregion by country
#' guild, or species.
#'
#' @param x a dataframe resulting from format_stecf_effort function
#' @param guild an identifier of the Fisheries guild to plot
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
#' stecf_plot("Greater North Sea Ecoregion", metric = "EFFORT", type = "GEAR", return_plot = TRUE, line_count = 4)
#' }
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Landings over time by country, guild, or species #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
plot_stecf_effort <- function(x, cap_year, cap_month, line_count, stecf_report, return_data = T) {
        # filter(ECOREGION == ecoregion) %>%
        #         rename_(.dots = setNames(c(type, "LANDINGS"),
        #                                  c("type_var", "VALUE")))
        
        
        dat <- x %>%rename_(.dots = setNames(c("COUNTRY", "EFFORT"),c("type_var", "VALUE")))
        dat <- x %>%rename_(.dots = setNames(c("GEAR", "LANDINGS"),c("type_var", "VALUE")))
        dat$type_var <- as.factor(dat$type_var)
        Plot <- dat %>%
                group_by(type_var) %>%
                summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% 
                arrange(-typeTotal) %>%
                filter(typeTotal >= 1) %>%
                mutate(RANK = min_rank(desc(typeTotal))) 
        
        
        # Plot <- dat %>%
        #         group_by(type_var) %>%
        #         summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% 
        #         arrange(-typeTotal) %>%
        #         filter(typeTotal >= 1) %>%
        #         mutate(RANK = min_rank(desc(typeTotal)))%>%
        #         inner_join(x, c("type_var")) %>%
        #         ungroup() %>%
        #         mutate(type_var = replace(type_var, RANK > line_count, "other"),
        #                ANNEX = stringr::str_wrap(ANNEX, width = 26)) %>%
        #         group_by(ANNEX, type_var, YEAR) %>%
        # summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
        #         filter(!is.na(YEAR))
        
        
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dat %>% left_join(Plot)
        dat <- dat %>% filter(!is.na(VALUE))
        dat <- dat  %>%
                group_by(type_var, YEAR) %>%
                summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
                filter(!is.na(YEAR))
        
        Label <- "Nominal effort (kW days at sea)"
        Label <- "Landings (thousand tonnes)"
                
        dat <- rbind(dat[!dat$type_var == "other",],
                           dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF %s. Accessed %s/%s.",
                             stecf_report,
                             cap_month,
                             cap_year)
        
        cap_lab <- labs(title = "", x = "", y = Label,
                        caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(line_count + 1)
        
        order <- dat %>%
                group_by(type_var) %>%
                summarize(total = sum(typeTotal, na.rm = TRUE)) %>%
                arrange(-total) %>%
                ungroup() %>%
                mutate(type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                                     levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        
        # dat$ANNEX <- as.factor(dat$ANNEX)
        
        pl <- ggplot(ungroup(dat), aes(x = YEAR, y = typeTotal)) +
                scale_fill_manual(values = myColors) +
                scale_color_manual(values = myColors) +
                scale_x_continuous(breaks = seq(min(dat$YEAR, na.rm = TRUE),
                                                max(dat$YEAR, na.rm = TRUE), by = 2)) +
                geom_segment(aes(x = -Inf, xend = 2016, y = -Inf, yend = -Inf), color = "grey50")+
                geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                expand_limits(x = c(min(dat$YEAR, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                theme_bw(base_size = 9) +
                theme(legend.position = 'none',
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.border = element_blank(),
                      strip.background = element_blank(),
                      plot.caption = element_text(size = 6),
                      axis.line = element_blank())
     
        
                pl <- pl + geom_line(aes(color = type_var),
                                     alpha = .9, position = "identity")
                pl <- pl + ggrepel::geom_label_repel(data = dat %>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                                     aes(label = type_var,
                                                         fill = type_var),
                                                     nudge_x = 3,
                                                     label.size = 0.2,
                                                     segment.size = 0.25,
                                                     size = 2,
                                                     color = 'white',
                                                     force = 3,
                                                     segment.color = 'grey60')
       
        
        # if(return_data) {
        #         write.csv(x = dat, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
        # }
        return(pl)
}
