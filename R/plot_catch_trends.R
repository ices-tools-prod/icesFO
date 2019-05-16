#' Returns an ordered plot of catch bars colored according to 
#' F/F<sub>MSY</sub> and SSB/MSY B<sub>trigger</sub>
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

plot_catch_trends <- function(x,type = c("COMMON_NAME", "COUNTRY", "GUILD")[1],
                              line_count = 10,
                              plot_type = c("line", "area")[1],
                              cap_month = "November",
                              cap_year = "2018",
                              return_data = FALSE) {
        cap_lab <-labs(x = "",
                       y = "Landings (thousand tonnes)",
                       caption = sprintf("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-2016, \n ICES, Copenhagen.",
                                         cap_year,
                                         cap_month))
        
        
        df <- x %>% rename(type_var = setNames(type , "type_var"))
        
        if(type == "COMMON_NAME"){
        df$type_var[which(df$type_var == "Angler(=Monk)")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Monkfishes nei")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Anglerfishes nei")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Megrims nei")] <- "Megrim"
        df$type_var[which(df$type_var == "Norway lobster")] <- "Nephrops"
        }
        
        plot <- df %>%
                group_by(type_var) %>%
                dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE)) %>% # Overall catch to order plot
                arrange(-typeTotal) %>%
                filter(typeTotal >= 1) %>%
                dplyr::mutate(RANK = min_rank(desc(typeTotal))) %>%
                inner_join(df, by = "type_var")
        
        plot$RANK<-as.numeric(plot$RANK)
        
        plot <- plot%>%
                dplyr::mutate(type_var = replace(type_var, RANK > line_count, "other")) %>%
                group_by(type_var, YEAR) %>%
                dplyr::summarize(typeTotal = sum(VALUE, na.rm = TRUE) / 1000) %>%
                filter(!is.na(YEAR))
        
        if(type == "COMMON_NAME") {
                # Clean up some of the FAO names... to appease ADGFO
                plot <- plot %>%
                        ungroup() %>%
                        mutate(type_var = gsub("Atlantic ", "", type_var),
                               type_var = gsub("European ", "", type_var),
                               type_var = gsub("Sandeels.*", "sandeel", type_var),
                               type_var = gsub("Finfishes nei", "undefined finfish", type_var),
                               type_var = gsub("Blue whiting.*", "blue whiting", type_var),
                               type_var = gsub("Saithe.*", "saithe", type_var),
                               type_var = ifelse(grepl("Norway", type_var),
                                                 type_var,
                                                 tolower(type_var))
                        )
        }
        
        plot <- rbind(plot[!plot$type_var == "other",],
                           plot[plot$type_var == "other",])
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(line_count + 1)
        
        order <- plot %>%
                group_by(type_var) %>%
                summarize(total = sum(typeTotal, na.rm = TRUE)) %>%
                arrange(-total) %>%
                ungroup() %>%
                mutate(type_var = factor(type_var, type_var))
        
        plot$type_var <- factor(plot$type_var,
                                     levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(plot$type_var))]
        names(myColors) <- levels(plot$type_var)
        myColors["other"] <- "#7F7F7F"
        
        pl <- ggplot(ungroup(plot), aes(x = YEAR, y = typeTotal)) +
                scale_fill_manual(values = myColors) +
                scale_color_manual(values = myColors) +
                scale_x_continuous(breaks = seq(min(plot$YEAR),
                                                max(plot$YEAR), by = 10)) +
                geom_segment(aes(x = -Inf, xend = max(plot$YEAR), y = -Inf, yend = -Inf), color = "grey50")+
                geom_segment(aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                expand_limits(x = c(min(plot$YEAR), max(plot$YEAR) + 20)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                theme_bw(base_size = 9) +
                theme(legend.position = 'none',
                      plot.caption = element_text(size = 6),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_blank())
        
        if(plot_type == "area") {
                cumPlot <- plot %>%
                        filter(YEAR == max(YEAR, na.rm = TRUE)) %>%
                        ungroup() %>%
                        arrange(desc(type_var)) %>%
                        mutate(cs = cumsum(as.numeric(typeTotal)), # cumulative sum
                               mp = lag(cs, order_by = desc(type_var)), # midpoint
                               mp = ifelse(is.na(mp), 1, mp)) %>% # midpoint
                        ungroup() %>%
                        arrange(desc(type_var)) %>%
                        mutate(td = rowMeans(.[,c("cs", "mp")]))#
                
                pl <- pl + geom_area(aes(fill = type_var, color = type_var),
                                     alpha = .8,
                                     position = "stack")
                pl <- pl + ggrepel::geom_label_repel(data = cumPlot,
                                                     aes(y = td,
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
                pl <- pl + geom_line(aes(color = type_var),
                                     alpha = .8, position = "identity")
                pl <- pl + ggrepel::geom_label_repel(data = plot%>% filter(YEAR == max(YEAR, na.rm = TRUE)),
                                                     aes(label = type_var,
                                                         fill = type_var),
                                                     nudge_x = 10,
                                                     label.size = 0.2,
                                                     segment.size = 0.25,
                                                     size = 2,
                                                     color = 'white',
                                                     force = 3,
                                                     segment.color = 'grey60')
                
        }
        pl
        
        # if(return_data) {
        #         write.csv(x = plot, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
        # }
        
        return(pl)
}
