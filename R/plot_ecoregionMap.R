#' Returns an map of the ecoregion and the divisions included in it
#' 
#' @param x a dataframe output of load_ecoregions.R
#' @param y a dataframe output of load_areas.R
#' @param ecoregion the ecoregion of interest
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
#' plot1 <- plot_ecoregionMap(ecoregions,areas, ecoregion = "Baltic Sea Ecoregion", 
#' return_data = FALSE)
#' 
#'
plot_ecoregionMap <- function(x, y, ecoregion){
        
        europe_shape <- rnaturalearth::ne_countries(scale = 10,
                                           type = "countries",
                                           continent = "europe",
                                           returnclass = "sf")[, c("iso_a3", "iso_n3", "admin", "geometry")]
        crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        
        eco_areas <- sf::st_transform(x,crs = crs)
        eco_areas <- dplyr::mutate(eco_areas, Ecoregion = dplyr::case_when(
                        grepl("Baltic Sea", Ecoregion) ~ "Baltic Sea Ecoregion",
                        grepl("Greater North Sea", Ecoregion) ~ "Greater North Sea Ecoregion",
                        grepl("Bay of Biscay and the Iberian Coast", Ecoregion) ~ "Bay of Biscay and the Iberian Coast Ecoregion",
                        grepl("Celtic Seas", Ecoregion) ~ "Celtic Seas Ecoregion",
                        grepl("Iceland Sea", Ecoregion) ~ "Icelandic Waters Ecoregion",
                        grepl("Norwegian Sea", Ecoregion) ~ "Norwegian Sea Ecoregion",
                        grepl("Barents Sea", Ecoregion) ~ "Barents Sea Ecoregion",
                        #grepl("Arctic Ocean", .$Ecoregion) ~ "Arctic Ocean Ecoregion",
                        # ... add remaining ecoregions
                        TRUE ~ "OTHER"))
        eco_areas <- dplyr::filter(eco_areas,Ecoregion == ecoregion)
        eco_areas <- sf::st_sf(eco_areas)
                
        Area_27_baltic <- c("3.d.27", "3.d.25", "3.d.24",
                            "3.b.23", "3.c.22", "3.d.31",
                            "3.d.30", "3.d.32", "3.d.29",
                            "3.d.28.1", "3.d.28.2", "3.d.26")
        #have to do like this, because both have the 7.e in the map
        if(ecoregion == "Greater North Sea Ecoregion") {
                Area_27_ns <- c("3.a.20", "3.a.21",
                                "4.a", "4.b", "4.c",
                                "7.d", "7.e")
        }
        if(ecoregion == "Celtic Seas Ecoregion") {
                Area_27_cs <- c("6.a", "6.b.2","7.a", "7.b", "7.c.2", "7.e",
                                "7.f", "7.g", "7.h","7.j.2", "7.k.2")
        }
        
        Area_27_bob <- c("8.a", "8.b","8.c",
                         "8.d.2", "8.e.2", "9.a",
                         "9.b.2")
        Area_27_is <- c("5.a.1", "5.a.2","12.a.4")
        
        Area_27_nw <- c("2.a.1", "2.a.2", "2.b.1", "2.b.2", "14.a")
        
        Area_27_br <- c("1.a", "1.b","2.a.2", "2.b.2")
        
        ices_areas <- sf::st_transform(y, crs = crs) 
        ices_areas <- dplyr::mutate(ices_areas, ECOREGION = dplyr::case_when(
                        Area_27 %in% Area_27_baltic ~ "Baltic Sea Ecoregion",
                        # .$Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion",
                        Area_27 %in%  Area_27_bob ~ "Bay of Biscay and the Iberian Coast Ecoregion",
                        #.$Area_27 %in%  Area_27_cs ~ "Celtic Seas Ecoregion",
                        Area_27 %in%  Area_27_is ~ "Icelandic Waters Ecoregion",
                        
                        #this bit has to be turned on and off, because either areas are 
                        # in norwegian or in barents
                        # .$Area_27 %in%  Area_27_nw ~ "Norwegian Sea Ecoregion",
                        Area_27 %in%  Area_27_br ~ "Barents Sea Ecoregion",
                        # ... add remaining ecoregions
                        TRUE ~ "OTHER"))
        ices_areas <- sf::st_sf(ices_areas)
        
        if(ecoregion == "Greater North Sea Ecoregion") {
                ices_areas <- sf::st_transform(y,crs = crs)
                ices_areas <- dplyr::mutate(ices_areas,ECOREGION = dplyr::case_when(
                                Area_27 %in%  Area_27_ns ~ "Greater North Sea Ecoregion"))
                ices_areas <- sf::st_sf(ices_areas)
        }
        
        if(ecoregion == "Celtic Seas Ecoregion") {
                ices_areas <- sf::st_transform(y,crs = crs)
                ices_areas <- dplyr::mutate(ices_areas, ECOREGION = dplyr::case_when(
                                Area_27 %in% Area_27_cs ~ "Celtic Seas Ecoregion"))
                ices_areas <- sf::st_sf(ices_areas)
        }
        
        # Centroids for labels
        ices_area_centroids <- sf::st_centroid(ices_areas)
        centroids <- data.frame(as.character(ices_area_centroids$Area_27),
                                ices_area_centroids$ECOREGION,
                                matrix(unlist(ices_area_centroids$geometry),
                                       ncol = 2,
                                       byrow = TRUE),
                                stringsAsFactors = FALSE)
        
        colnames(centroids) <- c("Area_27", "ECOREGION", "X", "Y")
        
        if(ecoregion == "Celtic Seas Ecoregion") {
                extracentroids <-dplyr::filter(centroids, Area_27 %in% c("4.a", "2.a.2", "5.b"))
                #mutate, change the position of labels so they are close to Celtic Seas
                extracentroids[,3] <- c(3710000, 3760000)
                extracentroids[,4] <- c(4250000, 4500000)
                extraareas <- dplyr::filter(ices_areas,Area_27 %in% c("4.a", "2.a.2"))
                extracentroids<<- extracentroids
                extraareas<<- extraareas
        }
        
        if(ecoregion == "Baltic Sea Ecoregion") {
                baltic_3a <- dplyr::filter(ices_areas, SubArea == "3",
                               Division == "a")
                baltic_3a <- dplyr::summarize(baltic_3a, Area_27 = "3.a",
                                  ECOREGION = "Baltic Sea Ecoregion",
                                  geometry = sf::st_union(geometry))
                baltic_3a <- sf::st_sf(baltic_3a)
                baltic_3a <- sf::st_centroid(baltic_3a)
                
                baltic_3a <- data.frame(baltic_3a$Area_27,
                                        baltic_3a$ECOREGION,
                                        matrix(unlist(baltic_3a$geometry),
                                               ncol = 2,
                                               byrow = TRUE),
                                        stringsAsFactors = FALSE)
                
                colnames(baltic_3a) <- c("Area_27", "ECOREGION", "X", "Y")
                
                centroids <- dplyr::bind_rows(centroids, baltic_3a)
        }
        
        
        centroids <- dplyr::mutate(centroids,Area_27 = dplyr::case_when(
                        ECOREGION == "Baltic Sea Ecoregion" ~ sub("3.b.|3.c.|3.d.", "", Area_27),
                        ECOREGION == "Greater North Sea Ecoregion" ~ as.character(Area_27),
                        ECOREGION == "Bay of Biscay and the Iberian Coast Ecoregion" ~ as.character(Area_27),
                        ECOREGION == "Celtic Seas Ecoregion" ~ as.character(Area_27),
                        ECOREGION == "Icelandic Waters Ecoregion" ~ as.character(Area_27),
                        ECOREGION == "Norwegian Sea Ecoregion" ~ as.character(Area_27),
                        ECOREGION == "Barents Sea Ecoregion" ~ as.character(Area_27),
                        TRUE ~ "OTHER"
                ))
        
        ices_areas <- dplyr::filter(ices_areas, grepl(ecoregion, ECOREGION))
        ices_areas <- sf::st_sf(ices_areas)
        
        centroids <- dplyr::filter(centroids, grepl(ecoregion, ECOREGION))
        
        
        cap_lab <- ggplot2::labs(caption = "Made with Natural Earth and ICES Marine Data")
        xmin <- min(sf::st_bbox(eco_areas)[1], sf::st_bbox(ices_areas)[1])
        xmax <- max(sf::st_bbox(eco_areas)[3], sf::st_bbox(ices_areas)[3])
        ymin <- min(sf::st_bbox(eco_areas)[2], sf::st_bbox(ices_areas)[2])
        ymax <- max(sf::st_bbox(eco_areas)[4], sf::st_bbox(ices_areas)[4])
        
        xlims <- c(xmin, xmax)
        ylims <- c(ymin, ymax)
        
        p1 <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = eco_areas, color = "grey90", fill = "gold") +
                # geom_sf(data = visahke, color = "grey80", fill = "gold") +
                ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
                # geom_sf(data = ices_areas, color = "grey60", fill = "gold") +
                # geom_sf(data = extraareas, color = "grey80", fill = "transparent")+
                ggplot2::geom_sf(data = ices_areas, color = "grey60", fill = "transparent") +
                ggplot2::geom_text(data = centroids, ggplot2::aes(x = X, y = Y, label = Area_27), size = 2.5) +
                # geom_text(data = extracentroids, aes(x = X, y = Y, label = Area_27), size = 2.5) +
                #geom_text(data = visahke, aes(x = X, y = Y, label = Area_27), size = 2.5) +
                ggplot2::theme_bw(base_size = 8) +
                ggplot2::theme(plot.caption = ggplot2::element_text(size = 6),
                      plot.subtitle = ggplot2::element_text(size = 7),
                      axis.title.x = ggplot2::element_blank(),
                      axis.title.y = ggplot2::element_blank()) +
                ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
                cap_lab
        
                p1
}
