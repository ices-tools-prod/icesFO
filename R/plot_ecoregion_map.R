#' Returns an map of the ecoregion and the divisions included in it
#' 
#' @param ecoregion a simple features dataframe output of load_ecoregions( ... )
#' @param ices_areas a simple features dataframe output of load_areas( ... )
#' 
#' @return A ggplot plot
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
#' }
#'
plot_ecoregion_map <- function(ecoregion, ices_areas){

  # set the csr for the map
  crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  # get europe coastline polygon      
  europe_shape <- 
    rnaturalearth::ne_countries(
      scale = 10, type = "countries", 
      continent = "europe", 
      returnclass = "sf")
  europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]

  # prepare ices areas
  ices_areas <- sf::st_transform(ices_areas, crs = crs)

  # prepare ecoregion
  ecoregion <- sf::st_transform(ecoregion, crs = crs)

  # Centroids for labels
  centroids <- sf::st_centroid(select(ices_areas, -WKT))
  centroids <- cbind(centroids, sf::st_coordinates(centroids))

  #if (ecoregion == "Celtic Seas") {
  #  extracentroids <- dplyr::filter(centroids, Area_27 %in% c("4.a", "2.a.2", "5.b"))
  #  #mutate, change the position of labels so they are close to Celtic Seas
  #  extracentroids[,3] <- c(3710000, 3760000)
  #  extracentroids[,4] <- c(4250000, 4500000)
  #  extraareas <- dplyr::filter(ices_areas,Area_27 %in% c("4.a", "2.a.2"))
  #  extracentroids<<- extracentroids
  #  extraareas<<- extraareas
  #}

  if(ecoregion == "Baltic Sea") {
    baltic_3a <- dplyr::filter(ices_areas, SubArea == "3", Division == "a")
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


  
  box1 <- sf::st_bbox(ecoregion)
  box2 <- sf::st_bbox(ices_areas)

  xlims <- c(min(box1["xmin"], box2["xmin"]), 
             max(box1["xmax"], box2["xmax"]))
  ylims <- c(min(box1["ymin"], box2["ymin"]), 
             max(box1["ymax"], box2["ymax"]))

  p <- 
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::geom_sf(data = ecoregion, color = "grey90", fill = "gold") +
    ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
    ggplot2::geom_sf(data = ices_areas, color = "grey60", fill = "transparent") +
    ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    ggplot2::geom_text(data = centroids, ggplot2::aes(x = X, y = Y, label = Area_27), size = 2.5) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 6),
          plot.subtitle = ggplot2::element_text(size = 7),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()) +
    ggplot2::labs(caption = "Made with Natural Earth and ICES Marine Data")

  p
}
