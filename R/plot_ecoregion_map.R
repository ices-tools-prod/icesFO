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
#'   ices_areas <- load_areas("Baltic Sea")
#'   ecoregion <- load_ecoregion("Baltic Sea")
#'   eco_map <- plot_ecoregion_map(ecoregion, ices_areas)
#' }
#'
#' @export

plot_ecoregion_map <- function(ecoregion, ices_areas) {

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
  centroids <- sf::st_centroid(ices_areas)
  centroids <- cbind(centroids, sf::st_coordinates(centroids))
  # centroids$X[which(centroids$Area_27 == "2.a.2")] <- "3600000.00000000"
  # centroids$Y[which(centroids$Area_27 == "2.a.2")] <- "4600000.00000000"
  centroids$X[which(centroids$Area_27 == "10.a.1")] <- "460000.00000000"
  centroids$Y[which(centroids$Area_27 == "12.a.2")] <- "4400000.00000000"
  centroids$X[which(centroids$Area_27 == "5.b.1.a")] <- "3107623.00000000"
  # centroids$X[which(centroids$Area_27 == "6.a")] <- "3250000.00000000"
  # centroids$Y[which(centroids$Area_27 == "6.a")] <- "4230000.00000000"
  # centroids$X[which(centroids$Area_27 == "4.a")] <- "3650000.00000000"
  # centroids$Y[which(centroids$Area_27 == "4.a")] <- "4300000.00000000"
  # centroids$X[which(centroids$Area_27 == "5.b.1.a")] <- "3055000.00000000"
  centroids$X <- as.numeric(centroids$X)
  centroids$Y <- as.numeric(centroids$Y)
  

  #if (ecoregion$Ecoregion == "Celtic Seas") {
  #  extracentroids <- dplyr::filter(centroids, Area_27 %in% c("4.a", "2.a.2", "5.b"))
  #  #mutate, change the position of labels so they are close to Celtic Seas
  #  extracentroids[,3] <- c(3710000, 3760000)
  #  extracentroids[,4] <- c(4250000, 4500000)
  #  extraareas <- dplyr::filter(ices_areas,Area_27 %in% c("4.a", "2.a.2"))
  #  extracentroids <- extracentroids
  #  extraareas<<- extraareas
  #}

  if (ecoregion$Ecoregion == "Baltic Sea") {
    ices_areas <- dplyr::filter(ices_areas, substr(Area_27, 1, 3) != "3.a")
  }

  # ices_areas <- dplyr::filter(ices_areas, substr(Area_27, 1, 3) != "5.b.1.a")
  
  # get plot extent
  box1 <- sf::st_bbox(ecoregion)
  box2 <- sf::st_bbox(ices_areas)

  xlims <- c(min(box1["xmin"], box2["xmin"]), 
             max(box1["xmax"], box2["xmax"]))
  ylims <- c(min(box1["ymin"], box2["ymin"]), 
             max(box1["ymax"], box2["ymax"]))

  # xlims <- c(min(box1["xmin"], box1["xmin"]),
  #            max(box1["xmax"], box1["xmax"]))
  # ylims <- c(min(box1["ymin"], box1["ymin"]),
  #            max(box1["ymax"], box1["ymax"]))

  
  # make plot
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
