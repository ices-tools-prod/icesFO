#' Returns an map of the ecoregion and the divisions included in it
#' 
#' @param effort a dataframe of effort data with a WKT columns (see notes)
#' @param ecoregion_shape a dataframe output of load_ecoregions()
#' @param ecoregion the ecoregion of interest (e.g. "Baltic Sea")
#' @return A ggplot object
#'
#' @notes
#' 
#' The effort data.frame is conveted to an sf object for plotting and
#' is expeted to have a Well Known Text column containing polygons
#' of c-squares
#' 
#' @seealso
#'
#' \code{\link{plot_ecoregionMap}} plots ICES areas and ecoregion.
#'
#' @examples
#' \dontrun{
#' ecoregions <- load_ecoregions()
#' effort <- icesVMS::get_effort_map("Baltic Sea")
#' plot1 <- 
#'   plot_effort_map(ecoregions, ecoregion = "Baltic Sea", 
#'                   return_data = FALSE)
#' }
#'
plot_effort_map <- function(effort, ecoregion_shape, ecoregion){
  
  # check ecoregion
  ecoregion <- match.arg(ecoregion, c("Baltic Sea", "Celtic Seas", "Greater North Sea"))

  # define projection to use
  crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        
  # get Europe shape file
  europe_shape <- 
    rnaturalearth::ne_countries(scale = 10,
                                type = "countries",
                                continent = "europe",
                                returnclass = "sf")[, c("iso_a3", "iso_n3", "admin", "geometry")]

  # get ecoregion        
  ecoregion_shape <- sf::st_transform(ecoregion_shape, crs = crs)
  ecoregion_shape <- dplyr::filter(ecoregion_shape, Ecoregion == ecoregion)
  ecoregion_shape <- sf::st_sf(ecoregion_shape)

  # prepare effort
  effort$wkt <- sf::st_as_sfc(effort$wkt)
  effort <- sf::st_sf(effort, sf_column_name = "wkt", crs = 4326)
  effort <- sf::st_transform(effort, crs = crs)

  # define plot limits
  box <- sf::st_bbox(ecoregion_shape)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])

  # do plot
  p1 <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = effort, aes(fill = mw_fishinghours), col = "transparent") +
    ggplot2::scale_fill_viridis_c(trans = "sqrt") +
    ggplot2::geom_sf(data = ecoregion_shape, color = "grey90", fill = "transparent") +
    ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 6),
                   plot.subtitle = ggplot2::element_text(size = 7),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    ggplot2::labs(caption = "Made with Natural Earth and ICES Marine Data") +
    ggplot2::facet_wrap( ~ fishing_category_FO) +
    ggplot2::theme_bw(base_size = 8)        

  p1
}
