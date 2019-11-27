#' Returns an map of the ecoregion and the divisions included in it
#'
#' @param sar a dataframe of swept area ratio data with a WKT
#'        columns (see notes)
#' @param ecoregion a dataframe output of load_ecoregion( ... )
#' @param what a flag which SAR value to plot - either "surface" or "subsurface"
#' @return A ggplot object
#'
#' @note
#'
#' The effort data.frame is conveted to an sf object for plotting and
#' is expeted to have a Well Known Text column containing polygons
#' of c-squares
#'
#' @seealso
#'
#' \code{\link{plot_ecoregion_map}} plots ICES areas and ecoregion.
#'
#' @examples
#' \dontrun{
#' ecoregion <- load_ecoregion("Baltic Sea")
#' sar <- icesVMS::get_sar_map("Baltic Sea")
#'
#' # convert to sf
#' sar <- sf::st_as_sf(sar, wkt = "wkt", crs = 4326)
#'
#' plot1 <- plot_sar_map(sar, ecoregion, what = "surface")
#' }
#'
#' @export
plot_sar_map <- function(sar, ecoregion, what) {

  # plot surfac or suburface?
  what <- match.arg(what, c("surface", "subsurface"))
  what <- paste0(what, "_sar")

  if (what == "surface_sar") {
    legend_name = "Surface Swept Area Ratio"
  } else {
    legend_name = "Subsurface Swept Area Ratio"
  }

  # define projection to use
  crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  # get europe coastline polygon
  europe_shape <-
    rnaturalearth::ne_countries(
      scale = 10, type = "countries",
      continent = "europe",
      returnclass = "sf")
  europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]

  # prepare ecoregion
  ecoregion <- sf::st_transform(ecoregion, crs = crs)

  # prepare effort
  sar <- sf::st_transform(sar, crs = crs)
  sar$val <- as.numeric(sar[[what]])
  sar <- dplyr::filter(sar, val > 0)

  # define plot limits
  box <- sf::st_bbox(ecoregion)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])

  # do plot
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = ecoregion, color = "grey90", fill = "transparent") +
    ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
    ggplot2::geom_sf(data = sar,
                     ggplot2::aes(fill = get_map_breaks(val)),
                     col = "transparent") +
    ggplot2::scale_fill_viridis_d(name = legend_name,
                                  direction = -1,
                                  option = "A",
                                  guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 6),
                   plot.subtitle = ggplot2::element_text(size = 7),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    ggplot2::labs(caption = "Made with Natural Earth and ICES Marine Data") +
    ggplot2::theme_bw(base_size = 8)

  p
}
