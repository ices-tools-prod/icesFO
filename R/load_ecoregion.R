#' Download ecoregion polygons
#'
#' Returns a simple features object with a polygon for each
#' ecoregion
#'
#' @param ecoregion an ICES ecoregion to download (e.g "Baltic Sea")
#' @param precision the numnber of decimal places required in the coordinates
#' 
#' @return A simple features collection
#'
#'
#' @seealso
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#'   ecoregion <- load_ecoregion("Baltic Sea")
#' }
#'
#' @export

load_ecoregion <- function(ecoregion, precision = 3) {

  # base url, remove after Ecoregion Baltic and check it works
  # baseurl <- "http://gis.ices.dk/gis/rest/services/ICES_reference_layers/ICES_Ecoregions/MapServer/0/query?where=Ecoregion%3D%27Baltic%20Sea%27&geometryType=esriGeometryPolygon&geometryPrecision=2&f=geojson"
  baseurl <- "http://gis.ices.dk/gis/rest/services/ICES_reference_layers/ICES_Ecoregions/MapServer/0/query?geometryType=esriGeometryPolygon&geometryPrecision=2&f=geojson"
  url <- httr::parse_url(baseurl)

  # add query
  url$query$where <- paste0("Ecoregion='", ecoregion, "'")
  url$query$geometryPrecision <- precision

  url <- httr::build_url(url)

  # file name
  filename <- tempfile(fileext = ".geojson")

  # download
  download.file(url,
                destfile = filename,
                quiet = FALSE)
  ecoreg <- sf::read_sf(filename)

  # delete zip file
  unlink(filename)

  ecoreg
}
