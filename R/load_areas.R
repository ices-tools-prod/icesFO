#' Download ICES areas polygons
#'
#' Returns a simple features object with polygons for all
#' subdivisions
#'
#' @param ecoregion an ICES ecoregion to download ICES areas from (e.g "Baltic Sea")
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
#'   ices_areas <- load_areas("Greater North Sea")
#' }
#'
#' @export

load_areas <- function(ecoregion, precision = 3) {

  # get areas
  areas <- get_area_27(ecoregion)

  # base url
  baseurl <- "http://gis.ices.dk/gis/rest/services/ICES_reference_layers/ICES_Areas/MapServer/0/query?where=Area_27+in+%28%273.d.27%27%2C%273.d.27%27%29&returnGeometry=true&geometryPrecision=2&f=geojson"
  url <- httr::parse_url(baseurl)
        
  # add query
  url$query$where <- paste0("Area_27 in ('", paste(areas, collapse = "','"), "')")
  url$query$geometryPrecision <- precision
  url$query$outFields <- "Area_27"

  url <- httr::build_url(url)

  # file name
  filename <- tempfile(fileext = ".geojson")

  # download
  download.file(url,
                destfile = filename,
                quiet = FALSE)
  areas <- sf::read_sf(filename)

  # delete file
  unlink(filename)
  
  areas
}
