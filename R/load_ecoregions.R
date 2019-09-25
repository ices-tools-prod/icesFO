#' Download ecoregion polygons
#'
#' Returns a simple features object with a polygon for each
#' ecoregion
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
#'   ecoregions <- load_ecoregions()
#' }
#'
#' @export

load_ecoregions <- function() {

  # get tempdir
  tmpdir <- tempdir()

  filename <- "ICES_ecoregions.zip"
  # download and unzip
  download.file(paste0("http://gis.ices.dk/shapefiles/", filename),
                destfile = file.path(tmpdir, filename),
                quiet = TRUE)
  unzip(file.path(tmpdir, filename),
        exdir = file.path(tmpdir, "ICES_ecoregions"))
  # delete zip file
  unlink(file.path(tmpdir, filename))

  ecoreg <- sf::read_sf(file.path(tmpdir, "ICES_ecoregions"))

  ecoreg
}
