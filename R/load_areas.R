#' Download ICES areas polygons
#'
#' Returns a simple features object with polygons for all
#' subdivisions
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
#'   areas <- load_areas()
#' }
#'
#' @export

load_areas <- function() {
        
        # get tempdir
        tmpdir <- tempdir()
        
        filename <- "ICES_areas.zip"
        # download and unzip
        download.file(paste0("http://gis.ices.dk/shapefiles/", filename),
                      destfile = file.path(tmpdir, filename),
                      quiet = TRUE)
        unzip(file.path(tmpdir, filename),
              exdir = file.path(tmpdir, "ICES_areas"))
        # delete zip file
        unlink(file.path(tmpdir, filename))
        
        areas <- sf::read_sf(file.path(tmpdir, "ICES_areas"))
        
        areas
}
