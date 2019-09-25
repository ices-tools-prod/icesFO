#' Download a statistical rectangles mapped to ecoregion
#'
#' Returns a data frame of giving the ecoregion for each ICES
#' statistical rectangle
#'
#' @return A data frame
#'
#'
#' @seealso
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#'   statrec2ecoregion <- load_statrec2ecoregion()
#' }
#'
#' @export

load_statrec2ecoregions <- function() {

  # get tempdir
  tmpdir <- tempdir()

  filename <- "ICES_StatRec_mapto_Ecoregions.zip"
  # download and unzip
  download.file(paste0("http://gis.ices.dk/shapefiles/", filename),
                destfile = file.path(tmpdir, filename),
                quiet = TRUE)
  unzip(file.path(tmpdir, filename),
        exdir = file.path(tmpdir, "ICES_StatRec_mapto_Ecoregions"))
  # delete zip file
  unlink(file.path(tmpdir, filename))

  eco <- sf::read_sf(file.path(tmpdir, 'ICES_StatRec_mapto_Ecoregions'))
  eco <- data.frame(eco)
  eco <- eco[c("ICESNAME", "Ecoregion")]

  eco
}
