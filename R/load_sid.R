#' Download data from the ICES Stock Information Database.
#'
#' Download data from the ICES Stock Information Database for the year in
#' which you are producing the Fisheries Overviews.
#'
#' @param year the year for which data is required.
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{load_sag}} for loading data from the ICES Stock Assessment database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' sid_raw <- load_sid(2019)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

load_sid <- function(year){
  # create url for SID web service
  url <- paste0("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20", year)
  # download json data
  out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value

  unique(out)
}
