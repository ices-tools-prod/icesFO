#' Visualise a matrix of technical interactions.
#'
#' Based on a matrix of technical interactions produce an image plot
#' with a legend.
#'
#' @param x a square matrix with identical row and column names containing
#'        values of tecnical interaction between species
#' @param col a vector of colours to use on the image plot
#'
#' @return A list with four elements, technical interactions for landings (recapLand)
#'         and catch (recapCatch) and tables of main gears involved in the interactions
#'         based on landings (MainGearsLandings) and catches (MainGearsCatches)
#'
#' @seealso
#'
#' \code{\link{compute_technical_interactions}} for computing technical interactions.
#' 
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' technical_interacton <- 
#'   compute_technical_interactions(STECF_landings, catchCoverage = 0.99)
#' 
#' plot_technical_interactions(technical_interacton$recapLand)
#' }
#'
#' @importFrom grDevices heat.colors
#' @importFrom graphics image mtext par
#' 
#' @export

plot_technical_interactions <- function(x, col = heat.colors(5, rev = TRUE)) {
   
   rnames <- row.names(x)
   at <- seq(0, nrow(x)-1) / (nrow(x)-1)

   op <- par(mar = c(6.1, 5.1, 5.1, 6.1), no.readonly = TRUE)
   on.exit(par(op))

   image(t(x)[,nrow(x):1], col = col, axes = FALSE)
   mtext(text = rev(rnames), side = 2, line = 0.3,  las = 1, cex = 0.8, at = at)
   mtext(text = rnames, side = 3, line = 0.3, las = 2, cex = 0.8, at = at)
   fields::image.plot(as.matrix(x), legend.only = TRUE, col = col)  
}
