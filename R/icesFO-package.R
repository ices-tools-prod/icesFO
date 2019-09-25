#' @docType package
#'
#' @name icesFO-package
#'
#' @aliases icesFO
#'
#' @title Functions to support the creation of ICES Fisheries Overviews
#'
#' @description
#' Functions to support the creation of ICES Fisheries Overviews, ....
#'
#' @details
#' \emph{Loading data:}
#' \tabular{ll}{
#'   \code{\link{load_sid}}                   \tab load data from SID\cr
#'   \code{\link{load_sag}}            \tab load data from SAG\cr
#'   \code{\link{load_catches}}      \tab load catch data from ICES\cr
#' }
#' \emph{Formatting datas:}
#' \tabular{ll}{
#'   \code{\link{format_sid}}      \tab format raw output from load_sid\cr
#'   \code{\link{format_stecf}}      \tab format raw output from load_sid\cr
#' }
#' \emph{Plots:}
#' \tabular{ll}{
#'   \code{\link{plot_CLD_bar}}      \tab plot\cr
#'   \code{\link{plot_GES_pies}}      \tab plot\cr
#'   \code{\link{plot_catch_trends}}      \tab plot\cr
#'   \code{\link{plot_discard_current}}      \tab plot\cr
#'   \code{\link{plot_discard_trends}}      \tab plot\cr
#'   \code{\link{plot_kobe}}      \tab plot\cr
#'   \code{\link{plot_status_prop_pies}}      \tab plot\cr
#'   \code{\link{plot_stecf}}      \tab plot\cr
#'   \code{\link{plot_stock_trends}}      \tab plot\cr
#' }
#'
#' @author Adriana Villamor, Scott Large, Sarah Louise Millar and Colin Millar.
#'
#' @importFrom stats complete.cases filter lag reorder setNames
#' @importFrom utils download.file read.csv read.delim unzip
#'

NA