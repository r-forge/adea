#' summary method for cadea class
#' @aliases summary.cadea
#' @importFrom stats sd
#' @param object is the object of class cadea to summarise
#' @param ... optional arguments to 'print'
#' @method summary cadea
#' @export
summary.cadea <- function(object, ...) summary.adea(object, ...)
