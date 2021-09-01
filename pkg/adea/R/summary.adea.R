#' summary method for adea class
#' @aliases summary.adea
#' @importFrom stats sd
#' @param object is the object of class adea to summarise
#' @param ... optional arguments to 'print'
#' @method summary adea
#' @export
summary.adea <- function(object, ...) {
    eps <- 1e-6
    cat(paste0(gettext('Model name'), ': ', object$name, '\n'))
    cat(paste0(gettext('Orientation is'), ' ', object$orientation, '\n'))
    cat('Inputs:', names(object$load$ratios$input), '\n')
    cat('Outputs:', names(object$load$ratios$output), '\n')
    cat(paste0(gettext('Input loads'), ': '), object$load$ratios$input, '\n')
    cat(paste0(gettext('Output loads'), ': '), object$load$ratios$output, '\n')
    cat(paste0(gettext('Model load'), ': ', object$load$load, '\n'))
    ## print(x$eff, ...)
    cat(paste0(gettext('#Efficients'), ': ', object$neff, '\n'))
    cat(paste0(gettext('Efficiencies'), ':\n'))
    print(object$eff)
    cat(paste0(gettext('Summary of efficiencies'), ':\n'))
    neff <- sum(abs(object$eff - 1) < eps)
    n <- nrow(object$ux)
    s <- summary(object$eff)
    s <- c(s[4], sd(object$eff), s[1:3], s[5:6])
    names(s)[2] <- 'sd'
    print(s, ...)
    invisible(object)
}
