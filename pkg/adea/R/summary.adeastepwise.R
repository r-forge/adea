#' Summary method for adeastepwise class
#'
#' For the final model of adea_stepwise function print the model name, orientation, load orientation, a summary, the input variables, and outputs variables.
#' 
#' @name summary.adeastepwise
#' @param object Is the object of class adeastepwise to summarise.
#' @param eff.tolerance Tolerance value for efficiencies to conside a DMU an efficient one. Its default value is .001.
#' @method summary adeastepwise
summary.adeastepwise <- function(object, eff.tolerance = .001) {
    l <- list()
    l['Model name'] <- object$name
    l['Orientation'] <- object$orientation
    l['Load orientation'] <- object$load.orientation
    neff <- sapply(object$models, neff.dea, eff.tolerance = eff.tolerance)
    s <- data.frame(
        'Loads' = object$loads,
        'nEfficients' = neff,
        'nVariables' = object$nvariables,
        'nInputs' = object$ninputs,
        'nOutputs' = object$noutputs,
        'Inputs' = object$inputnames,
        'Outputs' = object$outputnames)
    s <- s[s[,1] > 0,]
    s <- s[nrow(s):1,]
    l$models <- s
    class(l) <- 'summary.adeastepwise'
    l
}

#' @export
print.summary.adeastepwise <- function(x, ...) {
    models <- x$models
    lx <- x
    lx$models <- NULL
    lx <- data.frame(unlist(lx))
    rownames(lx) <- gettext(rownames(lx))
    names(lx) <- ''
    print(lx, ...)
    names(models) <- gettext(names(models))
    print(models, ...)
    invisible(x)
}
