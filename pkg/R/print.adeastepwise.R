#' Print method for adeastepwise class
#'
#' For the final model of adea_stepwise function prints a summary, the input variables, and outputs variables.
#' 
#' @name print.adeastepwise
#' @param x Object of class adeastepwise to print.
#' @param ... Optional arguments to "print".
#' @method print adeastepwise
#' @export
print.adeastepwise <- function(x, ...) {
    .n <- length(x$load)
    cat(sprintf('%9.8s%9.8s%9.8s%9.8s%9.8s%9.8s%9.8s\n', gettext(c('Stop', 'Steps', 'Load', '#Efficients', '#Variables', '#Inputs', '#Outputs'))))
    cat(sprintf('%9s%9d%9.3f%9d%9d%9d%9d\n', gettext(x$stop.criterion), x$steps, x$load[.n], x$neff[.n], x$nt[.n], x$ni[.n], x$no[.n]))
    cat(gettext('Inputs'), ':', paste(x$namesi[.n]), '\n')
    cat(gettext('Outputs'), ':',  paste(x$nameso[.n]), '\n')
    cat(gettext('Efficiencies'), ':', x$adea$eff, '\n')
    invisible(x)
}
