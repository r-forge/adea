#' Print method for adeaparametric class
#'
#' For the final model of adea_parametric function prints a summary, the input variables, and outputs variables.
#' 
#' @name print.adeaparametric
#' @param x Object of class adeaparametric to print.
#' @param ... Optional arguments to "print".
#' @method print adeaparametric
#' @export
print.adeaparametric <- function(x, ...) {
    s <- data.frame(x$load, x$neff, x$nt, x$ni, x$no, x$namesi, x$nameso)
    colnames(s) <- gettext(c('Load',  '#Efficients', '#Variables', '#Inputs', '#Outputs', 'Inputs', 'Outputs'))
    s <- s[nrow(s):1,]
    s <- s[s[, 1] > 0,]
    print(s, ...)
    invisible(x)
}
