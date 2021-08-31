#' Print method for adeahierarchical class
#'
#' For the final model of adea_hierarchical function prints a summary, the input variables, and outputs variables.
#' 
#' @name print.adeahierarchical
#' @param x Object of class adeahierarchical to print.
#' @param ... Optional arguments to "print".
#' @method print adeahierarchical
#' @export
print.adeahierarchical <- function(x, ...) {
    s <- data.frame(x$load, x$neff, x$nt, x$ni, x$no, x$namesi, x$nameso)
    colnames(s) <- gettext(c('Load',  '#Efficients', '#Variables', '#Inputs', '#Outputs', 'Inputs', 'Outputs'))
    s <- s[nrow(s):1,]
    s <- s[s[,1] > 0,]
    print(s, ...)
    invisible(x)
}
