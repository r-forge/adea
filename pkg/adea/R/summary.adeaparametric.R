#' summary method for adeaparametric class
#'
#' For the final model of adea_parametric function print the model name, orientation, load orientation, a summary, the input variables, and outputs variables.
#' 
#' @name summary.adeaparametric
#' @param object is the object of class adeaparametric to summarise
#' @param ... optional arguments to 'print'
#' @method summary adeaparametric
#' @export
summary.adeaparametric <- function(object, ...) {
    cat(paste0(gettext('Summary of adea parametric variable selection'), ifelse(object$name=='', '', paste0(' ', gettext('for model'), ' "')), object$name, '":\n'))
    cat(gettext('Orientation is'), ' ', object$orientation, '.\n', sep = '')
    cat(gettext('Load orientation is'), ' ', object$load.orientation, '.\n', sep = '')
    s <- data.frame(object$load, object$neff, object$nt, object$ni, object$no, object$namesi, object$nameso)
    colnames(s) <- gettext(c('Load',  '#Efficients', '#Variables', '#Inputs', '#Outputs', 'Inputs', 'Outputs'))
    s <- s[s[,1] > 0,]
    s <- s[nrow(s):1, ]
    print(s, ...)
    invisible(s)
}
