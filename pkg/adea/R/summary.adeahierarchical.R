#' Summary method for adeahierarchical class
#'
#' For the final model of adea_hierarchical function print the model name, orientation, load orientation, a summary, the input variables, and outputs variables.
#' 
#' @name summary.adeahierarchical
#' @param object Is the object of class adeahierarchical to summarise.
#' @param ... Optional arguments to "print".
#' @method summary adeahierarchical
#' @export
summary.adeahierarchical <- function(object, ...) {
    cat(paste0('\n', gettext('Summary of adea hierarchical variable selection'), ifelse(object$name=='', '', paste0(' ', gettext('for model'), ' "')), object$name, '":\n'))
    cat(gettext('Orientation is'), ' ', object$orientation, '.\n', sep = '')
    cat(gettext('Load orientation is'), ' ', object$load.orientation, '.\n', sep = '')
    s <- data.frame(object$load, object$neff, object$nt, object$ni, object$no, object$namesi, object$nameso)
    colnames(s) <- gettext(c('Load',  '#Efficients', '#Variables', '#Inputs', '#Outputs', 'Inputs', 'Outputs'))
    s <- s[s[,1] > 0,]
    s <- s[nrow(s):1,]
    print(s, ...)
    invisible(s)
}
