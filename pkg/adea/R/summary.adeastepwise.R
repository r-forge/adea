#' Summary method for adeastepwise class
#'
#' For the final model of adea_stepwise function print the model name, orientation, load orientation, a summary, the input variables, and outputs variables.
#' 
#' @name summary.adeastepwise
#' @param object is the object of class adeastepwise to summarise.
#' @param ... Optional arguments to "print".
#' @method summary adeastepwise
#' @export
summary.adeastepwise <- function(object, ...) {
    ## Last model
    .n <- length(object$load)
    cat(gettext('Summary of adea stepwise variable selection for model'), ' "', object$name , '":\n', sep = '')
    cat(gettext('Model orientation'), ': ', object$orientation, '.\n', sep = '')
    cat(gettext('Load orientation'), ': ', object$load.orientation, '.\n', sep = '')
    cat(gettext('Load level required'), ': ', object$load.critical, '.\n', sep = '')
    cat(gettext('Stop criterion reached'), ': ', gettext(object$stop.criterion), '.\n', sep = '')
    cat(gettext('Number of steps'), ': ', object$steps, '.\n', sep = '')
    cat(paste(gettext('Inputs'), ':', paste(names(object$adea$load$ratios$input), collapse = ' '), '\n'))
    cat(paste(gettext('Outputs'), ':', paste(names(object$adea$load$ratios$output), collapse = ' '), '\n'))
    cat(gettext('Efficiencies'), ':', object$adea$eff, '\n')
    ## All models
    cat(gettext('Step by step report'), ':\n', sep = '')
    s <- data.frame(object$load, object$neff, object$nt, object$ni, object$no, object$namesi, object$nameso, object$out)
    colnames(s) <- gettext(c('Load',  '#Efficients', '#Variables', '#Inputs', '#Outputs', 'Inputs', 'Outputs', 'Dropping'))
    s <- s[nrow(s):1, ]
    s <- s[s$Load > 0, ]
    print(s, ...)
    invisible(object)
}
