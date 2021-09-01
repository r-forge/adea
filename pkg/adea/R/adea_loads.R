#' Compute the variables load ratios in DEA
#'
#' For the given input, output and two sets of weights, the function computes the loads of each variable for such values.
#'
#' In DEA analysis, even when the efficiency scores remain constants, there are a great level of freedom to select the sets of weights for input and output variables.
#' 
#' Not all those sets of weights attaches the same importance to the variables. 
#' This function allows to compute the load of each variable for the given weights in order to compare different sets of weights for the same efficiencies scores.
#' Also compute load.levels which are the minimum value of such loads.
#' 
#' Take into account that different sets of weights means different ways to model the efficiency.
#'
#' This function does not solve any model, only if ux and vy are the optimal values for the adea model, this function provides the load-levels as described in the theoretical adea model.
#'
#' @inheritParams adea
#' @param ux A matrix of weights for DMUs and input variables.
#' @param vy A matrix of weights for DMUs and output variables.
#' @examples
#' # Load data
#' data('cardealers4')
#' # Define input and output
#' input <- cardealers4[, 1:2]
#' output <- cardealers4[, 3:4]
#' # Make dea analysis
#' model <- dea(input, output, RTS = 'crs', DUAL = TRUE)
#' # Show results
#' model
#' # Compute loads for such weights
#' adea_loads(input, output, model$ux, model$vy)
#' 
#' @return Loads ratios and load for input and output variables
#' @export
adea_loads <- function(input, output, ux, vy, load.orientation = c('inoutput', 'input', 'output'))
{
    ## Check input and output
    err <- adea.check(input = input, output = output, ux = ux, vy = vy, eff = NULL)
    if (err != TRUE) stop(err)
    
    ## Check for vectors and build matrix
    if (is.vector(input)) input <- matrix(input, ncol = 1)
    if (is.vector(output)) output <- matrix(output, ncol = 1)

    ## Check load.orientation
    load.orientation <- match.arg(load.orientation)

    ## Setup input and output names
    if (is.null(colnames(input))) colnames(input) <- paste('input_', 1:ncol(input), sep='')
    if (is.null(colnames(output))) colnames(output) <- paste('output_', 1:ncol(output), sep='')
    ## Setup DMU names
    if (is.null(rownames(input))) {
        if (is.null(rownames(output))) rownames(input) <- paste('DMU', 1:nrow(input), sep = '-')
        rownames(output) <- rownames(input)
    } else {
        if (is.null(rownames(output))) rownames(output) <- rownames(input)
    }

    ## Compute load ratios: normalised contribution of an input inside all inputs (or output)
    load <- list()
    if ((load.orientation == 'input') || (load.orientation == 'inoutput')) {
        load$ratios$input <- ncol(input) * colSums(ux * input) / sum(ux * input)
        names(load$ratios$input) <- colnames(input)
    }
    if ((load.orientation == 'output') || (load.orientation == 'inoutput')) {    
        load$ratios$output <- ncol(output) * colSums(vy * output) / sum(vy * output)
        names(load$ratios$output) <- colnames(output)
    }

    load$load <- switch(load.orientation,
                        input = min(load$ratios$input),
                        output = min(load$ratios$output),
                        inoutput = min(min(load$ratios$input), min(load$ratios$output))
                        )
    
    return(load)
}
