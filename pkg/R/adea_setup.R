#' Convert input and output in 1 column matrix
#'
#' Setup names for DMUs, input and outputs
#'
#' @keywords internal
#' @inheritParams adea
#' @return a list with input and output converted
adea_setup <- function(input, output)
{
    ## Check for vectors and build matrix
    if (is.vector(input)) input <- matrix(input, ncol = 1)
    if (is.vector(output)) output <- matrix(output, ncol = 1)

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

    return(list(input = input, output = output))
}
