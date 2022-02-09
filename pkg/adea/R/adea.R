#' ADEA analysis to variable selection in DEA
#'
#' ADEA analysis, computes a score for each DMU and load ratio for each variable.
#'
#' This function computes an efficiency score for each DMU, the same as in standard DEA model.
#'
#' Then a load ratio for each variable is computed searching two new set of weights while keeping DMU's scores.
#'
#' The load ratio of a variable is a number between 0 and 1.
#' Where 0 means that the contribution of that variable to the efficiency computations is negligible.
#' In an ideal case, each input or output variable will have a load of 1 divide by the number of them.
#'
#' As it is usually done in DEA this load ratio has been computed as its maximum allowable value.
#' But because the sum of all of them is 1, when one increases its load ratio any other decreases its value.
#' So only the lowest value of all load ratios, this is load model, has a real meaning.
#' This lowest value can be taken as a significance measure of the entire model.
#'
#' @param input A matrix or a data frame with the inputs of units to be evaluated, one row for each DMU and one column for each input.
#' @param output A matrix or a data frame with the outputs of units to be evaluated, one row for each DMU and one column for each output.
#' @param orientation Use "input" for input orientation or use "output" for output orientation in DEA model.
#' @param load.orientation It allows the selection of variables to be included in load analysis.
#' Its default value is "inoutput" which means that all input and all output variables will be included. Use "input" or "output" to include only input or output variables in load analysis.
#' @param eff.tolerance A value between 0 and 1 to tolerance when considering a DMU as efficient in reports.
#' @param name An optional descriptive name for the model. It will be shown in print and summary results.
#' @return The function return an adea class object with the following named members:
#' \itemize{
#' \item eff is a vector with DMU's scores
#' \item neff is the number of efficient DMU with eff.tolerance. It means DMUs with efficiencies between 1 - eff.tolerance and 1 + eff.tolerance.
#' \item load.orientation is the load orientation, one of 'input', 'output' or 'inoutput'. The last is the default value.
#' \item name: A label of the model
#' \item orientation: DEA model orientation 'input' or 'output'
#' \item ux: A set of weights for inputs
#' \item vy: A set of weights for output
#' \item load: A list with all information about loads:
#'   \itemize{
#'     \item ratios$input: A vector with load ratios of input variables
#'     \item ratios$output: A vector with load ratios of output variables
#'     \item load: The lowest load ratio, which is the load of the ADEA model
#'     \item lp: A pointer to the linear programming program of the model. Mainly for research purpose
#'     \item iinput: A vector of index of inputs that almost reach the load level
#'     \item ioutput: A vector of index of outputs that almost reach the load level
#'     \item vinput: Standardized virtual input dividing by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}.
#'     \item voutput: Standardized virtual output dividing by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}.
#'   }
#' }
#' @seealso \code{\link{adea-package}}.
#' @examples
#' data('cardealers4')
#' input <- cardealers4[, 1:2]
#' output <- cardealers4[, 3:4]
#' 
#' # Compute adea model
#' model <- adea(input, output)
#' model
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' 
#' # Get model's load
#' model$load$load
#' # [1] 0.6666667
#' 
#' # Get model's load ratios
#' model$load$ratios
#' # $input
#' # Employees Depreciation
#' # 0.6666667    1.3333333
#' # $output
#' # CarsSold WorkOrders
#' # 1.2663476  0.7336524 
#' 
#' @export
adea <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', eff.tolerance = .001)
{
    ## Define an internal parameter
    ## The number of digit used to round the loads
    load.digits = 4
    
    ## Check input and output
    err <- adea.check(input = input, output = output, ux = NULL, vy = NULL, eff = NULL)
    if (err != TRUE) stop(err)

    ## Check other parameters
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('adea:', gettext('eff.tolerance is not numeric a value between 0 and 1')))

    ## Check for vectors and build matrix
    if (is.vector(input)) input <- matrix(input, ncol = 1)
    if (is.vector(output)) output <- matrix(output, ncol = 1)
    
    ## Setup input and output names
    if (is.null(colnames(input))) colnames(input) <- paste('input_', 1:ncol(input), sep='')
    if (is.null(colnames(output))) colnames(output) <- paste('output_', 1:ncol(output), sep='')
    
    ## Setup DMU names
    if (is.null(rownames(input))) {
        if (is.null(rownames(output))) rownames(output) <- paste('DMU', 1:nrow(input), sep = '-')
        rownames(input) <- rownames(output)
    } else {
        if (is.null(rownames(output))) rownames(output) <- rownames(input)
    }



    ## Solve dea model
    .dea <- lp_solve_dea(input = input, output = output, orientation = orientation, solve = TRUE)

    ## Compute loads
    .dea.loads <- adea_loads(input, output, .dea$ux, .dea$vy)

    ## Solve adea model
    .adea <- lp_solve_adea(input = input, output = output, eff = .dea$eff, orientation = orientation, load.orientation = load.orientation, solve = TRUE, lp = .dea$lp)
    
    ## Compute loads
    .adea.loads <- adea_loads(input, output, .adea$ux, .adea$vy, load.orientation = load.orientation)
    
    ## Check for consistency
    if (
        any(.adea.loads$ratios$input < .adea.loads$ratios$input[.dea.loads$iinput]) &&
        any(.adea.loads$ratios$output < .adea.loads$ratios$output[.dea.loads$inoutput])
    ) stop(paste(gettext("Internal error: The minimum value in first and second stage don't agree.")))
    
    ## Store rations
    .adea.loads$ratios <- switch(load.orientation,
                                 input = list(input = .adea.loads$ratios$input),
                                 output = list(output = .adea.loads$ratios$output),
                                 inoutput = list(input = .adea.loads$ratios$input, output = .adea.loads$ratios$output)
                                 )
    ## Compute model load
    .adea.loads$load <- switch(load.orientation,
                               input = min(.adea.loads$ratios$input),
                               output = min(.adea.loads$ratios$output),
                               inoutput = min(.adea.loads$ratios$input, .adea.loads$ratios$output)
                               )
    ## Compute index
    .adea.loads$iinput <- switch(load.orientation,
                                 input = which(round(.adea.loads$ratios$input - .adea.loads$load, load.digits) == 0),
                                 output = NULL,
                                 inoutput = which(round(.adea.loads$ratios$input - .adea.loads$load, load.digits) == 0)
                                 )
    .adea.loads$ioutput<- switch(load.orientation,
                                 input = NULL,
                                 output = which(round(.adea.loads$ratios$output - .adea.loads$load, load.digits) == 0),
                                 inoutput = which(round(.adea.loads$ratios$output - .adea.loads$load, load.digits) == 0)
                                 )

    ## Prepare list to return
    .adea$load <- .adea.loads

    ## Include efficiencies scores
    .adea$eff <- .dea$eff
    
    ## Drop unused information
    .adea$status <- NULL
    ##.adea$lp <- NULL
    
    ## Store name and orientation
    .adea$name <- name
    .adea$orientation <- orientation
    .adea$load.orientation <- load.orientation

    ## Number of efficiency units
    .adea$neff <- sum((.dea$eff < 1 + eff.tolerance) & (.dea$eff > 1 - eff.tolerance))

    ## Store input and ouput
    ## .adea$input <- input
    ## .adea$output <- output

    ## Compute and store virtual input and output as described in @references A new approach to the bi-dimensional representation of the DEA efficient frontier with multiple inputs and outputs
    if (orientation == 'output') s <- rowSums(.adea$vy)
    if (orientation == 'input') s <- rowSums(.adea$ux)
    ux <- .adea$ux/s
    vy <- .adea$vy/s
    .adea$vinput <- rowSums(ux * input)
    .adea$voutput <- rowSums(vy * output)
    
    ## Change the class of the object
    class(.adea) <- 'adea'
    return(.adea)
}
