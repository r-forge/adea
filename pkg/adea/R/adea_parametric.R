#' Selection of an optimal subset of variables for DEA analysis
#'
#' The function returns a list of DEA models removing at least one variable in each step, to get a sequence of models with increasing values of adea loads.
#'
#' This procedure provides a list of all DEA models for all different threshold for load values.
#'
#' The models are sorted by increasing values of loads from initially given model to the one with one input and one output variable.
#' Note that the load value for the model with one input and one output is 1.
#' In each step at least one variable is dropped, but more than one can be dropped in each step.
#'
#' See example for more details.
#' 
#' @inheritParams adea
#' @inheritParams adea_stepwise
#' @return A list with all computed models. So, you can retrieve each model individually.
#' @seealso \code{\link{adea_hierarchical}}
#' @examples
#' # Read data
#' data('cardealers4')
#' input <- cardealers4[, 1:2]
#' output <- cardealers4[, 3:4]
#' 
#' # Compute all dea models in parametric way
#' adea_parametric(input, output)
#' #        Load #Efficient Factors #Inputs #Outputs                  Inputs              Outputs
#' # 4 0.6666667          2       4       2        2 Employees, Depreciation CarsSold, WorkOrders
#' # 3 0.9575672          2       3       1        2            Depreciation CarsSold, WorkOrders
#' # 2 1.0000000          1       2       1        1            Depreciation             CarsSold
#' 
#' # Compute again and store result in sol.ap
#' sol.ap <- adea_parametric(input, output)
#' 
#' # Summary the model with 3 variables
#' summary(sol.ap$models[[3]])
#' # Model name:
#' # Orientation is input
#' # Inputs: Depreciation
#' # Outputs: CarsSold WorkOrders
#' # Load: 0.9575672
#' # Input loads: 1
#' # Output loads: 1.042433 0.9575672
#' # Summary of efficiencies:
#' # Mean        sd      Min.   1st Qu.    Median   3rd Qu.      Max.
#' # 0.9002232 0.1351949 0.6515044 0.8722527 0.9422250 0.9978982 1.0000000
#' # Efficiencies:
#' #  Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F 
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' 
#' # Get efficiencies for the model with 3 variables
#' sol.ap$models[[3]]$eff
#' #  Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' @export
adea_parametric <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), verbose = 0) {
    .adea <- stepwise(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, direction = direction, verbose = verbose, load.critical = 1)
    ## Drop non useful information
    .adea$lp <- NULL
    .adea$load.critical <- NULL
    .adea$steps <- NULL
    .adea$stop.criterion <- NULL
    ## Drop decreasing load models
    models <- list()
    .load <- .adea$models[[length(.adea$models)]]$load$load
    if (length(.adea$models) >= 3) {
        models[[length(.adea$models)]] <- .adea$models[[length(.adea$models)]]
        for (i in length(.adea$models):3) {
        if (!is.null(.adea$models[[i-1]]$load$load)) {
            if (.load < .adea$models[[i-1]]$load$load) {
                .load <- .adea$models[[i-1]]$load$load
                models[[i-1]] <- .adea$models[[i-1]]
            } else {
                .adea$namesi[i-1] <- ''
                .adea$nameso[i-1] <- ''
                .adea$neff[i-1] <- 0
                .adea$ni[i-1] <- 0
                .adea$no[i-1] <- 0
                .adea$nt[i-1] <- 0
                .adea$load[i-1] <- 0
                .adea$out[i] <- paste0(.adea$out[i], ', ', .adea$out[i-1])
                .adea$out[i-1] <- ''
            }
        }
        }
        }
    .adea$models <- models
    ## Set class
    class(.adea) <- 'adeaparametric'
    .adea
}
