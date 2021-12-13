#' Selection of an optimal subset of variables for DEA analysis
#'
#' The function returns a list of DEA models dropping one variable in each step following ADEA methodology.
#'
#' This procedure provides a list of all DEA models for all nested sets of variables.
#' 
#' In each model, the variable with lower value of load is dropped.
#' In this case, the load of a new model can be lower than a previous one.
#'
#' See examples for more details.
#' 
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @inheritParams adea
#' @inheritParams adea_stepwise
#' 
#' @return A list with all computed models. So, you can retrieve each model individually.
#' @seealso \code{\link{adea_parametric}}
#' @examples
#' # Read data
#' data('cardealers4')
#' input <- cardealers4[, 1:2]
#' output <- cardealers4[, 3:4]
#' 
#' # Compute all dea models in hierarchical way
#' adea_hierarchical(input, output)
#' #        Load #Efficient Factors #Inputs #Outputs                  Inputs              Outputs
#' # 4 0.6666667          2       4       2        2 Employees, Depreciation CarsSold, WorkOrders
#' # 3 0.9575672          2       3       1        2            Depreciation CarsSold, WorkOrders
#' # 2 1.0000000          1       2       1        1            Depreciation CarsSold
#' 
#' # Compute again and store result in sol.ah
#' sol.ah <- adea_hierarchical(input, output)
#' 
#' # Summary the model with 3 variables
#' summary(sol.ah$models[[3]])
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
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' 
#' # Get efficiencies for the model with 3 variables
#' sol.ah$models[[3]]$eff
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044 
#' @export
adea_hierarchical <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), verbose = 0) {
    .adea <- stepwise(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, direction = direction, verbose = verbose, load.critical = 1)
    ## Drop non useful information
    .adea$load <- NULL
    .adea$lp <- NULL
    .adea$load.critical <- NULL
    .adea$steps <- NULL
    .adea$stop.criterion <- NULL
    ## Set class
    class(.adea) <- 'adeahierarchical'
    .adea
}
