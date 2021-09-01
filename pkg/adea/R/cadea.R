#' Make a constrained ADEA analysis
#'
#' This function computes an efficiency score for each DMU and a load for each variable in the current model.
#' But loads or contributions of input or output variables are constrained between the given values.
#' So DMU's efficiencies could change from regular DEA or ADEA model.
#'
#' A variable load is a number between 0 and 1.
#' Where 0 means that the contribution of that variable to the efficiency computations is negligible.
#' In an ideal case, each input or output variable will have a load of 1 divide by the number of them.
#' This model force input and output weights in such a way that final variable loads fall between the given values.
#' 
#' For more information about loads or ADEA model see \code{\link{adea}}
#' 
#' @note If the given limits are too narrow, then the model is infeasible, which will result in an error.
#'
#' @inheritParams adea
#' @param load.min A numeric value or vector giving minimum values for loads.
#' Values for \code{load.min} must belongs to [0, 1).
#' @param load.max A numeric value or vector giving maximum values for loads.
#' Values for \code{load.max} must be greater than 1.
#' If \code{load.min} or \code{load.max} are vectors then its length must be the same as the number of loads to compute.
#' This means, number of inputs when \code{load.orientation} is input, number of outputs when \code{load.orientation} is output, and the sum of both when \code{load.orientation} is inoutput.
#' @return The function returns an object with efficiency scores, one set of weights for inputs and other for outputs, number of efficient units, load ratios and load levels.
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
#' # Get model load ratios
#' model$load$ratios
#' # $input
#' # Employees Depreciation
#' # 0.6666667    1.3333333
#' # $output
#' #  CarsSold WorkOrders
#' # 1.2663476  0.7336524 
#' 
#' # Compute a constrained adea model to force load between .8 and 1.5
#' cmodel <- cadea(input, output, load.min = .8, load.max = 1.5)
#' cmodel
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' 
#' # Get model load ratios
#' cmodel$load$ratios
#' # $input
#' # Employees Depreciation
#' #       0.8          1.2
#' # $output
#' # CarsSold WorkOrders
#' #      1.2        0.8 
#'
#' # See differences of efficiencies in both models
#' model$eff - cmodel$eff
#' #      Dealer A      Dealer B      Dealer C      Dealer D      Dealer E      Dealer F
#' # -2.220446e-16  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  5.942183e-02 
#' @export
cadea <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', load.min, load.max, eff.tolerance = .0001)
{
    ## Define an internal parameter
    ## The number of digit used to round the loads
    load.digits = 4

    ## Check input and output
    err <- adea.check(input = input, output = output, ux = NULL, vy = NULL, eff = NULL)
    if (err != TRUE) stop(err)

    ## Standardise input and output
    dat <- adea_setup(input, output)
    input <- dat$input
    output <- dat$output
    
    ## Check load.min and load.max
    if (!is.numeric(load.min)) stop(paste('cadea:', gettext('load.min value is not numeric')))
    if ( (length(load.min) != 1) && (
        ((length(load.min) != ncol(input)) && (load.orientation == 'input')) ||
        ((length(load.min) != ncol(output)) && (load.orientation == 'output')) ||
        ((length(load.min) != ncol(input) + ncol(output)) && (load.orientation == 'inoutput'))
    )) stop(paste('cadea:', gettext('The length of load.min does not match')))
    if (any(load.min < 0) || any(load.min >= 1)) stop(paste("cadea: load.min", gettext('is not a numeric value or vector in [0, 1)')))
    if (!is.numeric(load.max)) stop(paste('cadea:', gettext('load.max value is not numeric')))
    if ( (length(load.max) != 1) && (
        ((length(load.max) != ncol(input)) && (load.orientation == 'input')) ||
        ((length(load.max) != ncol(output)) && (load.orientation == 'output')) ||
        ((length(load.max) != ncol(input) + ncol(output)) && (load.orientation == 'inoutput'))
    )) stop(paste('cadea:', gettext('The length of load.max does not match')))
    if (any(load.max < 1)) stop(paste("cadea: load.max", gettext('is not a numeric value or vector greater or equal to 1')))
    
    ## Check other parameters
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('cadea: eff.tolerance', gettext('is not a numeric value in [0, 1]')))

    ## Do an standard adea
    .adea <- adea(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, eff.tolerance = eff.tolerance)

    ## Add bounds to adea program and solve again
    .cadea <- lp_solve_cadea(input = input, output = output, eff = .adea$eff, orientation = orientation, load.orientation = load.orientation, load.min = load.min, load.max = load.max, solve = TRUE, lp = .adea$lp)

    ## Compute loads
    .cadea$load <- adea_loads(input = input, output = output, ux = .cadea$ux, vy = .cadea$vy, load.orientation = load.orientation)
    
    ## Compute index
    .cadea$load$iinput <- switch(load.orientation,
                                 input = which(round(.cadea$load$ratios$input - .cadea$load$load, load.digits) == 0),
                                 output = NULL,
                                 inoutput = which(round(.cadea$load$ratios$input - .cadea$load$load, load.digits) == 0)
                                 )
    .cadea$load$ioutput<- switch(load.orientation,
                                 input = NULL,
                                 output = which(round(.cadea$load$ratios$output - .cadea$load$load, load.digits) == 0),
                                 inoutput = which(round(.cadea$load$ratios$output - .cadea$load$load, load.digits) == 0)
                                 )
    
    ## Number of efficiency units
    .cadea$neff <- sum((.cadea$eff < 1 + eff.tolerance) & (.cadea$eff > 1 - eff.tolerance))
    
    ## Drop unused information
    .cadea$status <- NULL
    
    ## Store name and orientation
    .cadea$name <- name
    .cadea$orientation <- orientation
    .cadea$load.orientation <- load.orientation
    
    ## Change the class of the object
    class(.cadea) <- 'cadea'
    return(.cadea)
}
