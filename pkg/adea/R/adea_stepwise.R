#' Select an optimal subset of variables for DEA analysis
#'
#' Stepwise procedure for variable selection in DEA models.
#' This is a back end function for adea_hierarchical and adea_parametric functions.
#' So, it is not for end user use.
#' 
#' @inheritParams adea
#' @inheritParams adea_parametric
#' @param direction The direction in which the variables go in and out of the model. Until now, only backward option is implemented.
#' @param load.critical Minimum values for load.ratios to consider that a variable should be considered in the model. It can be also a vector with two values, the first value input loads and the second for output loads.
#' @param max.steps The maximum number of steps allowed.
#' @param verbose Use 0 for minimal output, only final model. 1 or more to get detailed information of each step. This option affects only to printed output but not the result.
#' @return The function returns a DEA model with optimised set of variables.
#' @export
adea_stepwise <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), load.critical = .5, max.steps = ncol(input) + ncol(output) - 2, verbose = 0) {
    .adea <- stepwise(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, direction = direction, load.critical = load.critical, max.steps = max.steps, verbose = verbose)
    .adea$adea <- .adea$models[[length(.adea$models) - .adea$steps]]
    .adea$models <- NULL
    class(.adea) <- 'adeastepwise'
    .adea
}

stepwise <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), load.critical = .5, max.steps = ncol(input) + ncol(output) - 2, verbose = 0) {
    ## Check input and output
    err <- adea.check(input = input, output = output)
    if (err != TRUE) stop(err)

    ## Check orientation
    orientation <- match.arg(orientation)

    ## Check direction
    direction <- match.arg(direction)

    ## Check load.orientation
    load.orientation <- match.arg(load.orientation)
    
    ## Check for vectors and build matrix
    if (is.vector(input)) input <- matrix(input, ncol = 1)
    if (is.vector(output)) output <- matrix(output, ncol = 1)

    ## Setup input and output names
    if (is.null(colnames(input))) colnames(input) <- paste('input_', 1:ncol(input), sep='')
    if (is.null(colnames(output))) colnames(output) <- paste('output_', 1:ncol(output), sep='')

    ## Setup DMU names
    if (is.null(rownames(input))) {
        if (is.null(rownames(output))) rownames(input) <- paste('DMU', 1:nrow(input), sep = '-')
        rownames(input) <- rownames(output)
    } else {
        if (is.null(rownames(output))) rownames(output) <- rownames(input)
    }
    
    ## Check load.critical value
    if (load.critical < 0 || load.critical > 1) stop(gettext('load.critical value should be a positive number less than 1.'))
    direction <- match.arg(direction)

    ## Compute initial adea model
    .adea <- switch(direction,
                    'backward' = stepwisebackward(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.critical = load.critical, max.steps = max.steps, index.input = 1:ncol(input), index.output = 1:ncol(output), verbose = verbose),
                    'backward/input' = stepwisebackward(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.critical = load.critical, max.steps = max.steps, index.input = 1:ncol(input), index.output = NULL, verbose = verbose),
                    'backward/output' = stepwisebackward(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.critical = load.critical, max.steps = max.steps, index.input = NULL, index.output = ncol(input) + 1:ncol(output), verbose = verbose)
                    )

    ## Return object with results
    .adea
}

### Currently the only step-by-step procedure implemented
stepwisebackward <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', load.critical = .5, max.steps = ncol(input) + ncol(output), index.input = 1:ncol(input), index.output = 1:ncol(output), verbose = 0)
{

    ## Setup initial values
    .step <- 1
    .nt <- ncol(input) + ncol(output)
    i <- .nt
    iter <- 0

    dropping <- character(.nt)
    load <- numeric(.nt)
    load[i] <- 0
    models <- list()
    ni <- numeric(.nt)
    namesi <- character(.nt)
    no <- numeric(.nt)
    nameso <- character(.nt)
    neff <- numeric(.nt)
    nt <- 1:.nt
    out <- character(.nt)
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)

    ## Compute initial adea model
    .adea <- adea(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name)

    ## Setup .index
    .index <- list(input = index.input, output = index.output)

    ## Store data log
    load[i] <- .adea$load$load
    neff[i] <- .adea$neff
    ni[i] <- length(.index$input)
    no[i] <- length(.index$output)
    nt[i] <- ni[i] + no[i]
    namesi[i] <- paste(colnames(input)[.index$input], collapse = ", ")
    nameso[i] <- paste(colnames(output)[.index$output], collapse = ", ")

    ## Start output log
    if (verbose > 0) {
        cat(gettext('\nStarting adea_stepwise using backward selection\n'))
        cat(gettext('load.critical value is'), load.critical, '\n')
    }

    ## Main stepwise loop
    while (.adea$load$load < load.critical && .step <= max.steps && i > 2) {

        ## Check one column case
        if ((load.orientation == 'input') && (length(.index$input) == 1)) break;
        if ((load.orientation == 'output') && (length(.index$output) == 1)) break;
        if ((load.orientation == 'inoutput') && (length(.index$input) == 1) && (length(.index$output) == 1)) break;
        
        ## Store data log
        load[i] <- .adea$load$load
        models[[i]] <- .adea
        neff[i] <- .adea$neff
        ni[i] <- length(.index$input)
        no[i] <- length(.index$output)
        nt[i] <- ni[i] + no[i]
        namesi[i] <- paste(colnames(input)[.index$input], collapse = ", ")
        nameso[i] <- paste(colnames(output)[.index$output], collapse = ", ")
        
        ## Compute the going out variable
        ## Next drop is input or output?
        .inout <- switch(load.orientation,
                         input = 'input',
                         output = 'output',
                         inoutput = ifelse(min(.adea$load$ratios$input) < min(.adea$load$ratios$output) , 'input', 'output')
                         )

                                        # print("***")
                                        # print(load.orientation)
                                        # print(.inout)
        ## Compute the outgoing out variable
        .out <- switch(load.orientation,
                       input = which.min(.adea$load$ratios$input),
                       output = which.min(.adea$load$ratios$output),
                       inoutput = ifelse(min(.adea$load$ratios$input) < min(.adea$load$ratios$output), which.min(.adea$load$ratios$input), which.min(.adea$load$ratios$output))
                       )
        
        ## Store data log and output
        out[i] <- colnames(get(.inout))[.index[[.inout]][.out]]

        if (verbose > 0) {
            cat(sprintf(gettext('Step %d dropping %s %s with load ratio equal to %f\n'), .step, .inout, colnames(get(.inout))[.index[[.inout]][.out]], .adea$load$ratios[[.inout]][.out]))
        }
        
        ## Output load ratios values
        if (verbose > 0) {
            cat('\n', gettext('Load ratios values for current model'), ':\n')
            print(.adea$load$ratios)
        }
        
        ## Drop variable
        .index[[.inout]] <- .index[[.inout]][-.out]
        
        ## Compute new model
        .adea <- adea(input = input[, .index$input], output = output[, .index$output], orientation = orientation, load.orientation = load.orientation, name = name)
        
        ## Avoid degenerate case
        if (length(.adea$load$ratios$input) == 1) {
            names(.adea$load$ratios$input) <- colnames(input)[.index$input[1]]
            colnames(.adea$ux) <- paste('u_', colnames(input)[.index$input[1]], sep = '')
        }
        if (length(.adea$load$ratios$output) == 1) {
            names(.adea$load$ratios$output) <- colnames(output)[.index$output[1]]
            colnames(.adea$vy) <- paste('v_', colnames(output)[.index$output[1]], sep = '')
        }
        
        ## Next step
        .step <- .step + 1
        i <- i - 1
    }

    ## Store data log for last step
    load[i] <- .adea$load$load
    models[[i]] <- .adea
    neff[i] <- .adea$neff
    ni[i] <- length(.index$input)
    no[i] <- length(.index$output)
    nt[i] <- ni[i] + no[i]
    namesi[i] <- paste(colnames(input)[.index$input], collapse = ", ")
    nameso[i] <- paste(colnames(output)[.index$output], collapse = ", ")

    ## Output verbose information
    if (verbose > 0) {
        cat('\n', gettext('Load ratios values for final model'), ':\n')
        print(.adea$load$ratios)
        cat(gettext("The reason for stop was that the"), ifelse(.step > max.steps, gettext('maximum number of steps has been reached.\n'), gettext('load level given has been reached.\n')))
    }

    ## Drop empty steps
    .steps <- load > 0
    .adeasw <- list(load = load, load.critical = load.critical, load.orientation = load.orientation, models = models, name = name, namesi = namesi, nameso = nameso, neff = neff, ni = ni, no = no, nt = nt, orientation = orientation, out = out, steps = .step -1, stop.criterion = ifelse(.step > max.steps, 'Steps', 'Level'))

    ## Set class of returning object
    class(.adeasw) <- 'adeastepwise'
    .adeasw
}
