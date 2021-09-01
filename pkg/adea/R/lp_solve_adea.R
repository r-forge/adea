#' Build adea problem
#'
#' For the given input and output build the lp_solve problem and return it.
#'
#' @name lp_solve_adea
#' @aliases lp_solve_adea
#' @keywords internal
#' @inheritParams adea
#' @param solve If TRUE then solve dea model
#' @param eff The efficiency scores from dea analysis.
#' @param lp The problem returned from lp_solve.dea or NULL
#' @return lp adea problem for the given input, output and scores
lp_solve_adea <- function(input, output, eff = NULL, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), solve = FALSE, lp = NULL)
{
    ## Check input and output
    err <- adea.check(input = input, output = output, eff = eff)
    if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)

    ## Check if lp is provided if not, build as first stage
    if (missing(lp) || is.null(lp)) lp <- lp_solve_dea(input = input, output = output, orientation = orientation)$lp

    ## Check for vectors and build matrix
    if (is.vector(input)) input <- matrix(input, ncol = 1)
    if (is.vector(output)) output <- matrix(output, ncol = 1)

    ## Initialise values
    status <- NULL
    ux <- NULL
    vy <- NULL
    ndmu <- .adea.check(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    
    ## Number of alpha variables
    nalpha <- switch(load.orientation,
                     input = ni,
                     output = no,
                     inoutput = nio)

    ## Compute the size of new problem
    ## The variables are: weights for each DMU + alpha + level
    ## To make it easy keep all alpha columns, for inputs and outputs, even when they are not uses
    ncol <- ndmu * nio + nio + 1

    ## The rows are: (ndmu + ndmu^2) + (ndmu + nio)
    nrow <- (ndmu + 2) * ndmu + nio

    ## Add alpha columns
    resize.lp(lp, nrow, ncol) # Resize do not really add columns just add space
    for (i in 1:(nio+1)) add.column(lp, c(0), c(1))

    ## Set objective function
    set.objfn(lp, c(-1), c(ncol))

    ## Change to on row.add.mode improves the performance
    ## Objective function must be before to activate row.add.mode
    ## Seems not to be available after solved
    ## row.add.mode(lp, state = 'on')
    ## Add rows for scores constraints
    if (!is.null(eff)) {
        if (orientation == 'input') {
            for (i in 1:ndmu)
                add.constraint(lp, output[i, ], type = '=', rhs = eff[i], ((i-1) * nio + 1):((i -1) * nio + no))
        } else if (orientation == 'output') {
            for (i in 1:ndmu)
                add.constraint(lp, input[i, ], type = '=', rhs = eff[i], ((i-1) * nio + 1 + no):((i -1) * nio + ni + no))
        } else {
            stop('orientation:', orientation, 'not implemented yet.\n')
        }
    }

    ## Add alpha constraints
    lp_solve_add_alpha_constraints(lp = lp, input = input, output = output, eff = eff, orientation = orientation, load.orientation = load.orientation)
    
    ## Add alpha level constraints
    if (load.orientation == 'input' || load.orientation == 'inoutput') {
        for (i in 1:ni)
            add.constraint(lp, c(-1 , 1), type = '<=', rhs = 0, c(ndmu * nio + i, ncol))
    }
    if (load.orientation == 'output' || load.orientation == 'inoutput') {
        for (i in 1:no)
            add.constraint(lp, c(-1 , 1), type = '<=', rhs = 0, c(ndmu * nio + ni + i, ncol))
    }

    ## Change to off row.add.mode
    ##row.add.mode(lp, state = 'off')

    ## Solve the model
    if (solve) {
        
        ## Build rownames
        if (!is.null(rownames(input))) .rownames <- rownames(input) else { if (!is.null(rownames(output))) .rownames <- rownames(output) else { if (nrow(input)) .rownames <- paste('DMU', 1:nrow(input), sep = '-') else .rownames <- paste('DMU', 1:length(input), sep = '-') } }

        ## Set some options to avoid numeric problems (testing only)
        ## lp.control(lp, verbose = 'full', basis.crash = 'leastdegenerate', simplextype = c('primal', 'primal'))
        
        ## This seems to be the best value for scaling
        ## lp.control(lp, scaling = c("geometric", "dynupdate"))
        lp.control(lp, scaling = c("none"))
        
        ## Call to solver
        status = solve(lp)

        ## Check return status (drop alpha values)
        if (status != 0) stop(paste0(gettext('Unable to solve adea model. Solver status is '), status, '. (', lpsolve.status.txt[status + 1], '.)'))

        ## Get u and v values
        sol <- get.variables(lp)[1:(ndmu * nio)]
        sol <- matrix(sol, nrow = ndmu, ncol = nio, byrow = TRUE)
        rownames(sol) <- .rownames

        ## Get weights for outputs, named vy for compatibility with Benchmarking package
        vy <- sol[, 1:no]

        ## Check for one column
        if (no == 1) vy <- matrix(vy, nrow = ndmu, ncol = 1)
        if (is.null(colnames(output))) colnames(vy) <- paste('v_', 1:no, sep='') else colnames(vy) <- paste('v_', colnames(output), sep='')

        ## Get weights for inputs, named ux for compatibility with Benchmarking package
        ux = sol[, (no+1):nio]

        ## Check for one column
        if (ni == 1) ux <- matrix(ux, nrow = ndmu, ncol = 1)
        if (is.null(colnames(input))) colnames(ux) <- paste('u_', 1:ni, sep='') else colnames(ux) <- paste('u_', colnames(input), sep='')
        ## Compute scores
        eff <- switch(orientation,
                      input = rowSums(vy * output),
                      output = rowSums(ux * input)
                      )
        names(eff) <- rownames(input)
    }

    ## Return the list values
    list(lp = lp, status = status, ux = ux, vy = vy, eff = eff);
}

## Auxiliary function to add alpha constraint to an adea model
lp_solve_add_alpha_constraints <- function(lp, input, output, eff, orientation, load.orientation) {

    ## Compute needed numbers
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    
    ## Add alpha constraints for inputs
    if (load.orientation == 'input' || load.orientation == 'inoutput') {
        for (i in 1:ni)
        {
            xt <- switch(orientation,
                         input = c(input[, i] * ni / ndmu, -1),
                         output = c(input[, i] * ni / sum(eff), -1)
                         )
            indices <- seq(no + i, ndmu * nio, by = nio)
            indices <- c(indices, ndmu * nio + i)
            add.constraint(lp, xt, type = '=', rhs = 0, indices = indices)
        }
    }
    ## Add alpha constraints for outputs
    if (load.orientation == 'output' || load.orientation == 'inoutput') {
        for (i in 1:no)
        {
            xt <- switch(orientation,
                         input = c(output[, i] * no / sum(eff), -1),
                         output = c(output[, i] * no / ndmu, -1)
                         )
            indices <- seq(i, ndmu * nio, by = nio)
            indices <- c(indices, ndmu * nio + i + ni)
            add.constraint(lp, xt, type = '=', rhs = 0, indices = indices)
        }
    }
}

## Definition of the lpsolve status text messages
lpsolve.status.txt <- c(gettext("Optimal solution found"),
                        gettext("The model is sub-optimal"),
                        gettext("The model is infeasible"),
                        gettext("The model is unbounded"),
                        gettext("The model is degenerate"),
                        gettext("Numerical failure encountered"),
                        gettext("Process aborted"),
                        gettext("Timeout"),
                        gettext("The model was solved by presolve"),
                        gettext("The branch and bound routine failed"),
                        gettext("The branch and bound was stopped because of a break-at-first or break-at-value"),
                        gettext("A feasible branch and bound solution was found"),
                        gettext("No feasible branch and bound solution was found"))

