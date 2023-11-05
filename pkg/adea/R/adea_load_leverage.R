#' Search for leverage units (DMU's) with a greater impact on load levels in DEA analysis
#'
#' Search for leverage units (DMU's) with a greater impact on load levels in DEA analysis.
#'
#' A leverage unit is a DMU that significantly alters the results of the current procedure, in this case, a DMU that produce a large change in variable loads.
#' 
#' @inheritParams adea
#' @param load.diff Minimum difference in load to consider a subset of DMUs as a leverage one
#' @param ndel Maximum number of units to drop out in each try.
#' @param nmax Maximum number of DMU sets to include in results. 0 for no limit.
#' @return The function returns a list with the following named members:
#' \itemize{
#' \item loads: Load of each model after removing the corresponding DMUs
#' \item loads.diff: For each model the difference between its load and the original one
#' \item dmu.indexs: Index of DMUs removed in each model
#' }
#' @examples
#' data('cardealers4')
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' adea_load_leverage(input, output, ndel = 2)
#' #         load  load.diff DMUs
#' # 1  1.0000000 0.33333333 1, 6
#' # 2  1.0000000 0.33333333 3, 4
#' # 3  1.0000000 0.33333333 2, 3
#' # 4  1.0000000 0.33333333 2, 5
#' # 5  1.0000000 0.33333333 4, 6
#' # 6  1.0000000 0.33333333    2
#' # 7  1.0000000 0.33333333 1, 4
#' # 8  1.0000000 0.33333333 2, 6
#' # 9  1.0000000 0.33333333 1, 2
#' # 10 0.9635628 0.29689609 2, 4
#' # 11 0.8743243 0.20765766 5, 6
#' # 12 0.8479940 0.18132736 1, 3
#' # 13 0.8420551 0.17538843 3, 6
#' # 14 0.8243243 0.15765766 1, 5
#' # 15 0.8000000 0.13333333    6
#' # 16 0.8000000 0.13333333    4
#' # 17 0.8000000 0.13333333    1
#' # 18 0.8000000 0.13333333    3
#' # 19 0.7461771 0.07951041 3, 5
#' # 20 0.7358231 0.06915643    5
#' 
#' @note This function has to solve a large number of large linear programs that grows with DMUs. So computation time required may be very large, be patient.
#' @importFrom combinat combn
#' @importFrom combinat permn
#' @export
adea_load_leverage <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), load.diff = .05, ndel = 1, nmax = 0)
{
    .output <- function() {
        cat('n:', next.index, ' ')
        cat(paste0(gettext('loads'), ':'), .loads[next.index])
        cat(paste0(gettext('loads.diff'), ':'), .loads.diff[next.index])
        cat(paste0(gettext('index'), ': {'), paste0(dmu.indexs[next.index, 1:k], collapse = ', '), '} ')
        cat(paste0(gettext('names'), ': {'), paste0(rownames(input)[dmu.indexs[next.index, 1:k]], collapse = ', '), '}')
        cat('\n')
    }

    ## Check input
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    if (!is.numeric(nmax) || nmax < 0) stop(gettext('adea_load_leverage: nmax must be numeric >= 0'))

    ## Check input and output
    err <- adea_check(input = input, output = output)
    if (!isTRUE(err)) stop(err)    

    ## Setup vars to store results
    .loads <- c()
    .loads.diff <- c()
    dmu.indexs <- matrix(NA, ncol = ndel, nrow = 0)
    next.index <- 1

    ## Normalise input
    input <- adea_setup(input, output)
    output <- input$output
    input <- input$input
    
    ## Compute initial model
    iload.level <- adea(input, output, orientation = orientation, load.orientation = load.orientation)$loads$load
    
    ## Main loop in size
    for (k in 1:ndel) {
        ## Iterates in DMU's
        ic <- combn(1:nrow(input), k)
        for (j in 1:ncol(ic)) {
            i <- ic[,j]
            load.i <- adea(input[-i,], output[-i,], orientation = orientation, load.orientation = load.orientation)$loads$load
            load.diff.i <-  abs(iload.level - load.i)
            if (load.diff.i > load.diff) {
                .loads[next.index] <- load.i
                .loads.diff[next.index] <- load.diff.i
                dmu.indexs <- rbind(dmu.indexs, NA)
                dmu.indexs[next.index, 1:k] <- i
                next.index <- next.index + 1
            }
        }
    }

    ## Return result
    if (length(.loads.diff) > 1) {
        index <- sort(.loads.diff, decreasing = TRUE, index.return = TRUE)
        index <- index$ix
        if (nmax > 1 && nmax < length(index)) index <- index[1:nmax]
        .loads <- .loads[index]
        .loads.diff <- .loads.diff[index]
        dmu.indexs <- dmu.indexs[index, ]
    } else {
        if (length(.loads.diff) == 0) {
            dmu.indexs <- NULL
        }
    }
    result <- list(loads = .loads, loads.diff = .loads.diff, dmu.indexs = dmu.indexs)
    class(result) <- 'adealoadleverage'
    result
}
