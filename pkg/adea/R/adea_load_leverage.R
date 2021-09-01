#' Search for leverage units (DMU's) with higher impact on load levels in DEA analysis
#'
#' Search for leverage units (DMU's) with higher impact on load levels in DEA analysis.
#' 
#' @inheritParams adea
#' @param load.diff Minimum difference in load to consider a subset of DMUs as a leverage one
#' @param ndel Maximum number of units to drop out in each try.
#' @param nmax Maximum number of DMU sets to include in results. 0 for no limit.
#' @return The indexes of units with higher impact on load levels.
#' @examples
#' data('cardealers4')
#' input <- cardealers4[, 1:2]
#' output <- cardealers4[, 3:4]
#' adea_load_leverage(input, output, ndel = 2)
#'
#' @note This function has to solve a large number of large linear programs that grows with DMUs. So computation time required may be very large, be patient.
#' @importFrom combinat combn
#' @importFrom combinat permn
#' @export
adea_load_leverage <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), load.diff = .05, ndel = 1, nmax = 0)
{
    .output <- function() {
        cat('n:', next.index, ' ')
        cat(paste0(gettext('load'), ':'), .load[next.index])
        cat(paste0(gettext('load.diff'), ':'), .load.diff[next.index])
        cat(paste0(gettext('index'), ': {'), paste0(dmu.indexs[next.index, 1:k], collapse = ', '), '} ')
        cat(paste0(gettext('names'), ': {'), paste0(rownames(input)[dmu.indexs[next.index, 1:k]], collapse = ', '), '}')
        cat('\n')
    }

    ## Check input
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    if (!is.numeric(nmax) || nmax < 0) stop(gettext('adea_load_leverage: nmax must be numeric >= 0'))
    
    ## Setup vars to store results
    .load <- c()
    .load.diff <- c()
    dmu.indexs <- matrix(NA, ncol = ndel, nrow = 0)
    next.index <- 1

    ## Normalise input
    input <- adea_setup(input, output)
    output <- input$output
    input <- input$input
    
    ## Compute initial model
    iload.level <- adea(input, output, orientation = orientation, load.orientation = load.orientation)$load$load
    
    ## Main loop in size
    for (k in 1:ndel) {
        ## Iterates in DMU's
        ic <- combn(1:nrow(input), k)
        for (j in 1:ncol(ic)) {
            i <- ic[,j]
            load.i <- adea(input[-i,], output[-i,], orientation = orientation, load.orientation = load.orientation)$load$load
            load.diff.i <-  abs(iload.level - load.i)
            if (load.diff.i > load.diff) {
                .load[next.index] <- load.i
                .load.diff[next.index] <- load.diff.i
                dmu.indexs <- rbind(dmu.indexs, NA)
                dmu.indexs[next.index, 1:k] <- i
                next.index <- next.index + 1
            }
        }
    }
    
    ## Return result
    if (length(.load.diff) > 1) {
        index <- sort(.load.diff, decreasing = TRUE, index.return = TRUE)
        index <- index$ix
        if (nmax > 1 && nmax < length(index)) index <- index[1:nmax]
        .load <- .load[index]
        .load.diff <- .load.diff[index]
        dmu.indexs <- dmu.indexs[index, ]
    }
    result <- list(load = .load, load.diff = .load.diff, dmu.indexs = dmu.indexs)
    class(result) <- 'adealoadleverage'
    result
}
