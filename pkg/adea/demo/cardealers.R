## Read data
data('cardealers4')
input <- cardealers4[, 1:2]
output <- cardealers4[, 3:4]

## Solve dea model using adea from adea package
adea(input, output)

## Solve again and store result in sol.dea
sol.adea <- adea(input, output)
## Scores
sol.adea$eff
## Weights
cbind(sol.adea$ux, sol.adea$vy)
## Load ratios
sol.adea$load$ratios
## Load levels
sol.adea$load$load

## Compute all dea models in hierarchical way
adea_hierarchical(input, output)
## Compute again and store result in sol.ah
sol.ah <- adea_hierarchical(input, output)
## Get the model with 3 variables
sol.ah$models[[3]]
## Get efficiencies for model with 3 variables
sol.ah$models[[3]]$eff
## Get inputs in model with 3 variables
sol.ah$namesi[[3]]
## Get outputs in model with 3 variables
sol.ah$nameso[[3]]

## Compute all dea models in parametric way
adea_parametric(input, output)
## Compute again and store result in sol.ap
sol.ap <- adea_parametric(input, output)
## Get the model with 3 variables
sol.ap$models[[3]]
## Get efficiencies for model with 3 variables
sol.ap$models[[3]]$eff
## Get inputs in model with 3 variables
sol.ap$namesi[[3]]
## Get outputs in model with 3 variables
sol.ap$nameso[[3]]

