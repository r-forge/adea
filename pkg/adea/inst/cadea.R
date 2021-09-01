## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library('adea')

## -----------------------------------------------------------------------------
data(tokyo_libraries)
head(tokyo_libraries)

## -----------------------------------------------------------------------------
input <- tokyo_libraries[, 1:4]
output <- tokyo_libraries[, 5:6]
m <- adea(input, output)
summary(m)

## -----------------------------------------------------------------------------
mc <- cadea(input, output, load.min = 0.6, load.max = 4)
summary(mc)

## ----echo=FALSE---------------------------------------------------------------
plot(m$eff, mc$eff, main ='Initial efficiencies vs constrained model efficiencies')

