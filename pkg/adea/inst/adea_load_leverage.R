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
adea_load_leverage(input, output)

## -----------------------------------------------------------------------------
adea_load_leverage(input, output, load.diff = 0.1, ndel = 2)

## -----------------------------------------------------------------------------
adea_load_leverage(input, output, load.diff = 0.1, ndel = 2, nmax = 10)

