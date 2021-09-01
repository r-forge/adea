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
adea_hierarchical(input, output)

## ----include=FALSE------------------------------------------------------------
m <- adea_hierarchical(input, output)

## -----------------------------------------------------------------------------
adea_parametric(input, output)

## -----------------------------------------------------------------------------
adea_parametric(input, output, load.orientation = 'output')

## -----------------------------------------------------------------------------
m <- adea_hierarchical(input, output)
m4 <- m$models[[4]]
m4

## -----------------------------------------------------------------------------
summary(m4)

