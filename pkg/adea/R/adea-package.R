#' Data Envelopment Analysis: Variable Selection, Constrained ADEA and Leverage Units
#'
#' @aliases adea-package
#' @docType package
#' @name adea-package
#'
#' @description
#' \preformatted{
#'
#' Package: adea
#'
#' Version: 1.2.1
#'
#' Date: 2021-12-13
#'
#' License: GPL (>= 3)
#' }
#' 
#' DEA, that means Data Envelopment Analysis, consider a set of DMUs (Decision Making Units) and computes an efficiency score for each DMU.
#' Such score is defined as a weighted ratio between several inputs and outputs values for such DMU.
#' 
#' This package provides an implementation of the ADEA method for variable selection in DEA.
#' ADEA methodology includes a new phase in the classical DEA analysis that measures the relative importance of each input and output variable.
#' This above-mentioned measure is called load ratio or contribution.
#' A load for the whole model is also defined.
#' Using such measure, a procedure to select an optimised or relevant set of variables has been developed.
#'
#' A variable load is a number between 0 and 1.
#' Where 0 means that the contribution of that variable to the efficiency values is negligible.
#' In an ideal case, each input or output variable will have a load of 1.
#'
#' As it is usually done in DEA, these loads are computed as its maximum allowable value.
#' Using alternative sets of weights, this procedure don't change efficiency scores.
#' But because the sum of all of them is 1, when one variable increases its load, any other decrease in value.
#' So only the lowest value of all loads has a real meaning.
#' This lowest value can be taken as a significance measure of the entire model.
#'
#' This measure, load, has two important properties that easy its interpretation:
#' \itemize{
#'    \item It has a bounded range from 0 to the number of input or output, and 1 as its ideal value.
#'    \item It is invariant by changes of scale.
#' }
#' 
#' ADEA analysis can be done measuring only input variables, in this case ADEA analysis has input \code{load.orientation}.
#' output when only output variables are considered.
#' And inoutput \code{load.orientation} when all variables in the model are taken into account.
#' 
#' The package is named after its main function.
#' adea makes a DEA analysis using alternative optimal solution of the same programs that DEA uses to compute efficiency scores.

#' For a detailed description of the maths behind the model, see the references.
#'
#' The main functions that this package provides are:
#' \itemize{
#'    \item adea: It makes ADEA analysis giving, efficiency scores for each DMU, a set of weight, and a load for each input and output variable, and also model load.
#'    \item cadea: Constrained ADEA analysis to force that variable loads fall in a given range. The efficiencies scores will change in order to allow this.
#'    \item adea_load_average: Search for DMU's with higher impact on ADEA model.
#' }
#'
#' @note The package is ready for translations, so contributions with translated versions of po files will be very welcomed.
#' 
#' @author Fernando Fernandez-Palacin <fernando.fernandez@uca.es> and Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' Mantainer: Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' @keywords package DEA
#'
#' @references Stepwise Selection of Variables in DEA Using Contribution Load. \emph{F. Fernandez-Palacin}, \emph{M. A. Lopez-Sanchez} and \emph{M. Munoz-Marquez}. Pesquisa Operacional 38 (1), pg. 1-24, 2018. <DOI:10.1590/0101-7438.2018.038.01.0000>.
#' @references Methodology for calculating critical values of relevance measures in variable selection methods in data envelopment analysis. \emph{Jeyms Villanueva-Cantillo} and \emph{Manuel Munoz-Marquez}. European Journal of Operational Research, 290 (2), pg. 657-670, 2021. <DOI:10.1016/j.ejor.2020.08.021>.
#' @import Benchmarking
#' @import knitr
#' @import lpSolveAPI
#' @import methods
#' @import rmarkdown
#' @import stats
NULL
