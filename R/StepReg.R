#' An R package for stepwise regression analysis
#'
#' The package provides functions to perform Stepwise Regression using various algorithms, e.g., Forward Selection, Backward Elimination, and Bidirectional Elimination. Meanwhile, it also supports the Best Subset method. Multiple selection criteria (stopping criteria) are supported, including AIC, AICc, BIC, HQ, and more.
#'
#' \tabular{ll}{ Package: \tab StepReg\cr Type: \tab Package\cr Version:
#' \tab 1.4.4\cr GitHub: \tab https://github.com/JunhuiLi1017/StepReg\cr Date: \tab 2023-01-23\cr }
#'
#' @name StepReg-package
#' @aliases StepReg-package StepReg
#' @docType package
#' @author Junhui Li, Xiaohuan Lu, Kun Cheng, Wenxin Liu
#'
#' Maintainers:\cr
#' Junhui Li <Junhui.li11@@umassmed.edu>\cr
#' Kai Hu <kai.hu@umassmed.edu>
#'
#' @references 1. [to be added]
#' @keywords package
#'
#' @examples
#' # exaple 1: stepwise logistic regression
#' if(interactive()) {
#'   data(mtcars)
#'   formula <- vs ~ .
#'   stepwiseLogit(formula,
#'                 data      = mtcars,
#'                 selection = "bidirection",
#'                 select    = "SL",
#'                 sle       = 0.15,
#'                 sls       = 0.15,
#'                 sigMethod = "Rao")
#' }
#'
"_PACKAGE"
