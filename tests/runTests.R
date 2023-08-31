require("testthat") || stop("unable to load testthat")
require("survival") || stop("unable to load survival")
require("dplyr") || stop("unable to load dplyr")

test_check("StepReg")
