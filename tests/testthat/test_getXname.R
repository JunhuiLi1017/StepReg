test_that("test_utils.R failed", {
	# prepare test data
	data(mtcars)
	formula1 <- mpg ~ . + 1
	formula2 <- cbind(mpg, drat) ~ . + 0
	## for cox tests
	lung <- survival::lung %>% na.omit()
	# lung$status1 <- ifelse(lung$status == 2, 1, 0)
	formula3 = Surv(time, status) ~ .
	
	# test1: getXname() and getYname()
	expect_identical(setdiff(colnames(mtcars), "mpg"), getXname(formula1, mtcars))
	expect_identical(getYname(formula1, mtcars), "mpg")
	
	## multi-response variables:
	expect_identical(setdiff(colnames(mtcars), c("mpg", "drat")), getXname(formula2, mtcars))
	expect_identical(getYname(formula2, mtcars), "cbind(mpg, drat)")
	
	# test2: getIntercept()
	expect_identical(getIntercept(formula1, data = mtcars, type = "linear"), "1")
	expect_identical(getIntercept(formula2, data = mtcars, type = "linear"), "0")
	expect_identical(getIntercept(formula1, data = mtcars, type = "logit"), "1")
	expect_identical(getIntercept(formula2, data = mtcars, type = "logit"), "0")
	expect_identical(getIntercept(formula3, data = lung, type = "cox"), NULL)

	# test3: getInitialModel()
	
	
	getIntercept <- function(formula, data, type){
		# for 'cox', we need to set intercept to NULL
		term_form <- terms(formula, data = data)
		if(type != 'cox'){
			if(attr(term_form, "intercept") == 0){
				intercept <- "0"
			}else{
				intercept <- "1"
			}
		}else{
			intercept <- NULL
		}
		return(intercept)
	}
	
	
		
    

		
    # mtcars$yes <- mtcars$wt
    # formula <- mpg ~ . + 1
    # include <- c("cyl","gear")
    # strategy <- c("forward")
    # metric <- c("Rsq")
    # sle <- 0.15
    # sls <- 0.15
    # multivarStat <- c("Pillai")
    # weights <- NULL
    # best <- NULL

    
    
    
    # save test results
    get_x_name <- getXname()

    Feature.distribution <- 
        assignChromosomeRegion(exons, nucleotideLevel=TRUE, TxDb=TxDb)
    expect_equal(as.integer(Feature.distribution$percentage["Exons"]), 100)

    # create expect_xxx()

})
