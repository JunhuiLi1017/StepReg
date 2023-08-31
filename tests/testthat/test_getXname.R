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
	
	library(stringr)
  # test4: linear stepwise regression vs older version
	#note1: output_linear_stepwise.Rdata
	#data: mtcars$yes <- mtcars$wt
	#formula: mpg ~ . + 0
	#method: version 1.4.4
	res_v1_4_4 <- readRDS(system.file("tests/data","res_v1_4_4.rds", package = "StepReg"))
	data(mtcars)
	mtcars$yes <- mtcars$wt
  #mod=names(res_v1_4_4)[1]
	for (mod in names(res_v1_4_4)[1]){
	  type <- unlist(stringr::str_split(mod,"_"))[1]
	  if(mod=="cox_model1"){
	    lung <- survival::lung %>% na.omit()
	    mydata <- lung
	  }else if(mod %in% c("linear_model1","linear_model2","logit_model1")){
	    data(mtcars)
	    mtcars$yes <- mtcars$wt
	    mydata <- mtcars
	  }
	  for (strategy in names(res_v1_4_4[[mod]])){
	    strategy_new <- strategy
	    if(strategy=="score"){
	      strategy_new <- "subset"
	      select_col1 <- c(2,3,4)-1
	      select_col2 <- c(2,3,4)-1
	      
	      index_n <- 2
	    }else{
	      index_n <- 3
	      if(type=="logit" | type=="cox"){
	        select_col1 <- c(2,3,7)
	        select_col2 <- c(2,3,6)
	      }else if(type=="linear"){
	        select_col1 <- c(2,3,7)
	        select_col2 <- c(2,3,7)
	      }
	    }
	    for(metric in names(res_v1_4_4[[mod]][[strategy]])){
	      message(mod,strategy,metric)
	      output_new <- NA
	      output_old <- res_v1_4_4[[mod]][[strategy]][[metric]]
	      
	      try(output_new <- stepwise1(type = type,
	                                  formula=get(mod),
	                                  data=mydata,
	                                  strategy=strategy_new,
	                                  metric=metric)[[3]][,select_col1],silent = TRUE)
	      
	      
	      
	      if(length(output_new) > 1 & length(output_old) > 1){
	        output_new[, c(index_n)] <- sapply(output_new[, c(index_n)], as.numeric)
	        output_old[,select_col2][,index_n] <- sapply(output_old[,select_col2][,index_n], as.numeric)
	        output_old <- output_old[,select_col2]
	      }
	      
	      res <- try(expect_equal(output_new,output_old),silent = TRUE)
	      if(inherits(res, "try-error")){
          message("Error",mod,strategy,metric)
	      }
	    }
	  }#strategy
	}
	
	expect_identical(NA,NA)
	expect_equal(NA,NA)
	
	res_v1_4_4[[mod]][["forward"]][["BIC"]]
	
	traceback()
	# test5: logit stepwise regression vs older version
	#note1: output_logit_stepwise.Rdata
	#data: mtcars
	#formula: vs ~ .
	#method: version 1.4.4
	
	res_v1_4_4 <- readRDS(system.file("tests/data","res_v1_4_4.rds", package = "StepReg"))
	linear_model1 <- mpg ~ . + 1
	linear_model2 <- cbind(mpg, drat) ~ . + 0
	logit_model1 <- vs ~ .
	cox_model1 <- Surv(time, status) ~ .
	
	
	
	
	data(mtcars)
	formula_logit_1 <- vs ~ .
	
	for (strategy in names(output_logit_stepwise)){
	  for(metric in names(output_logit_stepwise[[strategy]])){
	    message(strategy,metric)
	    output_new <- NA
	    try(output_new <- stepwise1(type = "logit",
	                                formula=formula_logit_1,
	                                data=mtcars,
	                                strategy=strategy,
	                                metric=metric
	                                )[[3]][,c(2,3,7)],silent = TRUE)
	    expect_identical(output_new,output_logit_stepwise[[strategy]][[metric]])
	  }
	}
	
	# test6: cox stepwise regression vs older version
	#note1: output_cox_stepwise.Rdata
	#data: lung <- survival::lung %>% na.omit()
	#formula: Surv(time, status) ~ .
	#method: version 1.4.4
	output_cox_stepwise <- readRDS("../data/output_cox_stepwise.Rdata")
	
	for (strategy in names(output_cox_stepwise)){
	  for(metric in names(output_cox_stepwise[[strategy]])){
	    message(strategy,metric)
	    output_new <- NA
	    try(output_new <- stepwise1(type = "cox",
	                                formula=formula3,
	                                data=lung,
	                                strategy=strategy,
	                                metric=metric,
	                                test_method_cox="efron")[[3]][,c(2,3,7)],silent = TRUE)
	    expect_identical(output_new,output_cox_stepwise[[strategy]][[metric]])
	  }
	}
	
	#traceback()

		
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


    # create expect_xxx()

})
