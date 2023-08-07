#' Stepwise Linear Model Regression
#'
#' Stepwise linear regression analysis selects model based on information criteria and F or approximate F test with 'forward', 'backward', 'bidirection' and 'subset' model strategy method.
#'
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- cbind(mpg,drat) ~ . + 0
#' stepwiseLinear(formula=formula,
#'          data=mtcars,
#'          strategy="bidirection",
#'          metric="AIC")

stepwiseLinear <- function(
                     formula,
                     data,
                     include = NULL,
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SL","SBC"),
                     sle = 0.15,
                     sls = 0.15,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     weights = NULL,
                     best_n = Inf){
  
	x_name_orig <- getXname(formula, data)
	y_name <- getYname(formula, data)
	y_df <- as.matrix(model_raw$model[,y_name])
	n_y <- ncol(y_df)
	intercept <- getIntercept(formula, data, type = "linear") # char type
	merged_include <- getMergedInclude(include)
	model_raw <- getModel(data, type = "linear", x_name_orig, y_name, weights, intercept)
	multico_x <- getMulticolX(data, x_name_orig, tolerance)
	merged_multico_x <- paste0(multico_x, sep = " ")
	x_name <- setdiff(x_name_orig, multico_x)

	test_method_linear <- getTestMethod(data, model_raw, type="linear", metric, y_name, test_method_linear, test_method_logit, test_method_cox)
  
  result <- list()
  ## table1
  table1_para_value <- getTable1SummaryOfParameters(data, x_name_orig, y_name, merged_multico_x, merged_include, strategy, metric, sle, sls, test_method, tolerance, intercept)
  result$'Summary of Parameters' <- table1_para_value
  
  ## table2
  table2_class_table <- getTable2TypeOfVariables(model_raw)
  result$'Variables and Type' <- table2_class_table

  if(strategy=="subset"){
    table3_process_table <- getSubsetWrapper(data, type, metric, x_name, y_name, intercept, include, weights, best_n, test_method_cox)
  	result$'Process of Selection' <- table3_process_table
  	## obtain x_name_selected (drop-in replacement for x_model/xModel)
  	x_final_model <- getXNameSelected(table3_process_table)
  }else{
    ## get intial stepwise model
    out_init_stepwise <- getInitialStepwise(data,type="linear",strategy,metric,weights,x_name,y_name,intercept,include,method=test_method_cox)
    add_or_remove <- out_init_stepwise$add_or_remove
    x_in_model <- out_init_stepwise$x_in_model
    x_notin_model <- out_init_stepwise$x_notin_model
    process_table <- out_init_stepwise$process_table
    
    ## get final stepwise model
    out_final_stepwise <- getFinalStepModel(add_or_remove,data,type="linear",metric,weights,y_name,x_in_model,x_notin_model,intercept, include, process_table, test_method_cox,test_method_linear,test_method_logit)
    x_in_model <- out_final_stepwise$x_in_model
    process_table <- out_final_stepwise$process_table
    
    ## get table3-table5
    #table3_process_table <- formatTable(process_table, tbl_name = "Table 3. Process of Selection")
    table3_process_table <- process_table
    x_final_model <- c(intercept,include,x_in_model)
  }
  result$"Process of Selection" <- table3_process_table
  table4_x_in_model <- getTBLFianlVariable(x_final_model)
  result$"Selected Varaibles" <- table4_x_in_model
  table5_coef_model <- getTBLCoefModel(type="linear",intercept,include,x_final_model,y_name,n_y,data,test_method_cox)
  result$"Summary of Selected Variables" <- table5_coef_model
  class(result) <- c("StepReg","list")
  return(result)
}
