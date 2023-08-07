#' Stepwise Logistic Regression
#' 
#' Stepwise logistic regression analysis selects model based on information criteria and Wald or Score test with 'forward', 'backward', 'bidirection' and 'subset' model strategy method.

stepwiseLogit <- function(formula,
                          data,
                          include=NULL,
                          strategy=c("forward","backward","bidirection","subset"),
                          metric=c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)"),
                          sle=0.15,
                          sls=0.15,
                          test_method_logit=c("Rao","LRT"),
                          weights=NULL,
                          best_n=Inf){
  
  x_name_orig <- getXname(formula, data)
  y_name <- getYname(formula, data)
  intercept <- getIntercept(formula, data, type = "logit") # char type
  merged_include <- getMergedInclude(include)
  model_raw <- getModel(data, type = "logit", x_name_orig, y_name, weights, intercept)
  multico_x <- getMulticolX(data, x_name_orig, tolerance)
  merged_multico_x <- paste0(multico_x, sep = " ")
  x_name <- setdiff(x_name_orig, multico_x)
  
  result <- list()
  ## table1
  table1_para_value <- getTable1SummaryOfParameters(data, x_name_orig, y_name, merged_multico_x, merged_include, strategy, metric, sle, sls, test_method, tolerance, intercept)
  result$'Summary of Parameters' <- table1_para_value
  
  ## table2
  table2_class_table <- getTable2TypeOfVariables(model_raw)
  result$'Variables and Type' <- table2_class_table
  
  if (strategy=="subset"){ #subset
    table3_process_table <- getSubsetWrapper(data, type, metric, x_name, y_name, intercept, include, weights, best_n, test_method_cox)
    result$'Process of Selection' <- table3_process_table
    ## obtain x_name_selected (drop-in replacement for x_model/xModel)
    x_final_model <- getXNameSelected(table3_process_table)
  }else{ #forward # bidirection # backward
    ## get intial stepwise model
    out_init_stepwise <- getInitialStepwise(data,type="logit",strategy,metric,weights,x_name,y_name,intercept,include,method=test_method_cox)
    add_or_remove <- out_init_stepwise$add_or_remove
    x_in_model <- out_init_stepwise$x_in_model
    x_notin_model <- out_init_stepwise$x_notin_model
    process_table <- out_init_stepwise$process_table
    
    ## get final stepwise model
    out_final_stepwise <- getFinalStepModel(add_or_remove,data,type="logit",metric,weights,y_name,x_in_model,x_notin_model,intercept, include, process_table, test_method_cox,test_method_linear,test_method_logit)
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
  table5_coef_model <- getTBLCoefModel(type="logit",intercept,include,x_final_model,y_name,n_y,data,test_method_cox)
  result$"Summary of Selected Variables" <- table5_coef_model
  class(result) <- c("StepReg","list")
  return(result)
}
