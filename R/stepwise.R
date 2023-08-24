#' StepReg: Stepwise Regression Made Simple
#' 
#' Stepwise regression is a common technique used for automatically selecting predictor variable(s) (independent) to determine an optimal model for the prediction of the response variable(s) (dependent). The StepReg package supports three types of models: linear regression, logistic regression, and Cox regression. Meanwhile, multiple model fit scoring methods are implemented.
#' 
#' @param formula (formula) The formula used for model fitting. The formula takes the form of a '~' (tilde) symbol, with the response variable(s) on the left-hand side, and the predictor variable(s) on the right-hand side. The 'lm()' function uses this formula to fit a regression model. A formula can be as simple as 'y ~ x'. For multiple predictors, they must be separated by the '+' (plus) symbol, e.g. 'y ~ x1 + x2'. To include an interaction term between variables, use the ':' (colon) symbol: 'y ~ x1 + x1:x2'. Use the '.' (dot) symbol to indicate that all other variables in the dataset should be included as predictors, e.g. 'y ~ .'. In the case of multiple response variables (multivariate), the formula can be specified as 'cbind(y1, y2) ~ x1 + x2'. By default, an intercept term is always included in the models, to exclude it, include '0' or '- 1' in your formula: 'y ~ 0 + x1', 'y ~ x1 + 0', and 'y ~ x1 - 1'.
#' 
#' @param data (data.frame) A dataset consisting of predictor variable(s) and response variable(s).
#' 
#' @param type (character) The stepwise regression type. Choose from 'linear', 'logit', and 'cox'. Default is 'linear'.
#' 
#' @param include (NULL|character) A character vector specifying predictor variables that will always stay in the model. A subset of the predictors in the dataset.
#' 
#' @param strategy (character) The model selection strategy. Choose from 'forward', 'backward', 'bidirectional' and 'subset'. Default is 'forward'. More information, see [StepReg](https://github.com/JunhuiLi1017/StepReg#stepwise-regression)
#' 
#' @param metric (character) The model selection criterion (model fit score). Used for the evaluation of the predictive performance of an intermediate model. Choose from 'AIC', 'AICc', 'BIC', 'CP', 'HQ', 'HQc', 'Rsq', 'adjRsq', 'SL', 'SBC', 'IC(3/2)', 'IC(1)'. Default is 'AIC'.
#' 
#' @param sle (numeric) Significance Level to Enter. It is the statistical significance level that a predictor variable must meet to be included in the model. E.g. if 'sle = 0.05', a predictor with a P-value less than 0.05 will 'enter' the model. Default is 0.15.
#' 
#' @param sls (numeric) Significance Level to Stay. Similar to 'sle', 'sls' is the statistical significance level that a predictor variable must meet to 'stay' in the model. E.g. if 'sls = 0.1', a predictor that was previously included in the model but whose P-value is now greater than 0.1 will be removed.
#' 
#' @param tolerance (numeric)  A statistical measure used to assess multicollinearity in a multiple regression model. It is calculated as the proportion of the variance in a predictor variable that is not accounted for by the other predictor variables in the model. Default is 1e-07.
#' 
#' @param weight (numeric) A numeric vector specifying the coefficients assigned to the predictor variables. The magnitude of the weight reflects the degree to which each predictor variable contributes to the prediction of the response variable. The range of weight should be from 0 to 1. Values greater than 1 will be coerced to 1, and values less than 0 will be coerced to 0. Default is NULL, which means that all weight are set equal.
#' 
#' @param test_method_linear (character) Test method for multivariate linear regression analysis, choose from 'Pillai', 'Wilks', 'Hotelling-Lawley', 'Roy'. Default is 'Pillai'. For univariate regression, 'F-test' will be used. 
#' 
#' @param test_method_logit (character) Test method for univariate logit regression analysis, choose from 'Rao', 'LRT'. Default is 'Rao'.
#' 
#' @param test_method_cox (character) Test method for cox regression analysis, choose from 'efron', 'breslow', 'exact'. Default is 'efron'.
#' 
#' @param best_n (numeric(integer)) The number of models to keep in the final output. Default is Inf, which means that all models will be displayed.
#' 
#' @param excel_name (NULL|character) The output excel name. If NULL, do not output excel file. Default is NULL.
#' 
#' @references
#' 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable strategy in multivariable regression using sas/iml. , 07(i12).
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' Dharmawansa, P. , Nadler, B. , & Shwartz, O. . (2014). Roy's largest root under rank-one alternatives:the complex valued case and applications. Statistics.
#' Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.
#' Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.
#' Hocking, R. R. (1976). A biometrics invited paper. the analysis and strategy of variables in linear regression. Biometrics, 32(1), 1-49.
#' Hurvich, C. M., & Tsai, C. (1989). Regression and time series model strategy in small samples. Biometrika, 76(2), 297-307.
#' Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.
#' Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.
#' Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.
#' Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.
#' Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model strategy. Regression and time series model strategy /. World Scientific.
#' Pillai, K. . (1955). Some new test criteria in multivariate analysis. The Annals of Mathematical Statistics, 26(1), 117-121.
#' R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable strategy in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.
#' Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.
#' Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
#' 
#' @author Junhui Li 
#' 
#' @examples
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- cbind(mpg,drat) ~ . + 0
#' stepwise(type = "linear",
#'          formula=formula,
#'          data=mtcars,
#'          strategy="bidirection",
#'          metric="AIC")
#'          
#' @keywords stepwise regression
#' @import survival
#' @importFrom utils combn
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms
#' 
#' @export
#' 
#' 
#' 
type = "linear"
formula=formula1
data=mtcars
strategy=strategy
metric=metric
stepwise1 <- function(formula,
                     data,
                     type = c("linear", "logit", "cox"),
                     include = NULL,
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                     sle = 0.15,
                     sls = 0.15,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     test_method_logit = c("Rao", "LRT"),
                     test_method_cox = c("efron", "breslow", "exact"),
                     tolerance = 1e-7,
                     weight = NULL,
                     best_n = Inf,
                     excel_name = NULL){
  ## validate input:
  ## check required parameters
  ## place match.arg() in the main function because validationUtils.R can't return type even with <<-, and type represents all values in c().
  type <- match.arg(type)
  strategy <- match.arg(strategy)
  metric <- match.arg(metric)
  
  test_method_linear <- match.arg(test_method_linear)
  test_method_logit <- match.arg(test_method_logit)
  test_method_cox <- match.arg(test_method_cox)
  
  validateUtils(formula = formula, data = data, type = type, include = include, strategy = strategy, metric = metric, sle = sle, sls = sls, test_method_linear = test_method_linear, test_method_logit = test_method_logit, test_method_cox = test_method_cox, tolerance = tolerance, weight = weight, best_n = best_n, excel_name = excel_name)
  
  x_name_orig <- getXname(formula, data)
  y_name <- getYname(formula, data)
  intercept <- getIntercept(formula, data, type = type) # char type
  merged_include <- getMergedInclude(include)
  model_raw <- getModel(data, type = type, intercept=intercept, x_name_orig, y_name, weight=weight, method=test_method_cox)
  if(type != "cox"){
    y_df <- as.matrix(model_raw$model[,y_name])
    n_y <- ncol(y_df)
  }else{
    n_y <- 1
  }
  
  test_method <- getTestMethod(data, model_raw, type, metric, n_y, test_method_linear, test_method_logit, test_method_cox)
  
  multico_x <- getMulticolX(data, x_name_orig, tolerance)
  merged_multico_x <- paste0(multico_x, sep = " ")
  x_name <- setdiff(x_name_orig, multico_x)
    
  result <- list()
  ## table1
  table1_para_value <- getTable1SummaryOfParameters(data, type, x_name_orig, y_name, merged_multico_x, merged_include, strategy, metric, sle, sls, test_method, tolerance, intercept)
  result$'Summary of Parameters' <- table1_para_value
  
  ## table2
  table2_class_table <- getTable2TypeOfVariables(model_raw)
  result$'Variables and Type' <- table2_class_table
  
  ## table3
  if(strategy == "subset"){
    table3_process_table <- getSubsetWrapper(data, type, metric, x_name, y_name, intercept, include, weight=weight, best_n, test_method)
    x_final_model <- getXNameSelected(table3_process_table,metric)
  }else{
    out_final_stepwise <- getStepwiseWrapper(data,type=type,strategy,metric,weight=weight,x_name,y_name,intercept,include,test_method)
    table3_process_table <- out_final_stepwise$process_table
    x_in_model <- out_final_stepwise$x_in_model
    x_final_model <- c(include,x_in_model)
  }
  result$"Process of Selection" <- table3_process_table
  
  ##table4
  table4_x_in_model <- getTable4FinalVariable(x_final_model)
  result$"Selected Varaibles" <- table4_x_in_model
  
  ##table5
  table5_coef_model <- getTable5CoefModel(type=type,intercept,include,x_final_model,y_name,n_y,data,weight,test_method_cox)
  if(length(table5_coef_model) > 1){
    for(i in names(table5_coef_model)){
      result[[paste0("Summary Model for ",i)]] <- table5_coef_model[[i]]
    }
  }else{
    result$"Summary Model" <- table5_coef_model
  }

  if(!is.null(excel_name)){
    openxlsx::write.xlsx(x=result, file=paste0(excel_name,".xlsx"))
  }
  class(result) <- c("StepReg","list")
  return(result)
}
