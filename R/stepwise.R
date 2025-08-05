#' Stepwise Regression Model Selection
#'
#' Performs stepwise regression model selection using various strategies and selection criteria.
#' Supports multiple regression types including linear, logistic, Cox, Poisson, and Gamma regression.
#'
#' @param formula A formula object specifying the model structure:
#'   \itemize{
#'     \item Response variable(s) on left side of ~
#'     \item Predictor variable(s) on right side of ~
#'     \item Use + to separate multiple predictors
#'     \item Use * for main effect and interaction terms
#'     \item Use : for continuous-nested-within-class variable, make sure class variable is a factor variable, e.g. X:A or A:X means a continuous variable X nested within a factor variable A
#'     \item Use . to include all variables
#'     \item Use cbind() for multiple responses
#'     \item Use 0 or -1 to exclude intercept
#'     \item Use strata() to include strata variable for Cox regression
#'   }
#'
#' @param data A data frame containing the variables in the model
#'
#' @param type The type of regression model to fit:
#'   \itemize{
#'     \item "linear" - Linear regression (default)
#'     \item "logit" - Logistic regression
#'     \item "poisson" - Poisson regression
#'     \item "cox" - Cox proportional hazards regression
#'     \item "gamma" - Gamma regression
#'     \item "negbin" - Negative binomial regression
#'   }
#'
#' @param strategy The model selection strategy:
#'   \itemize{
#'     \item "forward" - Forward selection (default)
#'     \item "backward" - Backward elimination
#'     \item "bidirection" - Bidirectional elimination
#'     \item "subset" - Best subset selection
#'   }
#'
#' @param metric The model selection criterion:
#'   \itemize{
#'     \item "AIC" - Akaike Information Criterion (default)
#'     \item "AICc" - Corrected AIC
#'     \item "BIC" - Bayesian Information Criterion
#'     \item "CP" - Mallows' Cp
#'     \item "HQ" - Hannan-Quinn criterion
#'     \item "adjRsq" - Adjusted R-squared
#'     \item "SL" - Significance Level
#'     \item "SBC" - Schwarz Bayesian Criterion
#'     \item "IC(3/2)" - Information Criterion with penalty 3/2
#'     \item "IC(1)" - Information Criterion with penalty 1
#'   }
#'
#' @param sle Significance Level to Enter (default: 0.15). A predictor must have p-value < sle to enter the model.
#'
#' @param sls Significance Level to Stay (default: 0.15). A predictor must have p-value < sls to remain in the model.
#'
#' @param include Character vector of predictor variables that must be included in all models.
#'
#' @param tolerance Threshold for detecting multicollinearity (default: 1e-07). Lower values are more strict.
#'
#' @param weight Optional numeric vector of observation weights. Values are coerced to [0,1].
#'
#' @param test_method_linear Test method for multivariate linear regression:
#'   \itemize{
#'     \item "Pillai" (default)
#'     \item "Wilks"
#'     \item "Hotelling-Lawley"
#'     \item "Roy"
#'   }
#'   For univariate regression, F-test is used.
#'
#' @param test_method_glm Test method for GLM models:
#'   \itemize{
#'     \item "Rao" (default)
#'     \item "LRT"
#'   }
#'   Only "Rao" available for subset strategy.
#'
#' @param test_method_cox Test method for Cox regression:
#'   \itemize{
#'     \item "efron" (default)
#'     \item "breslow"
#'     \item "exact"
#'   }
#'
#' @param best_n Maximum number of models to retain for each variable count (default: 3)
#'
#' @param num_digits Number of decimal places to round results (default: 6)
#'
#' @return A list containing:
#'   \itemize{
#'     \item Selected models for each strategy-metric combination
#'     \item Model selection process details
#'     \item Variable importance information
#'     \item Model fit statistics
#'   }
#'
#' @examples
#' # Multivariate linear regression with bidirectional selection
#' data(mtcars)
#' formula <- cbind(mpg, drat) ~ . + 0
#' stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = "bidirection",
#'   metric = "AIC"
#' )
#'
#' # Linear regression with multiple strategies and metrics
#' formula <- mpg ~ . + 1
#' stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = c("forward", "bidirection"),
#'   metric = c("AIC", "SBC", "SL", "AICc", "BIC", "HQ")
#' )
#'
#' # Logistic regression with significance level criteria
#' data(remission)
#' formula <- remiss ~ .
#' stepwise(
#'   formula = formula,
#'   data = remission,
#'   type = "logit",
#'   strategy = "forward",
#'   metric = "SL",
#'   sle = 0.05,
#'   sls = 0.05
#' )
#'
#' # Linear regression with continuous-nested-within-class effects
#' mtcars$am <- factor(mtcars$am)
#' formula <- mpg ~ am + cyl + wt:am + disp:am + hp:am
#' stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = "bidirection",
#'   metric = "AIC"
#' )
#' 
#' @references
#' \itemize{
#'   \item Alsubaihi et al. (2002) Variable strategy in multivariable regression using sas/iml
#'   \item Darlington (1968) Multiple regression in psychological research and practice
#'   \item Dharmawansa et al. (2014) Roy's largest root under rank-one alternatives
#'   \item Hannan & Quinn (1979) The determination of the order of an autoregression
#'   \item Hotelling (1992) The Generalization of Student's Ratio
#'   \item Hocking (1976) The analysis and strategy of variables in linear regression
#'   \item Hurvich & Tsai (1989) Regression and time series model strategy in small samples
#'   \item Judge (1985) The Theory and practice of econometrics
#'   \item Mallows (1973) Some comments on cp
#'   \item Mardia et al. (1979) Multivariate analysis
#'   \item Mckeon (1974) F approximations to the distribution of hotelling's t20
#'   \item Mcquarrie & Tsai (1998) Regression and Time Series Model strategy
#'   \item Pillai (1955) Some new test criteria in multivariate analysis
#'   \item Sparks et al. (1985) On variable strategy in multivariate regression
#'   \item Sawa (1978) Information criteria for discriminating among alternative regression models
#'   \item Schwarz (1978) Estimating the dimension of a model
#' }
#'
#' @author Junhui Li, Kai Hu, Xiaohuan Lu
#'
#' @keywords stepwise regression
#'
#' @importFrom survival coxph
#' @importFrom stringr str_replace
#' @importFrom utils combn
#' @importFrom dplyr %>% mutate_if mutate
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms deviance df.residual formula model.frame
#' @importFrom MASS glm.nb
#'
#' @export

stepwise <- function(formula,
                     data,
                     type = c("linear", "logit", "cox", "poisson", "gamma", "negbin"),
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                     sle = 0.15,
                     sls = 0.15,
                     include = NULL,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     test_method_glm = c("Rao", "LRT"),
                     test_method_cox = c("efron", "breslow", "exact"),
                     tolerance = 1e-7,
                     weight = NULL,
                     best_n = 3,
                     num_digits = 6) {

  type <- match.arg(type)
  strategy <- match_multiple_args(strategy, c("forward", "backward", "bidirection", "subset"))
  metric <- match_multiple_args(metric, c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"))
  
  test_method_linear <- match.arg(test_method_linear)
  test_method_glm <- match.arg(test_method_glm)
  test_method_cox <- match.arg(test_method_cox)
  
  x_name_orig <- getXname(formula, data)
  y_name <- getYname(formula, data)
  intercept <- getIntercept(formula, data, type = type) # char type
  merged_include <- getMergedVar(include)
  model_raw <- getModel(data, type = type, intercept = intercept, x_name_orig, y_name, weight = weight, method = test_method_cox)
  if(type != "cox") {
    y_df <- as.matrix(model_raw$model[, y_name])
    n_y <- ncol(y_df)
  }else{
    n_y <- 1
  }
  sigma_value <- getSigmaFullModel(model_raw, type, n_y)
  validateUtils(formula = formula, data = data, type = type, include = include, strategy = strategy, metric = metric, sle = sle, sls = sls, sigma_value = sigma_value, test_method_linear = test_method_linear, test_method_glm = test_method_glm, test_method_cox = test_method_cox, tolerance = tolerance, weight = weight, best_n = best_n, n_y = n_y)
  test_method <- getTestMethod(data, model_raw, type, metric, n_y, test_method_linear, test_method_glm, test_method_cox)
  
  multico_x <- getMulticolX(data, x_name_orig, tolerance)
  merged_multico_x <- getMergedVar(multico_x)
  x_name <- setdiff(x_name_orig, multico_x)
  
  result <- list()
  ## table1
  table1_para_value <- getTable1SummaryOfParameters(formula, data, type, x_name_orig, y_name, merged_multico_x, merged_include, strategy, metric, sle, sls, test_method, tolerance, intercept)
  result$arguments <- table1_para_value
  
  ## table2
  table2_class_table <- getTable2TypeOfVariables(model_raw)
  result$variables <- table2_class_table
  
  ## table3
  table3_process <- getTable3ProcessSummary(data, type, strategy, metric, sle, sls, weight, x_name, y_name, intercept, include, best_n, test_method, sigma_value, num_digits)
  x_final_model_metric <- table3_process$final_variable
  result <- append(result,table3_process[which(names(table3_process) != "final_variable")])
  
  
  table4_model <- getTable4ModelCall(type, intercept, include, x_final_model_metric, y_name, n_y, data, weight, test_method, num_digits)
  result <- append(result,table4_model)
  
  class(result) <- c("StepReg","list")
  attr(result, "nonhidden") <- strategy
  return(result)
}
