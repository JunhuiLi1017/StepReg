#' StepReg: Stepwise Regression Made Simple
#' 
#' Stepwise regression is a common technique used for automatically selecting predictor variable(s) (independent) to determine an optimal model for the prediction of the response variable(s) (dependent). The StepReg package supports three types of models: linear regression, logistic regression, and Cox regression. Meanwhile, multiple model fit scoring methods are implemented.
#' 
#' @param type (character) The stepwise regression type. Choose from 'linear', 'logit', and 'cox'. Default is 'linear'.
#' 
#' @param formula (formula) The formula used for model fitting. The formula takes the form of a '~' (tilde) symbol, with the response variable(s) on the left-hand side, and the predictor variable(s) on the right-hand side. The 'lm()' function uses this formula to fit a regression model. A formula can be as simple as 'y ~ x'. For multiple predictors, they must be separated by the '+' (plus) symbol, e.g. 'y ~ x1 + x2'. To include an interaction term between variables, use the ':' (colon) symbol: 'y ~ x1 + x1:x2'. Use the '.' (dot) symbol to indicate that all other variables in the dataset should be included as predictors, e.g. 'y ~ .'. In the case of multiple response variables (multivariate), the formula can be specified as 'cbind(y1, y2) ~ x1 + x2'. By default, an intercept term is always included in the models, to exclude it, include '0' or '- 1' in your formula: 'y ~ 0 + x1', 'y ~ x1 + 0', and 'y ~ x1 - 1'.
#' 
#' @param data (data.frame) A dataset consisting of predictor variable(s) and response variable(s).
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
#' @param weights (numeric) A numeric vector specifying the coefficients assigned to the predictor variables. The magnitude of the weights reflects the degree to which each predictor variable contributes to the prediction of the response variable. The range of weights should be from 0 to 1. Values greater than 1 will be coerced to 1, and values less than 0 will be coerced to 0. Default is NULL, which means that all weights are set equal.
#' 
#' @param test_method_linear (character) Test method for multivariate linear regression analysis, choose from 'Pillai', 'Wilks', 'Hotelling-Lawley', 'Roy'. Default is 'Pillai'. For univariate regression, 'F-test' will be used. 
#' 
#' @param test_method_logit (character) Test method for univariate logit regression analysis, choose from 'Rao', 'LRT'. Default is 'Rao'.
#' 
#' @param test_method_cox (character) Test method for cox regression analysis, choose from 'efron', 'breslow', 'exact'. Default is 'efron'.
#' 
#' @param best_n (numeric(integer)) The number of models to keep in the final output. Default is Inf, which means that all models will be displayed.
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
#' stepwise(formula=formula,
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
stepwise <- function(type = c("linear", "logit", "cox"),
                     formula,
                     data,
                     include = NULL,
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                     sle = 0.15,
                     sls = 0.15,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     test_method_logit = c("Rao", "LRT"),
                     test_method_cox = c("efron", "breslow", "exact"),
                     weights = NULL,
                     best_n = Inf){
	## validate input:
	## check required parameters
	type <- match.arg(type)
	strategy <- match.arg(strategy)
	metric <- match.arg(metric)
	test_method_linear <- match.arg(test_method_linear)
	test_method_logit <- match.arg(test_method_logit)
	test_method_cox <- match.arg(test_method_cox)
	## convert the following into a helper function:
	# x_name <- NULL
	# y_name <- NULL
	# intercept <- NULL
	# response_variable_n <- NULL
	# predictor_variable_n <- NULL 
	# include_name <- NULL
	# merge_inc_name <- NULL
	
	validateInputStepwise(type = type, formula = formula, data = data, include = include, strategy = strategy, metric = metric, sle = sle, sls = sls, test_method_linear = test_method_linear, test_method_logit = test_method_logit, test_method_cox = test_method_cox, weights = weights, best_n = best_n)
	
	## invoke corresponding function:
	shared_params <- list(formula = formula, data = data, include = include, strategy = strategy, metric = metric, sle = sle, sls = sls, weights = weights, best_n = best_n)
	if(type == "linear"){
		result <- do.call(stepwiseLinear, append(shared_params, list(test_method_linear = test_method_linear)))
	}else if(type == "logit"){
		result <- do.call(stepwiseLogit, append(shared_params, list(test_method_logit = test_method_logit)))
	}else if(type == "cox"){
	  result <- do.call(stepwiseCox, append(shared_params, list(test_method_cox = test_method_cox)))
	}
}

