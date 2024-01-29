#' Input validation
#' 
#' Validate the input parameters passed to stepwise()
#' 
#' @param formula (formula) The formula used for model fitting. The formula takes the form of a '~' (tilde) symbol, with the response variable(s) on the left-hand side, and the predictor variable(s) on the right-hand side. The 'lm()' function uses this formula to fit a regression model. A formula can be as simple as 'y ~ x'. For multiple predictors, they must be separated by the '+' (plus) symbol, e.g. 'y ~ x1 + x2'. To include an interaction term between variables, use the ':' (colon) symbol: 'y ~ x1 + x1:x2'. Use the '.' (dot) symbol to indicate that all other variables in the dataset should be included as predictors, e.g. 'y ~ .'. In the case of multiple response variables (multivariate), the formula can be specified as 'cbind(y1, y2) ~ x1 + x2'. By default, an intercept term is always included in the models, to exclude it, include '0' or '- 1' in your formula: 'y ~ 0 + x1', 'y ~ x1 + 0', and 'y ~ x1 - 1'.
#' 
#' @param data (data.frame) A dataset consisting of predictor variable(s) and response variable(s).
#' 
#' @param type (character) The stepwise regression type. Choose from 'linear', 'logit', 'poisson', and 'cox'. Default is 'linear'.
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
#' @param sigma_value sigma value from the full model for the calculation of "BIC" or "CP".
#' 
#' @param tolerance (numeric)  A statistical measure used to assess multicollinearity in a multiple regression model. It is calculated as the proportion of the variance in a predictor variable that is not accounted for by the other predictor variables in the model. Default is 1e-07.
#' 
#' @param weight (numeric) A numeric vector specifying the coefficients assigned to the predictor variables. The magnitude of the weight reflects the degree to which each predictor variable contributes to the prediction of the response variable. The range of weight should be from 0 to 1. Values greater than 1 will be coerced to 1, and values less than 0 will be coerced to 0. Default is NULL, which means that all weight are set equal.
#' 
#' @param test_method_linear (character) Test method for multivariate linear regression analysis, choose from 'Pillai', 'Wilks', 'Hotelling-Lawley', 'Roy'. Default is 'Pillai'. For univariate regression, 'F-test' will be used. 
#' 
#' @param test_method_logit (character) Test method for univariate logit regression analysis, choose from 'Rao', 'LRT'. Default is 'Rao'. Only "Rao" is available for strategy = 'subset'.
#' 
#' @param test_method_poisson (character) Test method for univariate poisson regression analysis, choose from 'Rao', 'LRT'. Default is 'Rao'. Only "Rao" is available for strategy = 'subset'.
#' 
#' @param test_method_cox (character) Test method for cox regression analysis, choose from 'efron', 'breslow', 'exact'. Default is 'efron'.
#' 
#' @param best_n (numeric(integer)) The number of models to keep in the final output. Default is Inf, which means that all models will be displayed.
#' 
#' @param excel_name (NULL|character) The output excel name. If NULL, do not output excel file. Default is NULL.
#' 
#' @return Stop and exit if any error detected in input parameters
#' 
#' @author Kai Hu, Junhui Li
#' 
validateUtils <- function(formula,
                          data,
                          type = c("linear", "logit", "poisson","cox"),
                          include = NULL,
                          strategy = c("forward", "backward", "bidirectional", "subset"),
                          metric = c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                          sle = 0.15,
                          sls = 0.15,
                          sigma_value,
                          test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                          test_method_logit = c("Rao", "LRT"),
                          test_method_poisson = c("Rao", "LRT"),
                          test_method_cox = c("efron", "breslow", "exact"),
													tolerance = 10e-7,
													weight = NULL,
                          best_n = Inf,
													excel_name = NULL) {
	## check required parameters
	if(missing(data)) { 
		stop("'data' parameter is missing.") 
	}else{
		if(!inherits(data, "data.frame")) {
			stop("'data' must be a data.frame class.")
		}
	}
	
	if(missing(formula)) { 
		stop("'formula' parameter is missing.") 
	}else{
		if(!inherits(formula, "formula")) {
			stop("'formula' must be a formula class.")
		}
		# term_form <- terms(formula,data=data)
		# vars <- as.character(attr(term_form, "variables"))[-1]
		# y_name <<- vars[attr(term_form, "response")]
		# x_name <<- attr(term_form,"term.labels")
		# if(attr(term_form, "intercept") == 0) {
		# 	intercept <<- "0"
		# }else{
		# 	intercept <<- "1"
		# }
	}
	# response_variable_n <<- length(all.vars(formula[[2]]))
	
	 # Ref: https://stackoverflow.com/questions/13217322/how-to-reliably-get-dependent-variable-name-from-formula-object
	
	# if(is.null(include)) {
	# 	# include_name <<- NULL
	# 	# merge_inc_name <<- "NULL"
	# }else{
	if(!is.null(include)) {
		term_form <- terms(formula, data = data)
		x_name <- attr(term_form, "term.labels")
		if(!all(include %in% x_name)) {
			stop(paste0("'include' must be a subset of: c('",paste0(x_name,collapse = "','"),"')"))
		}
		# else{
		# 	include_name <<- include
		# 	merge_inc_name <<- paste0(include_name,collapse=" ")
		# }
	}
	
	if(is.numeric(sle) & is.numeric(sls)) {
		if(sle <= 0 | sle > 1) {
			stop("'sle' shoule be a value from 0 to 1.")
		}
		if(sls <= 0 | sls > 1) {
			stop("'sls' shoule be a value from 0 to 1.")
		}
	}else{
		stop("'sle' and 'sls' should be numeric.")
	}
	
	## check 'metric' and 'test_method' according to 'type'
	linear_metric <- c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")
	logit_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	poisson_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	cox_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	
	if(type == "linear") {
		if(!metric %in% linear_metric) {
			stop("for type 'linear': 'metric' must be from one of the c('", paste0(linear_metric, collapse = "','"),"').")
		}
		if(strategy == "subset" & metric == "SL") {
			stop("metric = 'SL' is not allowed when strategy = 'subset'")
		}
	  if(metric == "CP" & sigma_value == 0) {
	    stop("metric = 'CP' is not allowed when Estimate of pure error variance from fitting the full model(sigma_value) is 0")
	  }
	}else if(type == "logit") {
		if(!metric %in% logit_metric) {
			stop("for type 'logit': 'metric' must be from one of the c('", paste0(logit_metric, collapse = "','"),"').")
		}
	  
	  # check if Y separates X completely, if so, stop
	  # ref: https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logistic-regression-and-what-are-some-strategies-to-deal-with-the-issue/
	  tryCatch(                
	    expr = {                      
	      glm(formula, data = data, family = "binomial")
	    },
	    error = function(e) {          
	      print("There was an error message.")
	    },
	    warning = function(w) {  
	      if (w$message %in% c("glm.fit: fitted probabilities numerically 0 or 1 occurred", "glm.fit: algorithm did not converge")) {
	        stop("There is a perfect separation of data points, can not obtain a valid model fit.")
	      }
	    }
	  )
	}else if(type == 'cox') {
		if(!metric %in% cox_metric) {
			stop("for type 'cox': 'metric' must be from one of the c('", paste0(cox_metric, collapse = "','"),"').")
		}
	}else if(type == "poisson") {
	  if(!metric %in% poisson_metric) {
	    stop("for type 'poisson': 'metric' must be from one of the c('", paste0(poisson_metric, collapse = "','"),"').")
	  }
	}
	## check 'tolerance'
	if(!is.numeric(tolerance)) {
		stop("the 'tolerance' must be a numeric value.")
	}else if (tolerance > 1 | tolerance < 0) {
		stop("the 'tolenrance' must be in range 0~1.")
	}
	
	## check 'weights'
	if(!is.null(weight)) {
		if(!is.numeric(weight)) {
			stop("the 'weight' must be a numeric vector.")
		}else if(length(weight) != nrow(data)) {
			stop("the length of the 'weight' vector must equal the number of observation of dataset.")
		}
	}
	
	## check integer of 'best_n'
	if(!is.infinite(best_n)) {
		if(!best_n %% 1 == 0) {
			stop("the 'best_n' must be an integer.")
		}
	}
	
	## check 'excel_name'
	if(!is.null(excel_name)) {
		if(!is.character(excel_name)) {
			stop("the 'excel_name' must be a character string.")
		}
	}
}
