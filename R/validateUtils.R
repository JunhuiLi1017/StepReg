#' Input validation
#' 
#' Validate the input parameters passed to stepwise()
#'
#' @return Stop and exit if any error detected in input parameters
#' 
#' @author Junhui Li, Kai Hu
validateUtils <- function(type = c("linear", "logit", "cox"),
                                  formula,
                                  data,
                                  include = NULL,
                                  strategy = c("forward", "backward", "bidirectional", "subset"),
                                  metric = c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                                  sle = 0.15,
                                  sls = 0.15,
                                  test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                                  test_method_logit = c("Rao", "LRT"),
                                  test_method_cox = c("efron", "breslow", "exact"),
																	tolerance = 10e-7,
																	weights = NULL,
                                  best_n = Inf,
																	excel_name = NULL) {
	## check required parameters
	if(missing(data)){ 
		stop("'data' parameter is missing.") 
	}else{
		if(!inherits(data, "data.frame")){
			stop("'data' must be a data.frame class.")
		}
	}
	
	if(missing(formula)){ 
		stop("'formula' parameter is missing.") 
	}else{
		if(!inherits(formula, "formula")){
			stop("'formula' must be a formula class.")
		}
		# term_form <- terms(formula,data=data)
		# vars <- as.character(attr(term_form, "variables"))[-1]
		# y_name <<- vars[attr(term_form, "response")]
		# x_name <<- attr(term_form,"term.labels")
		# if(attr(term_form, "intercept") == 0){
		# 	intercept <<- "0"
		# }else{
		# 	intercept <<- "1"
		# }
	}
	# response_variable_n <<- length(all.vars(formula[[2]]))
	
	 # Ref: https://stackoverflow.com/questions/13217322/how-to-reliably-get-dependent-variable-name-from-formula-object
	
	# if(is.null(include)){
	# 	# include_name <<- NULL
	# 	# merge_inc_name <<- "NULL"
	# }else{
	if(!is.null(include)){
		term_form <- terms(formula, data = data)
		x_name <- attr(term_form, "term.labels")
		if(!all(include %in% x_name)){
			stop(paste0("'include' must be a subset of: c('",paste0(x_name,collapse = "','"),"')"))
		}
		# else{
		# 	include_name <<- include
		# 	merge_inc_name <<- paste0(include_name,collapse=" ")
		# }
	}
	
	if(is.numeric(sle) & is.numeric(sls)){
		if(sle <= 0 | sle > 1){
			stop("'sle' shoule be a value from 0 to 1.")
		}
		if(sls <= 0 | sls > 1){
			stop("'sls' shoule be a value from 0 to 1.")
		}
	}else{
		stop("'sle' and 'sls' should be numeric.")
	}
	
	## check 'metric' and 'test_method' according to 'type'
	linear_metric <- c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")
	logit_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	cox_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	
	if(type == "linear"){
		if(!metric %in% linear_metric){
			stop("for type 'linear': 'metric' must be from one of the c('", paste0(linear_metric, collapse = "','"),"').")
		}
		if(strategy=="subset" & metric=="SL"){
			stop("metric = 'SL' is not allowed when strategy = 'subset'")
		}
	}else if(type == "logit"){
		if(!metric %in% logit_metric){
			stop("for type 'logit': 'metric' must be from one of the c('", paste0(logit_metric, collapse = "','"),"').")
		}
		
	}else if(type == 'cox'){
		if(!metric %in% cox_metric){
			stop("for type 'cox': 'metric' must be from one of the c('", paste0(cox_metric, collapse = "','"),"').")
		}
	}
	
	## check 'tolerance'
	if(!is.numeric(tolerance)){
		stop("the 'tolerance' must be a numeric value.")
	}else if (tolerance > 1 | tolerance < 0){
		stop("the 'tolenrance' must be in range 0~1.")
	}
	
	## check 'weights'
	predictor_variable_n <- length(all.vars(formula[[3]]))
	if(!is.null(weights)){
		if(!is.numeric(weights)){
			stop("the 'weights' must be a numeric vector.")
		}else if(length(weights) != predictor_variable_n){
			stop("the length of the 'weights' vector must equal the number of predictor variables specified in the 'formula'.")
		}
	}
	
	## check 'best_n'
	if(!is.infinite(best_n)){
		if(!is.integer(best_n)){
			stop("the 'best_n' must be an integer.")
		}
	}
	
	## check 'excel_name'
	if(!is.null(excel_name)){
		if(!is.character(excel_name)){
			stop("the 'excel_name' must be a character string.")
		}
	}
}
