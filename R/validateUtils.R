# Input validation
# Validate the input parameters passed to stepwise()
# Stop and exit if any error detected in input parameters
# @author Kai Hu, Junhui Li
validateUtils <- function(formula,
                          data,
                          type = c("linear", "logit", "poisson", "cox", "Gamma"),
                          include = NULL,
                          strategy = c("forward", "backward", "bidirectional", "subset"),
                          metric = c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                          sle = 0.15,
                          sls = 0.15,
                          sigma_value,
                          test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                          test_method_glm = c("Rao", "LRT"),
                          test_method_cox = c("efron", "breslow", "exact"),
													tolerance = 10e-7,
													weight = NULL,
                          best_n = Inf,
													excel_name = NULL,
													n_y) {
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
	linear_metric <- c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)")
	glm_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	#poisson_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	#Gamma_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	cox_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
	
	if(type == "linear") {
		if(any(!metric %in% linear_metric)) {
			stop("for type 'linear': 'metric' must be from one of the c('", paste0(linear_metric, collapse = "','"),"').")
		}
	  if(any(metric %in% c("BIC","CP"))){
	    x_name_orig <- getXname(formula, data)
	    y_name <- getYname(formula, data)
	    intercept <- getIntercept(formula, data, type = type) # char type
	    merged_include <- getMergedVar(include)
	    model_raw <- getModel(data, type = type, intercept = intercept, x_name_orig, y_name, weight = weight, method = test_method_cox)
	    if(model_raw$rank >= nrow(data)) {
	      stop("The 'metric' can not be 'CP' or 'BIC' when variable number is greater than the number of observation.")
	    }
	  }
	  if(n_y > 1) {
	    if(any(metric %in% c("BIC", "CP", "Rsq", "adjRsq"))) {
	      stop("The 'metric' can not be 'BIC', 'CP', 'Rsq' or 'adjRsq' when using multivariate multiple regression!")
	    }
	  }
		if(strategy == "subset" & any(metric == "SL")) {
			stop("metric = 'SL' is not allowed when strategy = 'subset'")
		}
	  if(any(metric == "CP") & sigma_value == 0) {
	    stop("metric = 'CP' is not allowed when Estimate of pure error variance from fitting the full model(sigma_value) is 0")
	  }
	}else if(type == "logit" | type == "poisson" | type == "Gamma") {
		if(any(!metric %in% glm_metric)) {
			stop("for type ",type,": 'metric' must be from one of the c('", paste0(glm_metric, collapse = "','"),"').")
		}
	  if(type == 'logit') {
	    type_glm <- "binomial"
	  }else {
	    type_glm <- type
	  }
	  # check if Y separates X completely, if so, stop
	  # ref: https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logistic-regression-and-what-are-some-strategies-to-deal-with-the-issue/
	  tryCatch(                
	    expr = {                      
	      glm(formula, data = data, family = type)
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
		if(any(!metric %in% cox_metric)) {
			stop("for type 'cox': 'metric' must be from one of the c('", paste0(cox_metric, collapse = "','"),"').")
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
