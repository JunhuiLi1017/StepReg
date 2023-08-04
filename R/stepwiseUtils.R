#' Stepwise helper functions
#' 
#' Extract information of parameters from stepwise regression.
#'
#' @return data.frame
#' 
#' @author Junhui Li, Kai Hu

# Paramters            Value
# 1          Response Variable cbind(mpg, drat)
# 2          Included Variable             NULL
# 3            Strategy Method      bidirection
# 4           Metric Criterion              AIC
# 5 Variable significance test           Pillai
# 6    Multicollinearity Terms              yes
# 7                  Intercept                0
# 
##----------------------test only
data(mtcars)
mtcars$yes <- mtcars$wt
formula <- cbind(mpg,drat) ~ . + 0
input_data <- mtcars
include <- NULL
type='linear'
weights=NULL
tolerance=1e-7

strategy="forward"
metric="AIC"
sle=0.15
sls=0.15
test_method_linear="F"
test_method_logit="Rao"
test_method_cox="efron"
##----------------------


##----------------------
library(survival)
type='cox'
my.data <- survival::lung
my.data <- na.omit(my.data)
my.data$status1 <- ifelse(my.data$status==2,1,0)
input_data <- my.data
formula = Surv(time, status1) ~ . - status 
include <- NULL
weights=NULL
tolerance=1e-7

strategy="forward"
metric="AIC"
sle=0.15
sls=0.15
test_method_linear="F"
test_method_logit="Rao"
test_method_cox="efron"
##----------------------


##----------------------
type='logit'
data(mtcars)
formula <- vs ~ .
input_data=mtcars
metric="SL"
sle=0.15
sls=0.15
include <- NULL
weights=NULL
tolerance=1e-7

strategy="forward"
sle=0.15
sls=0.15
test_method_linear="F"
test_method_logit="Rao"
test_method_cox="efron"

##----------------------

getXname <- function(formula, data){
	term_form <- terms(formula, data = data)
	vars <- as.character(attr(term_form, "variables"))[-1]
	x_name <- attr(term_form, "term.labels")
	return(x_name)
}

getYname <- function(formula, data){
	term_form <- terms(formula, data = data)
	vars <- as.character(attr(term_form, "variables"))[-1]
	y_name <- vars[attr(term_form, "response")]
	return(y_name)
}

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
	  intercept <- '0'
	}
	return(intercept)
}

getMergedInclude <- function(include){
	# obtain a single string concatenating all include variables by space
	if(is.character(include)){
		merge_include_name <- paste0(include_name,collapse=" ")
	}else if(is.null(include)){
		merge_include_name <- "NULL"
	}
	return(merge_include_name)
}

getModel <- function(data, type, x_name, y_name, weights, intercept, method=c("efron","breslow","exact")){
  ## "method" is only used for cox regression
  method <- match.arg(method)
	# create a new formula given explicit x, y, and intercept, bypassed the x being .
	formula_raw <- reformulate(c(intercept, x_name), y_name)
	
	if(type == 'linear'){
		## cannot perform multivariate multiple regression in glm() function
		#lm_raw <- glm(formula_raw,data = data, weights=weights, family="gaussian")
		model_raw <- lm(formula_raw, data = data, weights = weights)
	}else if(type == "logit"){
		model_raw <- glm(formula_raw, data = data, weights = weights, family = "binomial")
	}else if(type == 'cox'){
		model_raw <- survival::coxph(formula_raw, data = data, weights = weights, method= method)
	}
	return(model_raw)
}

getMulticolX <- function(data, x_name, tolerance){
	x_matrix <- as.matrix(data[, x_name])
	qrx_list <- qr(x_matrix, tol = tolerance)
	rank0 <- qrx_list$rank
	pivot0 <- qrx_list$pivot
	if(rank0 < length(pivot0)){
		multico_x <- colnames(qrx_list$qr)[pivot0[(rank0 + 1) : length(pivot0)]]
	}else{
		multico_x <- NULL
	}
	return(multico_x) # return multicollineared x_names
}

getTestMethod <- function(data, model_raw, type, metric, y_name, test_method_linear, test_method_logit, test_method_cox){
	if(type == 'linear'){
		y_df <- as.matrix(model_raw$model[,y_name])
		n_y <- ncol(y_df)
		n_obs <- nrow(data)
		# get sigma for BIC and CP
		if(n_y==1){
			test_method <- "F"
		}else{
			test_method <- test_method_linear
			if(any(c(metric) == c("BIC", "CP", "Rsq", "adjRsq"))){
				stop("The 'metric' can not be 'BIC', 'CP', 'Rsq' or 'adjRsq' when using multivariate multiple regression!")
			}
		}
		if((metric == "CP" | metric == 'BIC') & lm_raw$rank >= n_obs){
			stop("The 'metric' can not be 'CP' or 'BIC' when variable number is greater than the number of observation.")
		}
	}else if(type == 'logit'){
		test_method <- test_method_logit
	}else if(type == 'cox'){
		test_method <- test_method_cox
	}
	return(test_method)
}

# For "subset", "forward", "backward", "bi-directional": each model needs to calculate PIC. 
#' Fit Model Statistics
#'
#' Fit Model Statistics with least square or likelihood method to return an information criteria value 
#'
#' @param metric Information criteria, including AIC, AICc, BIC, CP, HQ, HQc, Rsq, adjRsq and SBC
#' 
#' @param fit Object of linear model or general linear model
#' 
#' @param type "linear", "cox", or "logit": to calculate information criteria value; for "linear", the "Least Square" method will be used; for "cox" and "logit", "Maximum Likelyhood" method will be used.
#' 
getModelFitStat <- function(metric, fit, type = c("linear","logit", "cox")){
	# "LeastSquare" is for linear; "Likelihood" is for cox and logit; cox and logit are essentially the same except for sample size calculation.
	if (type == "linear"){
		resMatrix <- as.matrix(fit$residuals)
		SSEmatrix <- t(resMatrix) %*% resMatrix
		SSE <- abs(det(SSEmatrix))
		p <- fit$rank
		n <- nrow(resMatrix)
		#yName <- rownames(attr(fit$terms,"factors"))[1]
		vars <- as.character(attr(fit$terms, "variables"))[-1]
		yName <- vars[attr(fit$terms, "response")]
		Y <- as.matrix(fit$model[,yName])
		nY <- ncol(Y)
		if(metric == "AIC"){
			PIC <- n*log(SSE/n)+2*p*nY+nY*(nY+1)+n
		}else if(metric == "AICc"){
			PIC <- n*log(SSE/n)+n*(n+p)*nY/(n-p-nY-1)
		}else if(metric == "CP"){
			PIC <- SSE/sigma(fit)+2*p-n
		}else if(metric == "HQ"){
			PIC <- n*log(SSE/n)+2*log(log(n))*p*nY/n
		}else if(metric == "HQc"){
			#PIC <- n*log(SSE*SSE/n)+2*log(log(n))*p*nY/(n-p-nY-1)
			PIC <- n*log(SSE/n)+2*log(log(n))*p*nY/(n-p-nY-1)
		}else if(metric == "BIC"){
			PIC <- n*log(SSE/n)+2*(2+p)*(n*sigma(fit)/SSE)-2*(n*sigma(fit)/SSE)*(n*sigma(fit)/SSE)
		}else if(metric == "Rsq"){
			#PIC <- 1-(SSE/SST)
			PIC <- summary(fit)$r.squared
		}else if(metric == "adjRsq"){
			#PIC <- 1-(SSE/SST)*(n-1)/(n-p)
			PIC <- summary(fit)$adj.r.squared
		}else if(metric == "SBC"){
			PIC <- n*log(SSE/n)+log(n)*p*nY
		}
	} else if (type %in% c("logit", "cox")){
		ll <- logLik(fit)[1]
		k <- attr(logLik(fit),"df")
		if (type == "cox"){
			n <- fit$nevent
		} else if (type == "logit"){
			n <- nrow(fit$data)
		}
		if(metric == "IC(1)"){
			PIC <- -2*ll+k
		}else if(metric == "IC(3/2)"){
			PIC <- -2*ll+1.5*k
		}else if(metric == "SBC"){
			PIC <- -2*ll+k*log(n)
		}else if(metric == "AICc"){
			PIC <- -2*ll+2*k*(k+1)/(n-k-1)
		}else if(metric == "AIC"){
			PIC <- -2*ll+2*k
		}else if(metric == "HQ"){
			PIC <- -2*ll+2*k*log(log(n))/n
		}else if(metric == "HQc"){
			PIC <- -2*ll+2*k*log(log(n))/(n-k-2)
		}
	}
	return(PIC)
}

## functions for "subset" method:
getFitModel <- function(data, type, x_name_subset, y_name, weights, test_method_cox){
	# obtain a fit (usually reduced) model using custom input variables (x_names)
  fm <- reformulate(x_name_subset, y_name)
	
	if (type == "linear"){
		weight_data <- data * sqrt(weights)
		fit <- lm(fm, data = weight_data)
	} else if (type == "logit"){
		fit <- glm(fm, data = data, weights = weights, family = "binomial")
	} else if (type == "cox"){
		fit <- survival::coxph(fm, data = data, weights = weights, method = test_method_cox)
	}
	fit
}

getInitialSet <- function(data, type, metric, y_name, intercept, include, weights, test_method_cox){
	# obtain the initial model information: if no include variable, return NULL, otherwise return a matrix containing columns of "NumberOfVariables", metric, and "VariablesInModel"
	# metric refers to PIC method, e.g. AIC, BIC, etc. for "logit" and "cox" type, if metric is "SL", the PIC is calculated differently
	if (length(include) != 0){
		initial_set <- matrix(NA, 1, 3)
		colnames(initial_set) <- c("NumberOfVariables", metric, "VariablesInModel")
		if (type == "linear"){
			fit <- getFitModel(data, type, c(intercept, include), y_name, weights)
			PIC <- getModelFitStat(metric = metric, fit, type = type)
			initial_set <- c(length(attr(fit$terms,"term.labels")), PIC, paste(c(intercept, include), collapse = " "))
		} else if (type == "logit"){
			fit <- getFitModel(data, type, c(intercept, include), y_name, weights)
			if (metric == "SL"){
				fit_reduced <- getFitModel(data, type, c(intercept), y_name, weights)
				PIC <- anova(fit_reduced, fit, test = "Rao")[2, "Rao"]
			} else {
				PIC <- getModelFitStat(metric = metric, fit, type = type)
			}
			initial_set[1, 1:3] <- c(fit$rank, PIC, paste0(c(intercept, include), collapse = " "))
		} else if (type == "cox"){
			fit <- getFitModel(data, type, c(include), y_name, weights, method = test_method_cox)
			if (metric == "SL"){
				PIC <- fit$score
			} else {
				PIC <- getModelFitStat(metric = metric, fit, type = type)
			}
			initial_set[1, 1:3] <- c(length(attr(fit$terms, "term.labels")), PIC, paste0(c(include), collapse = " "))
		}
		return(initial_set)
	} else{
		return(NULL)
	}
}

getFinalSet <- function(data, type, metric, x_check, initial_set, y_name, include, weights, intercept, best_n = Inf, test_method_cox){
	final_result <- initial_set
	single_set <- matrix(NA, 1, 3)
	colnames(single_set) <- c("NumberOfVariables", metric, "VariablesInModel")
	subset <- NULL
	
	for (nv in 1:length(x_check)){
		comTable <- combn(x_check, nv)
		for (ncom in 1:ncol(comTable)){
			if (type == "linear"){
				comVar <- c(intercept, include, comTable[, ncom])
				fit <- getFitModel(data, type, comVar, y_name, weights)
				PIC <- getModelFitStat(metric = metric, fit, type = type)
				single_set[1, 1:3] <- c(length(attr(fit$terms,"term.labels")), PIC, paste(comVar, collapse = " "))
			} else if (type == "logit"){
				comVar <- c(intercept, include, comTable[, ncom])
				fit <- getFitModel(data, type, comVar, y_name, weights)
				if (metric == "SL"){
					PIC <- anova(fit_reduced, fit, test = "Rao")[2, "Rao"] 
				} else{
					PIC <- getModelFitStat(metric = metric, fit, type = type)
				}
				single_set[1, 1:3] <- c(fit$rank, PIC, paste0(comVar, collapse = " "))
			} else if (type == "cox"){
				comVar <- c(include, comTable[, ncom])
				fit <- getFitModel(data, type, comVar, y_name, weights, test_method_cox = test_method_cox)
				if (metric == "SL"){
					PIC <- fit$score
				}else{
					PIC <- getModelFitStat(metric = metric, fit, type = type)
				}
				single_set[1, 1:3] <- c(attr(logLik(fit), "df"), PIC, paste0(comVar, collapse = " "))
			}
		  subset <- rbind(subset, single_set)
		}
		
		best_subset <- as.data.frame(subset)
		best_subset[, 2] <- as.numeric(best_subset[, 2])
		if (metric %in% c("SL", "Rsq", "adjRsq")){
			# "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
			decreasing = TRUE
		} else{
			decreasing = FALSE
		}
		sub_result_sort <- best_subset[order(best_subset[, 2], decreasing = decreasing), ]
		
		if(nrow(subset) < best_n){
			best_n <- nrow(subset)
		}
		final_result <- rbind(final_result, sub_result_sort[1:best_n, ])
	}
	
	final_result <- final_result[-1, ]
	final_result
}

getXNameSelected <- function(final_set){
	final_set[, 2] %in% min(final_set[, 2])
	if (metric %in% c("SL", "Rsq", "adjRsq"){
		# "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
		x_name_selected <- unlist(strsplit(final_set[which.max(as.numeric(final_set[, 2])), 3]," "))
	} else{
		x_name_selected <- unlist(strsplit(final_set[which.min(as.numeric(final_set[, 2])), 3]," "))
	}
	x_name_selected
}

getFinalSetWrapper <- function(input_data, type, metric, y_name, intercept, include, weights, best_n, test_method_cox){
	# a wrapper to obtain x_name_selected
	## obtain initial model info
	initial_set <- getInitialSet(input_data, type, metric, y_name, intercept, include, weights, test_method_cox)
	
	## obtain final model info
	x_check <- setdiff(x_name, include)
	final_set <- getFinalSet(input_data, type, metric, x_check, initial_set, y_name, include, weights, intercept, best_n, test_method_cox)
	
	## add rownames to sort the variables in final_set when output
	rownames(final_set) <- c(1:nrow(final_set))
	return(final_set)
}



## functions for "forward" method:

## functions for "backward" method:

## functions for "bi-directional" method:



formatTable <- function(tbl, tbl_name = "Test"){
	tbl_list <- list(tbl)
	names(tbl_list) <- tbl_name
	class(tbl_list) <- "StepReg"
	return(tbl_list)
}

x_name <- getXname(formula, input_data)
y_name <- getYname(formula, input_data)
intercept <- getIntercept(formula, input_data, type) # char type
merged_include <- getMergedInclude(include)
model_raw <- getModel(input_data, type, method, x_name, y_name, weights, intercept)
test_method <- getTestMethod(data, model_raw, type, metric, y_name, test_method_linear, test_method_logit, test_method_cox)
multico_x <- getMulticolX(data, x_name, tolerance)
merged_multico_x <- paste0(multico_x, sep = " ")
if(merged_multico_x == " "){
	merged_multico_x <- "NULL"
}

getTable1SummaryOfParameters <- function(data, x_name, y_name, merged_multico_x, 
																				 merged_include, strategy, metric, sle, sls, 
																				 test_method, tolerance, intercept){
	# generate: table1: Summary of Parameters
	table_1_summary_of_parameters <- tibble(
		Parameter = c("response variable",
									"included variable",
									"strategy",
									"metric",
									"entry significance level (sle)",
									"stay significance level (sls)",
									"test method",
									"tolerance of multicollinearity",
									"multicollinearity term",
									"intercept"),
		Value = c(y_name,
							merged_include,
							strategy,
							metric,
							sle,
							sls,
							test_method,
							tolerance,
							merged_multico_x,
							intercept)
	)
	
	# get rid of unrelevant variables from table 1:
	if(metric == "SL"){
		if(strategy == "forward"){
			# "sls" is not relevant
			table_1_summary_of_parameters <- table_1_summary_of_parameters[-6, ]
		}else if(strategy == "backward"){
			# "sle" is not relevant
			table_1_summary_of_parameters <- table_1_summary_of_parameters[-5, ]
		}else if(strategy == "subset"){
			# "sle" and "sls" are not relevant
			table_1_summary_of_parameters <- table_1_summary_of_parameters[-c(5:6), ]
		}
	}else{
		table_1_summary_of_parameters <- table_1_summary_of_parameters[-c(5:6), ]
	}
	
	if(type == 'cox'){
		# "intercept" is not relevant
		table_1_summary_of_parameters <- table_1_summary_of_parameters[-10, ]
	}
	
	return(table_1_summary_of_parameters)
}

# Obtaion table 1: Summary of Parameters
data <- input_data
table1 <- getTable1SummaryOfParameters(data, x_name, y_name, merged_multico_x, 
																			 merged_include, strategy, metric, sle, sls, 
																			 test_method, tolerance, intercept)
table1 <- formatTable(table1, tbl_name = "Summary of Parameters")


stepwiseParams <- function(type,
                          formula, 
                          input_data,
                          include,
                          strategy,
                          metric,
                          sle,
                          sls,
                          test_method_linear,
                          test_method_logit,
                          test_method_cox,
                          tolerance=1e-7,
                          weights){
	# term_form <- terms(formula, data = input_data)
	# vars <- as.character(attr(term_form, "variables"))[-1]
	# y_name <- vars[attr(term_form, "response")]
	# x_name <- attr(term_form, "term.labels")
	# 
	# if(type != 'cox'){
	#   if(attr(term_form, "intercept") == 0){
	#     intercept <- "0"
	#   }else{
	#     intercept <- "1"
	#   }
	#   intercept_name <- intercept
	# }else{
	#   intercept <- NULL
	#   intercept_name <- "NULL"
	# }


	# if(is.character(include)){
	# 	include_name <- include
	# 	merge_include_name <- paste0(include_name,collapse=" ")
	# }else if(is.null(include)){
	# 	include_name <- NULL
	# 	merge_include_name <- "NULL"
	# }
	
	# weight_data seems not used
	# if(!is.null(weights)){
	# 	if(length(weights) == nrow(input_data)){
	# 		weight_data <- input_data*sqrt(weights)
	# 	}else{
	# 		stop("Variable length is different ('(weights)')")
	# 	}
	# }else{
	# 	weight_data <- input_data
	# }

	# formula_raw <- reformulate(c(intercept,x_name),y_name) # create a new formula given explicit x, y, and intercept, bypassed the x being .
	# 
	# if(type == 'linear'){
	#   ## cannot perform multivariate multiple regression in glm() function
	#   #lm_raw <- glm(formula_raw,data=input_data, weights=weights, family="gaussian")
	#   lm_raw <- lm(formula_raw,data=input_data,weights = weights)
	# }else if(type == "logit"){
	#   lm_raw <- glm(formula_raw,data=input_data, weights=weights, family="binomial")
	# }else if(type == 'cox'){
	#   lm_raw <- survival::coxph(formula_raw,data=input_data, weights=weights,method=test_method_cox)
	# }
	
	all_var_class <- attr(lm_raw$terms, "dataClasses")

	## for table2
	class_table <- as.data.frame(table(all_var_class))
	colnames(class_table) <- c("class", "variable")
	
	for(i in names(table(all_var_class))){
		class_table[names(table(all_var_class)) %in% i,2] <- paste0(names(all_var_class[all_var_class %in% i]), collapse=" ")
	}
	
	## detect multicollinearity
	if(any(all_var_class=="factor")){
	  factor_var <- names(which(all_var_class=="factor"))
	  for(i in factor_var){
	    input_data[,i] <- as.factor(as.numeric(input_data[,i]))
	  }
	}
	# detect and remove multicollinearity
	# #removeColinear <- function(df_in,col_name){
	#   x_matrix <- as.matrix(input_data[,x_name])
	#   qrx_list <- qr(x_matrix,tol=tolerance)
	#   rank0 <- qrx_list$rank
	#   pivot0 <- qrx_list$pivot
	#   if(rank0 < length(pivot0)){
	#     multico_x <- colnames(qrx_list$qr)[pivot0[(rank0+1):length(pivot0)]]
	#     multicol_merged_name <- paste0(multico_x,collapse=" ")
	#   }else{
	#     multico_x <- NULL
	#     multicol_merged_name <- "NULL"
	#   }
	#   #return(list(multico_x,multicol_merged_name))
	# #}
	
	x_name_remove_multicol <- setdiff(x_name, multico_x) # not used?
	# if(type == 'linear'){
	#   y_df <- as.matrix(lm_raw$model[,y_name])
	#   n_y <- ncol(y_df)
	#   n_obs <- nrow(input_data)
	#   # get sigma for BIC and CP
	#   if(n_y==1){
	#     test_method <- "F"
	#   }else{
	#     test_method <- test_method_linear
	#     if(any(c(metric)==c("BIC","CP","Rsq","adjRsq"))){
	#       stop("Can't specify 'BIC','CP','Rsq' or 'adjRsq' when using multivariate multiple regression")
	#     }
	#   }
	#   if((metric=="CP" | metric=='BIC') & lm_raw$rank >= n_obs){
	#     stop("'metric' can't specify 'CP' or 'BIC' when variable number is greater than number of observation")
	#   }
	# }else if(type == 'logit'){
	#   test_method <- test_method_logit
	# }else if(type == 'cox'){
	#   test_method <- test_method_cox
	# }

	# table1:
	result <- list()
	# table_1_summary_of_parameters <- matrix(NA,10,1)
	# table_1_summary_of_parameters <- cbind(table_1_summary_of_parameters,matrix(c(y_name,merge_include_name,strategy,metric,sle,sls,test_method,tolerance,multicol_merged_name,intercept_name),10,1))
	# table_1_summary_of_parameters <- data.frame(table_1_summary_of_parameters)
	# colnames(table_1_summary_of_parameters) <- c("Paramters","Value")
	# table_1_summary_of_parameters[,1] <- c("Response Variable",
	#                 "Included Variable",
	#                 "Strategy Method",
	#                 "Metric Criterion",
	#                 "Entry Significance Level(sle)",
	#                 "Stay Significance Level(sls)",
	#                 "Variable significance test",
	#                 "Tolerance of Multicollinearity",
	#                 "Multicollinearity Terms",
	#                 "Intercept")
	# 
	# if(metric=="SL"){
	#   if(strategy=="forward"){
	#     table_1_summary_of_parameters <- table_1_summary_of_parameters[-6,]
	#   }else if(strategy=="backward"){
	#     table_1_summary_of_parameters <- table_1_summary_of_parameters[-5,]
	#   }else if(strategy=="subset"){
	#     table_1_summary_of_parameters <- table_1_summary_of_parameters[-c(5:6),]
	#   }
	# }else{
	#   table_1_summary_of_parameters <- table_1_summary_of_parameters[-c(5:6),]
	# }
	# rownames(table_1_summary_of_parameters) <- 1:nrow(table_1_summary_of_parameters)
	# if(type == 'cox'){
	#   table_1_summary_of_parameters <- table_1_summary_of_parameters[-nrow(table_1_summary_of_parameters),]
	# }
	result$'Summary of Stepwise Regression Parameters' <- table_1_summary_of_parameters
	result$'Variables Type' <- class_table
	return(result)
	
}





## Kai: best_n helpers: stepwiseLinear:115-161
if(strategy=="subset"){
	## best subset model selection
	tempresult <- matrix(NA,1,4)
	colnames(tempresult) <- c("NoVariable","RankModel",metric,"VariablesEnteredinModel")
	finalResult <- tempresult
	if(!is.null(includeName)){
		lmIncForm <- reformulate(c(intercept,includeName), yName)
		lmInc <- lm(lmIncForm,data=weightData)
		tempresult[1,c(1:4)] <- c(length(attr(lmInc$terms,"term.labels")),lmInc$rank,modelFitStat(metric,lmInc,"LeastSquare"),paste(c(intercept,includeName),collapse=" "))
		finalResult <- rbind(finalResult,tempresult)
		checkX <- xName[!xName %in% includeName]
	}else{
		checkX <- xName
	}
	for(nv in 1:length(checkX)){
		comTable <- combn(length(checkX),nv)
		subSet <- NULL
		for(ncom in 1:ncol(comTable)){
			comVar <- c(intercept,includeName,checkX[comTable[,ncom]])
			tempFormula <- reformulate(comVar, yName)
			lmresult <- lm(tempFormula,data=weightData)
			tempresult[1,1:4] <- c(length(attr(lmresult$terms,"term.labels")),lmresult$rank,modelFitStat(metric,lmresult,"LeastSquare"),paste(comVar,collapse=" "))
			subSet <- rbind(subSet,tempresult)
		}
		
		if(nrow(subSet) < best_n){
			best_n <- nrow(subSet)
		}
		
		bestSubSet <- as.data.frame(subSet)
		bestSubSet[,2] <- as.numeric(bestSubSet[,2])
		if(metric=="Rsq" | metric=="adjRsq"){
			subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
		}else{
			subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
		}
		finalResult <- rbind(finalResult,subResultSort[1:best_n,])
	}
	finalResult <- finalResult[-1,]
	rownames(finalResult) <- 1:nrow(finalResult)
	result$'Process of Selection' <- finalResult
	if(metric=="Rsq" | metric=="adjRsq"){
		xModel <- unlist(strsplit(finalResult[which.max(as.numeric(finalResult[,3])),4]," "))
	}else{
		xModel <- unlist(strsplit(finalResult[which.min(as.numeric(finalResult[,3])),4]," "))
	}
}




# for testing with logit model:
type='logit'
data(mtcars)
formula <- vs ~ .
input_data=mtcars
metric="SL"
sle=0.15
sls=0.15
include <- NULL
weights=NULL
tolerance=1e-7


# 99 - 161: stepwiseLogit
if(strategy=="subset"){ #subset
	bestSubSet <- NULL
	singSet <- matrix(NA,1,3)
	colnames(singSet) <- c("NumberOfVariables",metric,"VariablesInModel")
	finalResult <- singSet
	fmReduce <- reformulate(c(intercept), yName)
	fitReduce <- glm(fmReduce,data=data, weights=weights, family="binomial")
	if(length(includeName)!=0){
		fm <- reformulate(c(intercept,includeName), yName)
		#fit <- multinom(fm, data = YXdata, weights = weights)
		fit <- glm(fm,data=data, weights=weights, family="binomial")
		if(metric=="SL"){
			PIC <- anova(fitReduce,fit,test="Rao")[2,"Rao"]
		}else{
			PIC <- modelFitStat(metric,fit,"Likelihood")
		}
		singSet[1,1:3] <- c(fit$rank,PIC,paste0(c(intercept,includeName),collapse=" "))
		includeSubSet <- singSet
		xCheck <- setdiff(xName, includeName)
	}else{
		includeSubSet <- NULL
		xCheck <- xName
	}
	for(nv in 1:length(xCheck)){
		subSet <- NULL
		comTable <- combn(xCheck,nv)
		for(ncom in 1:ncol(comTable)){
			comVar <- c(intercept,includeName,comTable[,ncom])
			fm <- reformulate(comVar, yName)
			fit <- glm(fm,data = data,weights=weights,family="binomial")
			if(metric=="SL"){
				PIC <- anova(fitReduce,fit, test="Rao")[2,"Rao"] 
			}else{
				PIC <- modelFitStat(metric,fit,"Likelihood")
			}
			singSet[1,1:3] <- c(fit$rank,PIC,paste0(comVar,collapse=" "))
			subSet <- rbind(subSet,singSet)
		}
		bestSubSet <- as.data.frame(subSet)
		bestSubSet[,2] <- as.numeric(bestSubSet[,2])
		if(metric=="SL"){
			subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
		}else{
			subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
		}
		
		if(nrow(subSet) < best_n){
			best_n <- nrow(subSet)
		}
		
		finalResult <- rbind(finalResult,subResultSort[1:best_n,])
	}
	finalResult <- finalResult[-1,]
	RegPIC <- rbind(includeSubSet,finalResult)
	rownames(RegPIC) <- c(1:nrow(RegPIC))
	result$'Process of Selection' <- RegPIC
	RegPIC[,2] %in% min(RegPIC[,2])
	if(metric=="SL"){
		xModel <- unlist(strsplit(RegPIC[which.max(as.numeric(RegPIC[,2])),3]," "))
	}else{
		xModel <- unlist(strsplit(RegPIC[which.min(as.numeric(RegPIC[,2])),3]," "))
	}
}


# 91 - 150: stepwiseCox:
if(strategy=="subset"){ #subset
	bestSubSet <- NULL
	singSet <- matrix(NA,1,3)
	colnames(singSet) <- c("NumberOfVariables",metric,"VariablesInModel")
	finalResult <- singSet
	if(length(includeName)!=0){
		fm <- reformulate(c(includeName), yName)
		fit <- survival::coxph(fm,data=data, weights=weights,method=test_method_cox)
		if(metric=="SL"){
			#PIC <- summary(fit)[[sigMethod]][1]
			PIC <- fit$score
		}else{
			PIC <- modelFitStat(metric,fit,"Likelihood",TRUE)
		}
		singSet[1,1:3] <- c(length(attr(fit$terms,"term.labels")),PIC,paste0(c(includeName),collapse=" "))
		includeSubSet <- singSet
		xCheck <- setdiff(xName,includeName)
	}else{
		includeSubSet <- NULL
		xCheck <- xName
	}
	for(nv in 1:length(xCheck)){
		subSet <- NULL
		comTable <- combn(xCheck,nv)
		for(ncom in 1:ncol(comTable)){
			comVar <- c(includeName,comTable[,ncom])
			fm <- reformulate(comVar, yName)
			fit <- survival::coxph(fm,data = data,weights=weights,method=test_method_cox)
			if(metric=="SL"){
				PIC <- fit$score
			}else{
				PIC <- modelFitStat(metric,fit,"Likelihood",TRUE)
			}
			singSet[1,1:3] <- c(attr(logLik(fit),"df"),PIC,paste0(comVar,collapse=" "))
			subSet <- rbind(subSet,singSet)
		}
		bestSubSet <- as.data.frame(subSet)
		bestSubSet[,2] <- as.numeric(bestSubSet[,2])
		if(metric=="SL"){
			subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
		}else{
			subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
		}
		
		if(nrow(subSet) < best_n){
			best_n <- nrow(subSet)
		}
		
		finalResult <- rbind(finalResult,subResultSort[1:best_n,])
	}
	finalResult <- finalResult[-1,]
	RegPIC <- rbind(includeSubSet,finalResult)
	rownames(RegPIC) <- c(1:nrow(RegPIC))
	result$'Process of Selection' <- RegPIC
	if(metric=="SL"){
		xModel <- unlist(strsplit(RegPIC[which.max(as.numeric(RegPIC[,2])),3]," "))
	}else{
		xModel <- unlist(strsplit(RegPIC[which.min(as.numeric(RegPIC[,2])),3]," "))
	}
}

# block2 and block 3 differ by: ChatGPT
## block2:
if(metric=="SL"){
	#PIC <- summary(fit)[[sigMethod]][1]
	PIC <- fit$score
}else{
	PIC <- modelFitStat(metric,fit,"Likelihood",TRUE)
}
## block3:
if(metric=="SL"){
	PIC <- anova(fitReduce,fit,test="Rao")[2,"Rao"]
}else{
	PIC <- modelFitStat(metric,fit,"Likelihood")
}









#note1: test_method_linear should be 'F' for univariate and c(“Pillai”, “Wilks”, “Hotelling-Lawley”, “Roy”) for multivariates
#
getAnovaStat <- function(fit_reduced, fit_full, type, test_method_linear, test_method_logit){
  #fvalue <- NA
  
  if (type == "linear") {
    test_method <- test_method_linear
    ptype <- 'Pr(>F)'
    if(test_method_linear %in% c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")){
      stattype <- 'approx F'
    }else{
      stattype <- 'F'
    }
    #pic <- anova(fit_reduced, fit_full, test = test_method_linear)[2, 'Pr(>F)']
    # statistics is F value for linear
    #statistics <- anova(fit_reduced, fit_full, test = test_method_linear)[2, 'F']
  } else if (type == "logit") {
    test_method <- test_method_logit
    ptype <- 'Pr(>Chi)'
    if(test_method_logit == "Rao"){
      stattype <- "Rao"
    }else{
      stattype <- "Deviance"
    }
    
    #pic <- anova(fit_reduced, fit_full, test = test_method_logit)[2, 'Pr(>Chi)']
    # statistics is Chi value for logit
    #statistics <- anova(fit_reduced, fit_full, test = test_method_logit)[2, 'Chi']
  } else if (type == "cox") {
    # test is not used in cox regression
    test_method <- ""
    ptype <- 'Pr(>|Chi|)'
    stattype <- "Chisq"
    #pic <- anova(fit_reduced, fit_full)[2, 'Pr(>|Chi|)']
    # statistics is |Chi| value for cox
    #statistics <- anova(fit_reduced, fit_full)[2, '|Chi|']
  }
  anova_table <- anova(fit_reduced, fit_full, test = test_method)
  statistics <- anova_table[2,stattype]
  pic <- anova_table[2, ptype]
  return(c("statistics" = statistics, "pic" = pic))
}

## get pic based on model fit, it needs fit_reduced and fit_full for SL and only fit_formula for other metrics
## used in stepwise in step0: SL:0,1,inf  non-SL:
## fit_fm<-fit_intercept
getInitStepModelStat <- function(fit_intercept,fit_fm,type,strategy,metric,intercept,include,test_method_cox,test_method_linear,test_method_logit){
  if(metric == "SL"){
    if(!is.null(include)){
      if(all(include %in% attr(fit_fm$terms,"term.labels")) & strategy != "backward"){
        f_pic_vec <- getAnovaStat(fit_intercept,fit_fm,type,test_method_cox,test_method_linear,test_method_logit)
        pic <- f_pic_list["pic"]
      }else{
        pic <- 1
      }
    }else{
      pic <- 1
    }
  }else{
    if(strategy == "backward"){
      pic <- getModelFitStat(metric,fit_fm,type)
    }else{
      if(!is.null(include)){
        if(all(include %in% attr(fit_fm$terms,"term.labels"))){
          pic <- getModelFitStat(metric,fit_fm,type)
        }else{
          if(intercept == '1'){
            pic <- getModelFitStat(metric,fit_fm,type)
          }else{
            if(metric %in% c("Rsq","adjRsq") & type == "linear"){
              pic <- 0
            }else{
              pic <- Inf
            }
          }
        }
      }else{
        if(intercept == '1'){
          pic <- getModelFitStat(metric,fit_fm,type)
        }else{
          if(metric %in% c("Rsq","adjRsq") & type == "linear"){
            pic <- 0
          }else{
            pic <- Inf
          }
        }
      }
    }
  }
  return(pic)
}

#return 3 num for IC and remove 1st of 3 num for SL
getNumberEffect <- function(fit,type){
  if(type == "linear"){
    vec <- c(fit$rank,length(attr(fit$terms,"term.labels")),fit$rank)
  }else if(type == "logit"){
    vec <- c(fit$rank,fit$rank,fit$rank)
  }else if(type == "cox"){
    vec <- c(attr(logLik(fit),"df"),attr(logLik(fit),"df"),attr(logLik(fit),"df"))
  }
  return(vec)
}

initialSubBestPoint <- function(metric){
  sub_best_point <- data.frame(Step=numeric(),
                               EnteredEffect=character(),
                               RemovedEffect=character(),
                               DF=numeric(),
                               NumberEffectIn=numeric(),
                               NumberParmsIn=numeric(),
                               metric=numeric())
  colnames(sub_best_point)[ncol(sub_best_point)] <- metric
  return(sub_best_point)
}

##getInitialStepwise -> getInitialModel
##getInitialSubset -> 
getInitialStepwise <- function(data,type,strategy,metric,weights,x_name,y_name,intercept,include,method=c("efron","breslow","exact")){
  sub_best_point <- initialSubBestPoint(metric)
  best_point <- sub_best_point
  if(strategy == "backward"){
    add_or_remove <- "remove"
    #the order of variable in x_in_model will affect pic calculation.
    x_in_model <- c(setdiff(x_name,include))
    x_notin_model <- NULL
    #fit_full <- getModel(data=data, type=type, x_name=x_in_model, y_name=y_name, weights=weights, intercept=intercept, test_method_cox=test_method_cox,test_method_linear=test_method_linear,test_method_logit=test_method_logit)
    fit_full <- getModel(data=data, type=type, x_name=c(include,x_in_model), y_name=y_name, weights=weights, intercept=intercept)
    pic <- getInitStepModelStat(fit_intercept=NULL,fit_fm=fit_full,type=type,strategy=strategy,metric=metric,intercept=intercept,include=include,test_method_cox=test_method_cox,test_method_linear=test_method_linear,test_method_logit=test_method_logit)
    num_eff_para_in <- getNumberEffect(fit=fit_full,type=type)
    best_point[1,] <- c(rep("",3),num_eff_para_in,pic)
  }else{
    add_or_remove <- "add"
    x_in_model <- NULL
    x_notin_model <- setdiff(x_name,include)
    ## for intercept
    #fit_intercept <- getModel(data=data, type=type, x_name=x_in_model, y_name=y_name, weights=weights, intercept=intercept, method=test_method_logit)
    fit_intercept <- getModel(data=data, type=type, x_name=NULL, y_name=y_name, weights=weights, intercept=intercept)
    pic <- getInitStepModelStat(fit_intercept=fit_intercept,fit_fm=fit_intercept,type=type,strategy=strategy,metric=metric,intercept=intercept,include=include,test_method_cox=test_method_cox,test_method_linear=test_method_linear,test_method_logit=test_method_logit)
    num_eff_para_in <- getNumberEffect(fit=fit_intercept,type=type)
    best_point[1,] <- c("",intercept,"",num_eff_para_in,pic)
    ## for include
    if(!is.null(include)){
      #fit_include <- getModel(data=data, type=type, x_name=x_in_model, y_name=y_name, weights=weights, intercept=intercept, method=test_method_logit)
      fit_include <- getModel(data=data, type=type, x_name=include, y_name=y_name, weights=weights, intercept=intercept)
      pic <- getInitStepModelStat(fit_intercept=fit_intercept,fit_fm=fit_include,type=type,strategy=strategy,metric=metric,intercept=intercept,include=include,test_method_cox=test_method_cox,test_method_linear=test_method_linear,test_method_logit=test_method_logit)
      num_eff_para_in <- getNumberEffect(fit=fit_include,type=type)
      sub_best_point[1,] <- c("",paste0(include,collapse=" "),"",anova(fit_include,fit_intercept)[2,'Df'],num_eff_para_in[-1],pic)
      best_point <- rbind(best_point,sub_best_point)
    }
  }
  best_point$Step <- 0
  return(list("add_or_remove"=add_or_remove,"x_in_model"=x_in_model,"x_notin_model"=x_notin_model,"best_point"=best_point))
}

#addOrRemX2StepModel <- function(add_or_remove,data,type,strategy,metric,sle,sls,weights,y_name,x_in_model,x_notin_model,intercept, include,index,test_method_cox,test_method_linear,test_method_logit,best_point){
getCandStepModel <- function(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method_cox,test_method_linear,test_method_logit){
  #fit_x_in_model <- getModel(data=data, type=type, x_name=x_in_model, y_name=y_name, weights=weights, intercept=intercept, method=test_method_cox)
  fit_x_in_model <- getModel(data=data, type=type, x_name=c(include,x_in_model), y_name=y_name, weights=weights, intercept=intercept)
  BREAK <- FALSE
  if(add_or_remove == "add"){
    x_test <- x_notin_model
  }else{
    x_test <- x_in_model
  }
  x_test_list <- as.list(x_test)
  names(x_test_list) <- x_test
  if(length(x_test) == 0){
    BREAK <- TRUE
  }
  if(add_or_remove == "add"){
    x_name_list <- lapply(x_test_list,function(x){c(x_in_model,x)})
  }else{
    x_name_list <- lapply(x_test_list,function(x){setdiff(x_in_model,x)})
  }
  #fit_x_list <- lapply(x_test_list,function(x){getModel(data=data, type=type, x_name=x, y_name=y_name, weights=weights, intercept=intercept, method=test_method_cox)})
  fit_x_list <- lapply(x_name_list,function(x){getModel(data=data, type=type, x_name=c(include,x), y_name=y_name, weights=weights, intercept=intercept)})
  
  if(metric == "SL"){
    #f_pic_list <- sapply(fit_x_list,function(x){getAnovaStat(fit_reduced=x,fit_full=fit_x_in_model,type=type,test_method_linear,test_method_logit)})
    f_pic_vec <- sapply(fit_x_list,function(x){getAnovaStat(fit_reduced=x,fit_full=fit_x_in_model,type=type,test_method_linear="F",test_method_logit="Rao")})
    pic_set <- f_pic_vec[2,]
    f_set <- f_pic_vec[1,]
  }else{
    if(add_or_remove == "remove" & length(x_test) == 1 & intercept == "0"){
      pic_set <- Inf
      names(pic_set) <- x_test
    }else{
      pic_set <- sapply(fit_x_list,function(x){getModelFitStat(metric,x,type)})
    }
  }
  if(metric == "Rsq" | metric == "adjRsq" | (metric == "SL" & add_or_remove == "remove")){
    pic <- max(pic_set)
    minmax_var <- names(which.max(pic_set))
    best_candidate_model <- fit_x_list[[minmax_var]]
  }else{
    pic <- min(pic_set)
    minmax_var <- names(which.min(pic_set))
    best_candidate_model <- fit_x_list[[minmax_var]]
    if(sum(pic_set %in% pic) > 1 & metric == "SL"){
      Fvalue <- max(f_set)
      minmax_var <- names(which.max(f_set))
      best_candidate_model <- fit_x_list[[minmax_var]]
      pic <- pic_set[minmax_var]
    }
  }
  if(type != "cox"){
    if(best_candidate_model$rank == fit_x_in_model$rank & add_or_remove == "add"){
      BREAK <- TRUE
    }
  }
  return(list("pic"=pic,"minmax_var"=minmax_var,"best_candidate_model"=best_candidate_model,"BREAK"=BREAK))
}

getGoodnessFit <- function(best_candidate_model,y_name,metric){
  smr <- summary(best_candidate_model)
  n_y <- ncol(as.matrix(best_candidate_model$model[,y_name]))
  if(n_y == 1){
    f <- smr$fstatistic
    if(is.nan(f[1])){
      pval <- NaN
    }else{
      pval <- pf(f[1],f[2],f[3],lower.tail=F)
    }
  }else{
    for(ny in 1:n_y){
      f <- smr[[ny]]$fstatistic
      if(is.nan(f[1])){
        pval <- NaN
      }else{
        pval <- pf(f[1],f[2],f[3],lower.tail=F)
      }
    }
  }
  if(is.nan(pval) == TRUE & (metric != 'Rsq' | metric != 'adjRsq')){
    BREAK <- TRUE
  }else{
    BREAK <- FALSE
  }
  return(BREAK)
}

checkEnterOrRemove <- function(add_or_remove,best_candidate_model,type,metric,y_name,pic,sls,sle,best_point){
  if(metric == 'SL'){
    if(add_or_remove == "remove"){
      indicator <- pic > sls
    }else{
      indicator <- pic < sle
    }
  }else if(metric == 'Rsq' | metric == 'adjRsq'){
    indicator <- pic > as.numeric(best_point[nrow(best_point),7])
  }else{
    indicator <- pic <= as.numeric(best_point[nrow(best_point),7])
  }
  if(indicator == TRUE & type == "linear" & (metric != "Rsq"|metric != "adjRsq")){
    BREAK <- getGoodnessFit(best_candidate_model,y_name,metric)
  }else{
    BREAK <- FALSE
  }
  return(c("indicator"=indicator,"BREAK"=BREAK))
}

updateXinModel <- function(add_or_remove,indicator,best_candidate_model,BREAK,pic,x_in_model,x_notin_model,best_point,minmax_var){
  sub_best_point <- initialSubBestPoint(metric)
  if(indicator == TRUE & BREAK == FALSE){
    if(add_or_remove == "add"){
      x_in_model <- append(x_in_model,minmax_var)
      x_notin_model <- setdiff(x_notin_model,minmax_var)
      sub_best_point[1,] <- c(as.numeric(best_point[nrow(best_point),1])+1,minmax_var,"",getNumberEffect(fit=best_candidate_model,type),pic)
    }else{
      x_notin_model <- append(x_notin_model,minmax_var)
      x_in_model <- setdiff(x_in_model,minmax_var)
      sub_best_point[1,] <- c(as.numeric(best_point[nrow(best_point),1])+1,"",minmax_var,getNumberEffect(fit=best_candidate_model,type),pic)
    }
    best_point <- rbind(best_point,sub_best_point)
    #best_point[nrow(best_point),1] <- as.numeric(best_point[1,nrow(best_point)-1]) + 1
  }else{
    BREAK <- TRUE
  }
  return(list("BREAK"=BREAK,"best_point"=best_point,"x_in_model"=x_in_model,"x_notin_model"=x_notin_model))
}

getFinalStepModel <- function(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method_cox,test_method_linear,test_method_logit){
  while(TRUE){
    out_cand_stepwise <- getCandStepModel(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method_cox,test_method_linear,test_method_logit)
    BREAK <- out_cand_stepwise$BREAK
    minmax_var <- out_cand_stepwise$minmax_var
    if(BREAK == TRUE){
      break
    }
    best_candidate_model <- out_cand_stepwise$best_candidate_model
    pic <- out_cand_stepwise$pic
    
    
    if(type == "linear"){
      if(best_candidate_model$rank != 0){
        BREAK <- getGoodnessFit(best_candidate_model,y_name,metric)
        if(BREAK == TRUE){
          break
        }
      }
    }
    
    out_check <- checkEnterOrRemove(add_or_remove,best_candidate_model,type,metric,y_name,pic,sls,sle,best_point)
    indicator <- out_check["indicator"]
    BREAK <- out_check["BREAK"]
    if(BREAK == TRUE){
      break
    }
    
    out_updateX <- updateXinModel(add_or_remove,indicator,best_candidate_model,BREAK,pic,x_in_model,x_notin_model,best_point,minmax_var)
    x_in_model <- out_updateX$x_in_model
    x_notin_model <- out_updateX$x_notin_model
    best_point <- out_updateX$best_point
    
    if(indicator == TRUE){
      if(strategy == 'bidirection'){
        if(add_or_remove == "remove"){
          next
        }else{
          add_or_remove <- "remove"
          next
        }
      }else{
        next
      }
    }else{
      if(strategy == 'bidirection' & add_or_remove == "remove") {
        add_or_remove <- "add"
        next
      }else{
        break
      }
    }
  }
  
  best_point$DF <- abs(as.numeric(best_point$DF))
  best_point$DF[is.na(best_point$DF)] <- ""
  if(type == "cox" && strategy != "backward"){
    best_point <- best_point[-1,]
  }
  return(list("best_point"=best_point,"x_in_model"=x_in_model))
}

getTBLFianlVariable <- function(include,x_in_model){
  all_x_in_model <- c(include,x_in_model)
  variables <- as.data.frame(t(data.frame(all_x_in_model)))
  colnames(variables) <- paste0("variable",1:ncol(variables))
  rownames(variables) <- c("x in model")
  table4 <- formatTable(variables,tbl_name = "Table 4. Selected Varaibles")
  return(table4)
}

getTBLCoefModel <- function(type,intercept,include,x_in_model,y_name,n_y,data,test_method_cox){
  if(is.null(c(include,x_in_model))){
    summary_model <- NULL
  }else{
    summary_model <- summary(getModel(data, type, x_name=c(include,x_in_model), y_name, weights, intercept, method=test_method_cox))
    summary_model_list <- list()
    if(n_y>1){
      #i=names(summary_model)[1]
      for(i in names(summary_model)){
        subsummary_model <- summary_model[[i]]$coefficients
        col_name <- colnames(subsummary_model)
        subsummary_model <- data.frame(rownames(subsummary_model),subsummary_model)
        colnames(subsummary_model) <- c("Variable",col_name)
        summary_model_list[i] <- list(subsummary_model)
      }
    }else{
      subsummary_model <- summary_model$coefficients
      col_name <- colnames(subsummary_model)
      subsummary_model <- data.frame(rownames(subsummary_model),subsummary_model)
      colnames(subsummary_model) <- c("Variable",col_name)
      summary_model_list <- list(subsummary_model)
      names(summary_model_list) <- y_name
    }
  }
  table5 <- formatTable(summary_model_list, tbl_name = "Table 5. Summary of Model for")
  return(table5)
}