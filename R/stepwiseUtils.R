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
	  intercept <- NULL
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

getModel <- function(data, type, method, x_name, y_name, weights, intercept){
	# create a new formula given explicit x, y, and intercept, bypassed the x being .
	formula_raw <- reformulate(c(intercept, x_name), y_name)
	
	if(type == 'linear'){
		## cannot perform multivariate multiple regression in glm() function
		#lm_raw <- glm(formula_raw,data = data, weights=weights, family="gaussian")
		model_raw <- lm(formula_raw, data = data, weights = weights)
	}else if(type == "logit"){
		model_raw <- glm(formula_raw, data = data, weights = weights, family = "binomial")
	}else if(type == 'cox'){
		model_raw <- survival::coxph(formula_raw, data = data, weights = weights, method= test_method_cox)
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

formatTable <- function(tbl, tbl_name = "Test"){
	tbl_list <- list(tbl)
	names(tbl_list) <- tbl_name
	class(tbl_list) <- "StepReg"
	return(tbl_list)
}

# functions for "best subset" algorithm:
getFitModel <- function(data, x_name_subset, y_name, weights, family = "binomial"){
	fm <- reformulate(x_name_subset, y_name)
	fit <- glm(fm, data = data, weights = weights, family = family)
	fit
}

getIncludeSubset <- function(data, single_set, metric, fit_reduced, x_name_subset, y_name, weights){
	if (length(include) != 0){
		fit <- getFitModel(data, x_name_subset, y_name, weights, include = include, family = "binomial")
		if (metric == "SL"){
			PIC <- anova(fit_reduced, fit, test = "Rao")[2,"Rao"]
		} else{
			PIC <- modelFitStat(metric, fit, "Likelihood")
		}
		single_set[1, 1:3] <- c(fit$rank, PIC, paste0(c(intercept, include), collapse = " "))
		return(single_set)
		# xCheck <- setdiff(xName,includeName)
	} else{
		return(NULL)
	}
}

getFinalSet <- function(data, x_check, single_set, y_name, include, weights, intercept, best_n = 1){
	final_result <- single_set
	for (nv in 1:length(x_check)){
		subset <- NULL
		comTable <- combn(x_check, nv)
		for (ncom in 1:ncol(comTable)){
			comVar <- c(intercept, include, comTable[, ncom])
			fit <- getFitModel(data, comVar, y_name, weights)
			if (metric == "SL"){
				PIC <- anova(fit_reduced, fit, test = "Rao")[2, "Rao"] 
			} else{
				PIC <- modelFitStat(metric, fit, "Likelihood")
			}
			single_set[1,1:3] <- c(fit$rank, PIC, paste0(comVar, collapse=" "))
			subset <- rbind(subset, single_set)
		}
		best_subset <- as.data.frame(subset)
		best_subset[,2] <- as.numeric(best_subset[, 2])
		if (metric == "SL"){
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
	final_result <- final_result[-1,]
	final_result
}

getXmodel <- function(include_subset, final_result){
	reg_pic <- rbind(include_subset, final_result)
	rownames(reg_pic) <- c(1:nrow(reg_pic))
	result$'Process of Selection' <- reg_pic
	reg_pic[, 2] %in% min(reg_pic[, 2])
	if (metric == "SL"){
		x_model <- unlist(strsplit(reg_pic[which.max(as.numeric(reg_pic[, 2])), 3]," "))
	} else{
		x_model <- unlist(strsplit(reg_pic[which.min(as.numeric(reg_pic[, 2])), 3]," "))
	}
	x_model
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
	
	x_name_remove_multicol <- setdiff(x_name,multico_x) # not used
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
		xCheck <- setdiff(xName,includeName)
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
