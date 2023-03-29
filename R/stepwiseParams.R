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
##----------------------
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
	term_form <- terms(formula, data = input_data)
	vars <- as.character(attr(term_form, "variables"))[-1]
	y_name <- vars[attr(term_form, "response")]
	x_name <- attr(term_form, "term.labels")
	
	if(type != 'cox'){
	  if(attr(term_form, "intercept") == 0){
	    intercept <- "0"
	  }else{
	    intercept <- "1"
	  }
	  intercept_name <- intercept
	}else{
	  intercept <- NULL
	  intercept_name <- "NULL"
	}


	if(is.character(include)){
		include_name <- include
		merge_include_name <- paste0(include_name,collapse=" ")
	}else if(is.null(include)){
		include_name <- NULL
		merge_include_name <- "NULL"
	}
	
	if(!is.null(weights)){
		if(length(weights) == nrow(input_data)){
			weight_data <- input_data*sqrt(weights)
		}else{
			stop("Variable length is different ('(weights)')")
		}
	}else{
		weight_data <- input_data
	}

	formula_raw <- reformulate(c(intercept,x_name),y_name)
	if(type == 'linear'){
	  ## cannot perform multivariate multiple regression in glm() function
	  #lm_raw <- glm(formula_raw,data=input_data, weights=weights, family="gaussian")
	  lm_raw <- lm(formula_raw,data=input_data,weights = weights)
	}else if(type == "logit"){
	  lm_raw <- glm(formula_raw,data=input_data, weights=weights, family="binomial")
	}else if(type == 'cox'){
	  lm_raw <- survival::coxph(formula_raw,data=input_data, weights=weights,method=test_method_cox)
	}
	
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
	#removeColinear <- function(df_in,col_name){
	  x_matrix <- as.matrix(input_data[,x_name])
	  qrx_list <- qr(x_matrix,tol=tolerance)
	  rank0 <- qrx_list$rank
	  pivot0 <- qrx_list$pivot
	  if(rank0 < length(pivot0)){
	    multico_x <- colnames(qrx_list$qr)[pivot0[(rank0+1):length(pivot0)]]
	    multicol_merged_name <- paste0(multico_x,collapse=" ")
	  }else{
	    multico_x <- NULL
	    multicol_merged_name <- "NULL"
	  }
	  #return(list(multico_x,multicol_merged_name))
	#}
	
	x_name_remove_multicol <- setdiff(x_name,multico_x)
	if(type == 'linear'){
	  y_df <- as.matrix(lm_raw$model[,y_name])
	  n_y <- ncol(y_df)
	  n_obs <- nrow(input_data)
	  # get sigma for BIC and CP
	  if(n_y==1){
	    test_method <- "F"
	  }else{
	    test_method <- test_method_linear
	    if(any(c(metric)==c("BIC","CP","Rsq","adjRsq"))){
	      stop("Can't specify 'BIC','CP','Rsq' or 'adjRsq' when using multivariate multiple regression")
	    }
	  }
	  if((metric=="CP" | metric=='BIC') & lm_raw$rank >= n_obs){
	    stop("'metric' can't specify 'CP' or 'BIC' when variable number is greater than number of observation")
	  }
	}else if(type == 'logit'){
	  test_method <- test_method_logit
	}else if(type == 'cox'){
	  test_method <- test_method_cox
	}

	result <- list()
	regression_information <- matrix(NA,10,1)
	regression_information <- cbind(regression_information,matrix(c(y_name,merge_include_name,strategy,metric,sle,sls,test_method,tolerance,multicol_merged_name,intercept_name),10,1))
	regression_information <- data.frame(regression_information)
	colnames(regression_information) <- c("Paramters","Value")
	regression_information[,1] <- c("Response Variable",
	                "Included Variable",
	                "Strategy Method",
	                "Metric Criterion",
	                "Entry Significance Level(sle)",
	                "Stay Significance Level(sls)",
	                "Variable significance test",
	                "Tolerance of Multicollinearity",
	                "Multicollinearity Terms",
	                "Intercept")
	
	if(metric=="SL"){
	  if(strategy=="forward"){
	    regression_information <- regression_information[-6,]
	  }else if(strategy=="backward"){
	    regression_information <- regression_information[-5,]
	  }else if(strategy=="subset"){
	    regression_information <- regression_information[-c(5:6),]
	  }
	}else{
	  regression_information <- regression_information[-c(5:6),]
	}
	rownames(regression_information) <- 1:nrow(regression_information)
	if(type == 'cox'){
	  regression_information <- regression_information[-nrow(regression_information),]
	}
	result$'Summary of Stepwise Regression Parameters' <- regression_information
	result$'Variables Type' <- class_table
	return(result)
}
