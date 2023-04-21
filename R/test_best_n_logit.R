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
best_n <- 5

x_name <- getXname(formula, input_data)
y_name <- getYname(formula, input_data)
intercept <- getIntercept(formula, input_data, type) 

getFitModel <- function(data, type, x_name_subset, y_name, weights, test_method_cox = NULL){
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

getInitialSet <- function(data, type, metric, y_name, intercept, include, weights, test_method_cox = NULL){
	# obtain the initial model information: if no include variable, return NULL, otherwise return a matrix containing columns of "NumberOfVariables", metric, and "VariablesInModel"
	if (length(include) != 0){
		single_set <- matrix(NA, 1, 3)
		colnames(single_set) <- c("NumberOfVariables", metric, "VariablesInModel")
		if (type == "linear"){
			fit <- getFitModel(data, type, c(intercept, include), y_name, weights)
			PIC <- modelFitStat(metric, fit, "LeastSquare")
			single_set <- c(length(attr(fit$terms,"term.labels")), PIC, paste(c(intercept, include), collapse = " "))
		} else if (type == "logit"){
			fit <- getFitModel(data, type, c(intercept, include), y_name, weights)
			if (metric == "SL"){
				fit_reduced <- getFitModel(data, type, c(intercept), y_name, weights)
				PIC <- anova(fit_reduced, fit, test = "Rao")[2, "Rao"]
			} else{
				PIC <- modelFitStat(metric, fit, "Likelihood")
			}
			single_set[1, 1:3] <- c(fit$rank, PIC, paste0(c(intercept, include), collapse = " "))
		} else if (type == "cox"){
			fit <- getFitModel(data, type, c(include), y_name, weights, method = test_method_cox)
			if (metric == "SL"){
				PIC <- fit$score
			} else{
				PIC <- modelFitStat(metric, fit, "Likelihood", TRUE)
			}
			single_set[1, 1:3] <- c(length(attr(fit$terms, "term.labels")), PIC, paste0(c(include), collapse = " "))
		}
		return(single_set)
	} else{
		return(NULL)
	}
}

getFinalSet <- function(data, type, metric, x_check, initial_set, y_name, include, weights, intercept, best_n = 1, test_method_cox = NULL){
	final_result <- initial_set
	single_set <- matrix(NA, 1, 3)
	colnames(single_set) <- c("NumberOfVariables", metric, "VariablesInModel")
	subset <- NULL
	comTable <- combn(x_check, nv)
	
	for (nv in 1:length(x_check)){
		for (ncom in 1:ncol(comTable)){
			if (type == "linear"){
				comVar <- c(intercept, include, comTable[, ncom])
				fit <- getFitModel(data, type, comVar, y_name, weights)
				PIC <- modelFitStat(metric, fit, "LeastSquare")
				single_set[1, 1:3] <- c(length(attr(fit$terms,"term.labels")), PIC, paste(comVar, collapse = " "))
				subset <- rbind(subset, single_set)
			} else if (type == "logit"){
				comVar <- c(intercept, include, comTable[, ncom])
				fit <- getFitModel(data, type, comVar, y_name, weights)
				if (metric == "SL"){
					PIC <- anova(fit_reduced, fit, test = "Rao")[2, "Rao"] 
				} else{
					PIC <- modelFitStat(metric, fit, "Likelihood")
				}
				single_set[1, 1:3] <- c(fit$rank, PIC, paste0(comVar, collapse = " "))
				subset <- rbind(subset, single_set)
			} else if (type == "cox"){
				comVar <- c(include, comTable[, ncom])
				fit <- getFitModel(data, type, comVar, y_name, weights, test_method_cox = test_method_cox)
				if (metric == "SL"){
					PIC <- fit$score
				}else{
					PIC <- modelFitStat(metric, fit, "Likelihood", TRUE)
				}
				single_set[1, 1:3] <- c(attr(logLik(fit), "df"), PIC, paste0(comVar, collapse = " "))
				subset <- rbind(subset, single_set)
			}
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

getXNameSelected <- function(result, final_set){
	# result is a list containing logging info passed in from upstream
	rownames(final_set) <- c(1:nrow(final_set))
	result$'Process of Selection' <- final_set
	final_set[, 2] %in% min(final_set[, 2])
	if (metric %in% c("SL", "Rsq", "adjRsq"){
		# "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
		x_name_selected <- unlist(strsplit(final_set[which.max(as.numeric(final_set[, 2])), 3]," "))
	} else{
		x_name_selected <- unlist(strsplit(final_set[which.min(as.numeric(final_set[, 2])), 3]," "))
	}
	x_name_selected
}

getXNameSelectedWrapper <- function(input_data, type, metric, y_name, intercept, include, weights, test_method_cox = NULL, result){
	# a wrapper to obtain x_name_selected
	## obtain initial model info
	initial_set <- getInitialSet(input_data, type, metric, y_name, intercept, include, weights, test_method_cox = NULL)
	
	## obtain final model info
	x_check <- setdiff(x_name, include)
	final_set <- getFinalSet(input_data, type, metric, x_check, initial_set, y_name, include, weights, intercept, best_n = best_n, test_method_cox = NULL)
	
	## obtain x_name_selected (drop-in replacement for x_model/xModel)
	x_name_selected <- getXNameSelected(result, final_set)
	return(list(result, x_name_selected))
}

# 99 - 161: stepwiseLogit
if(strategy=="subset"){
	tem_res <- getXNameSelectedWrapper(input_data, type, metric, x_name, y_name, intercept, include, weights, result, test_method_cox = NULL, best_n = 1)
	result <- tem_res[[1]]
	x_name_selected <- tem_res[[2]]
}

# 91 - 150: stepwiseCox:
if(strategy=="subset"){
	tem_res <- getXNameSelectedWrapper(input_data, type, metric, x_name, y_name, intercept, include, weights, result, test_method_cox = NULL, best_n = 1)
	result <- tem_res[[1]]
	x_name_selected <- tem_res[[2]]
}

## 115-161: stepwiseLinear:
if(strategy=="subset"){
	tem_res <- getXNameSelectedWrapper(input_data, type, metric, x_name, y_name, intercept, include, weights, result, test_method_cox = NULL, best_n = 1)
	result <- tem_res[[1]]
	x_name_selected <- tem_res[[2]]
}

