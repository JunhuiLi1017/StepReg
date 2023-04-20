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


# 99 - 161: stepwiseLogit
if(strategy=="subset"){ #subset
	# bestSubSet <- NULL
	# singSet <- matrix(NA,1,3)
	# colnames(singSet) <- c("NumberOfVariables",metric,"VariablesInModel")
	# finalResult <- singSet
	# fmReduce <- reformulate(c(intercept), y_name)
	# fitReduce <- glm(fmReduce,data=input_data, weights=weights, family="binomial")
	single_set <- matrix(NA, 1, 3)
	colnames(single_set) <- c("NumberOfVariables", metric, "VariablesInModel")
	fit_reduced <- getFitModel(input_data, c(intercept), y_name, weights)
	include_subset <- getIncludeSubset(input_data, single_set, metric, fit_reduced, c(intercept, include), y_name, weights)
	x_check <- setdiff(x_name, include)
	
	# if(length(includeName)!=0){
	# 	fm <- reformulate(c(intercept,includeName), yName)
	# 	#fit <- multinom(fm, data = YXdata, weights = weights)
	# 	fit <- glm(fm,data=data, weights=weights, family="binomial")
	# 	if(metric=="SL"){
	# 		PIC <- anova(fitReduce,fit,test="Rao")[2,"Rao"]
	# 	}else{
	# 		PIC <- modelFitStat(metric,fit,"Likelihood")
	# 	}
	# 	singSet[1,1:3] <- c(fit$rank,PIC,paste0(c(intercept,includeName),collapse=" "))
	# 	includeSubSet <- singSet
	# 	xCheck <- setdiff(xName,includeName)
	# }else{
	# 	includeSubSet <- NULL
	# 	x_check <- x_name
	# }
	
	# obtain final_set:
	final_result <- getFinalSet(input_data, x_check, single_set, y_name, include, weights, intercept, best_n = best_n)
	
	# for(nv in 1:length(x_check)){
	# 	# nv <- 1
	# 	subSet <- NULL
	# 	comTable <- combn(x_check,nv)
	# 	for(ncom in 1:ncol(comTable)){
	# 		comVar <- c(intercept,include,comTable[,ncom])
	# 		fm <- reformulate(comVar, y_name)
	# 		fit <- glm(fm,data = input_data,weights=weights,family="binomial")
	# 		if(metric=="SL"){
	# 			PIC <- anova(fitReduce,fit, test="Rao")[2,"Rao"] 
	# 		}else{
	# 			PIC <- modelFitStat(metric,fit,"Likelihood")
	# 		}
	# 		singSet[1,1:3] <- c(fit$rank,PIC,paste0(comVar,collapse=" "))
	# 		subSet <- rbind(subSet,singSet)
	# 	}
	# 	bestSubSet <- as.data.frame(subSet)
	# 	bestSubSet[,2] <- as.numeric(bestSubSet[,2])
	# 	if(metric=="SL"){
	# 		subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
	# 	}else{
	# 		subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
	# 	}
	# 	
	# 	if(nrow(subSet) < best_n){
	# 		best_n <- nrow(subSet)
	# 	}
	# 	
	# 	finalResult <- rbind(finalResult,subResultSort[1:best_n,])
	# }
	# 
	# finalResult <- finalResult[-1,]
	
	
	# obtain x_model:
	x_model <- getXmodel(include_subset, final_result)
	# 
	# RegPIC <- rbind(includeSubSet,finalResult)
	# rownames(RegPIC) <- c(1:nrow(RegPIC))
	# result$'Process of Selection' <- RegPIC
	# RegPIC[,2] %in% min(RegPIC[,2])
	# if(metric=="SL"){
	# 	xModel <- unlist(strsplit(RegPIC[which.max(as.numeric(RegPIC[,2])),3]," "))
	# }else{
	# 	xModel <- unlist(strsplit(RegPIC[which.min(as.numeric(RegPIC[,2])),3]," "))
	# }
	
	
}