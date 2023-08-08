#' Stepwise helper functions
#' 
#' Extract information of parameters from stepwise regression.
#'
#' @return data.frame
#' 
#' @author Junhui Li, Kai Hu

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
	# create a new formula given explicit x, y, and intercept, bypassed the x being .
	formula_raw <- reformulate(c(intercept, x_name), y_name)
	if(type == 'linear'){
		## cannot perform multivariate multiple regression in glm() function
		#lm_raw <- glm(formula_raw,data = data, weights=weights, family="gaussian")
		model_raw <- lm(formula_raw, data = data, weights = weights)
	}else if(type == "logit"){
		model_raw <- glm(formula_raw, data = data, weights = weights, family = "binomial")
	}else if(type == 'cox'){
	  ## "method" is only used for cox regression
	  method <- match.arg(method)
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

getTestMethod <- function(data, model_raw, type, metric, n_y, test_method_linear, test_method_logit, test_method_cox){
	if(type == "linear"){
	  n_obs <- nrow(data)
	  # get sigma for BIC and CP
	  if(n_y == 1){
	    test_method <- "F"
	  }else{
	    test_method <- test_method_linear
	    if(any(c(metric) == c("BIC", "CP", "Rsq", "adjRsq"))){
	      stop("The 'metric' can not be 'BIC', 'CP', 'Rsq' or 'adjRsq' when using multivariate multiple regression!")
	    }
	  }
	  if((metric == "CP" | metric == 'BIC') & model_raw$rank >= n_obs){
	    stop("The 'metric' can not be 'CP' or 'BIC' when variable number is greater than the number of observation.")
	  }
	}else if(type == "logit"){
	  test_method <- test_method_logit
	}else if(type == "cox"){
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
getModelFitStat <- function(metric=c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SBC", "IC(3/2)", "IC(1)"), fit, type = c("linear","logit", "cox")){
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

getInitialSubSet <- function(data, type, metric, y_name, intercept, include, weights, test_method){
	# obtain the initial model information: if no include variable, return NULL, otherwise return a matrix containing columns of "NumberOfVariables", metric, and "VariablesInModel"
	# metric refers to PIC method, e.g. AIC, BIC, etc. for "logit" and "cox" type, if metric is "SL", the PIC is calculated differently
  initial_process_table <- NULL
  if (length(include) != 0){
    initial_process_table <- matrix(NA, 1, 3)
    colnames(initial_process_table) <- c("NumberOfVariables", metric, "VariablesInModel")
    x_fit <- getModel(data=data, type=type, x_name=c(intercept,include), y_name=y_name, weights=weights, intercept=intercept, method=test_method)

    if(metric == "SL"){
      if(type == "logit"){
        fit_reduce <- glm(reformulate(intercept, y_name), data = data, weights = weights, family = "binomial")
        f_pic_vec <- getAnovaStat(fit_reduced=fit_reduce,fit_full=x_fit,type=type,test_method="Rao")
        pic_set <- f_pic_vec[2]
      }else if(type == "cox"){
        pic_set <- x_fit$score
      }
    }else{
      pic_set <- getModelFitStat(metric,x_fit,type)
    }
    initial_process_table[1,1:3] <- c(as.numeric(intercept)+length(include),pic_set,paste(intercept,include,sep=" "))
  }
  return(initial_process_table)
}

getFinalSubSet <- function(data, type, metric, x_notin_model, initial_process_table, y_name, include, weights, intercept, best_n = Inf, test_method){
	process_table <- initial_process_table
	for (nv in 1:length(x_notin_model)){
		com_table <- as.data.frame(combn(x_notin_model, nv))
		n_test <- ncol(com_table)
		com_var <- apply(com_table,2,paste,collapse=" ")
		sub_process_table <- matrix(rep(c(nv+length(include)+as.numeric(intercept),NA),each=n_test), n_test, 2)
		com_var_df <- cbind(paste(intercept,include,sep=" "),data.frame(com_var))
		com_var_set <- apply(com_var_df,1,paste,collapse=" ")
		sub_process_table <- data.frame(sub_process_table,c(com_var_set))
		colnames(sub_process_table) <- c("NumberOfVariables", metric, "VariablesInModel")
		colnames(com_table) <- com_var_set
		x_test_list <- as.list(com_table)
		x_name_list <- lapply(x_test_list,function(x){c(intercept, include, x)})
		x_fit_list <- lapply(x_name_list,function(x){getModel(data=data, type=type, x_name=x, y_name=y_name, weights=weights, intercept=intercept, method=test_method)})
		
		if(metric == "SL"){
		  if(type == "logit"){
		    fit_reduce <- glm(reformulate(intercept, y_name), data = data, weights = weights, family = "binomial")
		    f_pic_vec <- sapply(x_fit_list,function(x){getAnovaStat(fit_reduced=fit_reduce,fit_full=x,type=type,test_method="Rao")})
		    pic_set <- f_pic_vec[2,]
		  }else if(type == "cox"){
		    #pic_set <- fit$score
		    pic_set <- sapply(x_fit_list,function(x){x$score})
		  }
		}else{
		  pic_set <- sapply(x_fit_list,function(x){getModelFitStat(metric,x,type)})
		}
		sub_process_table[,2] <- pic_set
		
		if (metric %in% c("SL", "Rsq", "adjRsq")){
		  # "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
		  decreasing = TRUE
		} else{
		  decreasing = FALSE
		}
		sub_process_table_sort <- sub_process_table[order(sub_process_table[, 2], decreasing = decreasing), ]
		if(nrow(sub_process_table_sort) < best_n){
		  best_n <- nrow(sub_process_table_sort)
		}
		process_table <- rbind(process_table,sub_process_table_sort[1:best_n, ])
	}
	return(process_table)
}

getXNameSelected <- function(process_table){
	if (metric %in% c("SL", "Rsq", "adjRsq")){
		# "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
		x_name_selected <- unlist(strsplit(process_table[which.max(as.numeric(process_table[, 2])), 3]," "))
	} else{
		x_name_selected <- unlist(strsplit(process_table[which.min(as.numeric(process_table[, 2])), 3]," "))
	}
  x_name_selected <- x_name_selected[!x_name_selected %in% ""]
	return(x_name_selected)
}

getSubsetWrapper <- function(data, type, metric, x_name, y_name, intercept, include, weights, best_n, test_method){
  # a wrapper to obtain x_name_selected
	## obtain initial model info
	initial_process_table <- getInitialSubSet(data, type, metric, y_name, intercept, include, weights, test_method)
	
	## obtain final model info
	x_notin_model <- setdiff(x_name, include)
	process_table <- getFinalSubSet(data, type, metric, x_notin_model, initial_process_table, y_name, include, weights, intercept, best_n, test_method)
	
	## add rownames to sort the variables in process_table when output
	rownames(process_table) <- c(1:nrow(process_table))
	return(process_table)
}

formatTable <- function(tbl, tbl_name = "Test"){
	tbl_list <- list(tbl)
	names(tbl_list) <- tbl_name
	class(tbl_list) <- "StepReg"
	return(tbl_list)
}

getTable1SummaryOfParameters <- function(data, type, x_name, y_name, merged_multico_x, 
																				 merged_include, strategy, metric, sle, sls, 
																				 test_method, tolerance, intercept){
	# generate: table1: Summary of Parameters
	table_1_summary_of_parameters <- data.frame(
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

getTable2TypeOfVariables <- function(model){
  class_var <- attr(model$terms,"dataClasses")
  table2_class_table <- as.data.frame(table(class_var))
  colnames(table2_class_table) <- c("class", "variable")
  for(i in names(table(class_var))){
    table2_class_table[names(table(class_var)) %in% i,2] <- paste0(names(class_var[class_var %in% i]),collapse=" ")
  }
  return(table2_class_table)
}

#note1: test_method_linear should be 'F' for univariate and c(“Pillai”, “Wilks”, “Hotelling-Lawley”, “Roy”) for multivariates
#
getAnovaStat <- function(fit_reduced, fit_full, type, test_method){
  if (type == "linear") {
    ptype <- 'Pr(>F)'
    if(test_method %in% c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")){
      stattype <- 'approx F'
    }else{
      stattype <- 'F'
    }
  } else if (type == "logit") {
    ptype <- 'Pr(>Chi)'
    if(test_method == "Rao"){
      stattype <- "Rao"
    }else{
      stattype <- "Deviance"
    }
  } else if (type == "cox") {
    # test is not used in cox regression
    test_method <- ""
    ptype <- 'Pr(>|Chi|)'
    stattype <- "Chisq"
  }
  anova_table <- anova(fit_reduced, fit_full, test = test_method)
  statistics <- anova_table[2,stattype]
  pic <- anova_table[2, ptype]
  return(c("statistics" = statistics, "pic" = pic))
}

## get pic based on model fit, it needs fit_reduced and fit_full for SL and only fit_formula for other metrics
## used in stepwise in step0: SL:0,1,inf  non-SL:
## fit_fm<-fit_intercept
getInitStepModelStat <- function(fit_intercept,fit_fm,type,strategy,metric,intercept,include,test_method){
  if(metric == "SL"){
    if(!is.null(include)){
      if(all(include %in% attr(fit_fm$terms,"term.labels")) & strategy != "backward"){
        f_pic_vec <- getAnovaStat(fit_intercept,fit_fm,type,test_method=test_method)
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

initialProcessTable <- function(metric){
  sub_init_process_table <- data.frame(Step=numeric(),
                               EnteredEffect=character(),
                               RemovedEffect=character(),
                               DF=numeric(),
                               NumberEffectIn=numeric(),
                               NumberParmsIn=numeric(),
                               metric=numeric())
  colnames(sub_init_process_table)[ncol(sub_init_process_table)] <- metric
  return(sub_init_process_table)
}

##getInitialStepwise -> getInitialModel
##getInitialSubset -> 
getInitialStepwise <- function(data,type,strategy,metric,weights,x_name,y_name,intercept,include,test_method){
  sub_init_process_table <- initialProcessTable(metric)
  process_table <- sub_init_process_table
  if(strategy == "backward"){
    add_or_remove <- "remove"
    #the order of variable in x_in_model will affect pic calculation.
    x_in_model <- c(setdiff(x_name,include))
    x_notin_model <- NULL
    fit_full <- getModel(data=data, type=type, x_name=c(include,x_in_model), y_name=y_name, weights=weights, intercept=intercept, method=test_method)
    pic <- getInitStepModelStat(fit_intercept=NULL,fit_fm=fit_full,type=type,strategy=strategy,metric=metric,intercept=intercept,include=include,test_method=test_method)
    num_eff_para_in <- getNumberEffect(fit=fit_full,type=type)
    process_table[1,] <- c(rep("",3),num_eff_para_in,pic)
  }else{
    add_or_remove <- "add"
    x_in_model <- NULL
    x_notin_model <- setdiff(x_name,include)
    ## for intercept
    fit_intercept <- getModel(data=data, type=type, x_name=NULL, y_name=y_name, weights=weights, intercept=intercept, method=test_method)
    pic <- getInitStepModelStat(fit_intercept=fit_intercept,fit_fm=fit_intercept,type=type,strategy=strategy,metric=metric,intercept=intercept,include=include,test_method=test_method)
    num_eff_para_in <- getNumberEffect(fit=fit_intercept,type=type)
    process_table[1,] <- c("",intercept,"",num_eff_para_in,pic)
    ## for include
    if(!is.null(include)){
      fit_include <- getModel(data=data, type=type, x_name=include, y_name=y_name, weights=weights, intercept=intercept, method=test_method)
      pic <- getInitStepModelStat(fit_intercept=fit_intercept,fit_fm=fit_include,type=type,strategy=strategy,metric=metric,intercept=intercept,include=include,test_method=test_method)
      num_eff_para_in <- getNumberEffect(fit=fit_include,type=type)
      sub_init_process_table[1,] <- c("",paste0(include,collapse=" "),"",anova(fit_include,fit_intercept)[2,'Df'],num_eff_para_in[-1],pic)
      process_table <- rbind(process_table,sub_init_process_table)
    }
  }
  process_table$Step <- 0
  return(list("add_or_remove"=add_or_remove,"x_in_model"=x_in_model,"x_notin_model"=x_notin_model,"process_table"=process_table))
}

getCandStepModel <- function(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include, test_method){
  fit_x_in_model <- getModel(data=data, type=type, x_name=c(include,x_in_model), y_name=y_name, weights=weights, intercept=intercept, method=test_method)
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
  x_fit_list <- lapply(x_name_list,function(x){getModel(data=data, type=type, x_name=c(include,x), y_name=y_name, weights=weights, intercept=intercept, method=test_method)})

  if(metric == "SL"){
    f_pic_vec <- sapply(x_fit_list,function(x){getAnovaStat(fit_reduced=x,fit_full=fit_x_in_model,type=type,test_method=test_method)})
    pic_set <- f_pic_vec[2,]
    f_set <- f_pic_vec[1,]
  }else{
    if(add_or_remove == "remove" & length(x_test) == 1 & intercept == "0"){
      pic_set <- Inf
      names(pic_set) <- x_test
    }else{
      pic_set <- sapply(x_fit_list,function(x){getModelFitStat(metric,x,type)})
    }
  }
  if(metric == "Rsq" | metric == "adjRsq" | (metric == "SL" & add_or_remove == "remove")){
    pic <- max(pic_set)
    minmax_var <- names(which.max(pic_set))
    best_candidate_model <- x_fit_list[[minmax_var]]
  }else{
    pic <- min(pic_set)
    minmax_var <- names(which.min(pic_set))
    best_candidate_model <- x_fit_list[[minmax_var]]
    if(sum(pic_set %in% pic) > 1 & metric == "SL"){
      Fvalue <- max(f_set)
      minmax_var <- names(which.max(f_set))
      best_candidate_model <- x_fit_list[[minmax_var]]
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

checkEnterOrRemove <- function(add_or_remove,best_candidate_model,type,metric,y_name,pic,sls,sle,process_table){
  if(metric == 'SL'){
    if(add_or_remove == "remove"){
      indicator <- pic > sls
    }else{
      indicator <- pic < sle
    }
  }else if(metric == 'Rsq' | metric == 'adjRsq'){
    indicator <- pic > as.numeric(process_table[nrow(process_table),7])
  }else{
    indicator <- pic <= as.numeric(process_table[nrow(process_table),7])
  }
  if(indicator == TRUE & type == "linear" & (metric != "Rsq"|metric != "adjRsq")){
    BREAK <- getGoodnessFit(best_candidate_model,y_name,metric)
  }else{
    BREAK <- FALSE
  }
  return(c("indicator"=indicator,"BREAK"=BREAK))
}

updateXinModel <- function(add_or_remove,indicator,best_candidate_model,type,metric,BREAK,pic,x_in_model,x_notin_model,process_table,minmax_var){
  sub_init_process_table <- initialProcessTable(metric)
  if(indicator == TRUE & BREAK == FALSE){
    if(add_or_remove == "add"){
      x_in_model <- append(x_in_model,minmax_var)
      x_notin_model <- setdiff(x_notin_model,minmax_var)
      sub_init_process_table[1,] <- c(as.numeric(process_table[nrow(process_table),1])+1,minmax_var,"",getNumberEffect(fit=best_candidate_model,type),pic)
    }else{
      x_notin_model <- append(x_notin_model,minmax_var)
      x_in_model <- setdiff(x_in_model,minmax_var)
      sub_init_process_table[1,] <- c(as.numeric(process_table[nrow(process_table),1])+1,"",minmax_var,getNumberEffect(fit=best_candidate_model,type),pic)
    }
    process_table <- rbind(process_table,sub_init_process_table)
    #process_table[nrow(process_table),1] <- as.numeric(process_table[1,nrow(process_table)-1]) + 1
  }else{
    BREAK <- TRUE
  }
  return(list("BREAK"=BREAK,"process_table"=process_table,"x_in_model"=x_in_model,"x_notin_model"=x_notin_model))
}

getFinalStepModel <- function(add_or_remove,data,type,strategy,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,process_table,test_method){
  while(TRUE){
    out_cand_stepwise <- getCandStepModel(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method)
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
    
    out_check <- checkEnterOrRemove(add_or_remove,best_candidate_model,type,metric,y_name,pic,sls,sle,process_table)
    indicator <- out_check["indicator"]
    BREAK <- out_check["BREAK"]
    if(BREAK == TRUE){
      break
    }
    
    out_updateX <- updateXinModel(add_or_remove,indicator,best_candidate_model,type,metric,BREAK,pic,x_in_model,x_notin_model,process_table,minmax_var)
    x_in_model <- out_updateX$x_in_model
    x_notin_model <- out_updateX$x_notin_model
    process_table <- out_updateX$process_table
    
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
  
  process_table$DF <- abs(as.numeric(process_table$DF))
  process_table$DF[is.na(process_table$DF)] <- ""
  if(type == "cox" && strategy != "backward"){
    process_table <- process_table[-1,]
  }
  return(list("process_table"=process_table,"x_in_model"=x_in_model))
}

getStepwiseWrapper <- function(data,type=type,strategy,metric,weights,x_name,y_name,intercept,include,test_method){
  out_init_stepwise <- getInitialStepwise(data,type=type,strategy,metric,weights,x_name,y_name,intercept,include,test_method=test_method)
  add_or_remove <- out_init_stepwise$add_or_remove
  x_in_model <- out_init_stepwise$x_in_model
  x_notin_model <- out_init_stepwise$x_notin_model
  process_table <- out_init_stepwise$process_table
  
  ## get final stepwise model
  out_final_stepwise <- getFinalStepModel(add_or_remove,data,type=type,strategy,metric,weights,y_name,x_in_model,x_notin_model,intercept, include, process_table, test_method)
  return(out_final_stepwise)
}

getTBLFianlVariable <- function(all_x_in_model){
  variables <- as.data.frame(t(data.frame(all_x_in_model)))
  colnames(variables) <- paste0("variable",1:ncol(variables))
  rownames(variables) <- c("x in model")
  #table4 <- formatTable(variables,tbl_name = "Table 4. Selected Varaibles")
  table4 <- variables
  return(table4)
}

getTBLCoefModel <- function(type,intercept,include,x_in_model,y_name,n_y,data,test_method){
  if(is.null(c(include,x_in_model))){
    summary_model <- NULL
  }else{
    summary_model <- summary(getModel(data, type, x_name=c(x_in_model), y_name, weights, intercept, method=test_method))
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
  table5 <- summary_model_list
  #table5 <- formatTable(summary_model_list, tbl_name = "Table 5. Summary of Model for")
  return(table5)
}