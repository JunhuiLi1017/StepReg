#' Stepwise Linear Model Regression
#'
#' Stepwise linear regression analysis selects model based on information criteria and F or approximate F test with 'forward', 'backward', 'bidirection' and 'subset' model strategy method.

stepwiseLinear <- function(
                     formula,
                     data,
                     include = NULL,
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SL","SBC"),
                     sle = 0.15,
                     sls = 0.15,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     weights = NULL,
                     best_n = Inf){
	x_name <- getXname(formula, data)
	y_name <- getYname(formula, data)
	intercept <- getIntercept(formula, data, type = "linear") # char type
	merged_include <- getMergedInclude(include)
	model_raw <- getModel(input_data, type, method, x_name, y_name, weights, intercept)
	multico_x <- getMulticolX(data, x_name, tolerance)
	merged_multico_x <- paste0(multico_x, sep = " ")

	x_name_remove_multicol <- setdiff(x_name, multico_x)

  ## extract response, independent variable and intercept
  # termForm <- terms(formula, data = data)
  # vars <- as.character(attr(termForm, "variables"))[-1]
  # yName <- vars[attr(termForm, "response")]
  # xName <- attr(termForm,"term.labels")
  #
  # if(attr(termForm, "intercept") == 0){
  #   intercept <- "0"
  # }else{
  #   intercept <- "1"
  # }
  # if(is.character(include)){
  #   includeName <- include
  #   mergeIncName <- paste0(includeName,collapse=" ")
  # }else if(is.null(include)){
  #   includeName <- NULL
  #   mergeIncName <- "NULL"
  # }
  # if(!is.null(weights)){
  #   if(length(weights)==nrow(data)){
  #   	weightData <- data*sqrt(weights)
  #   }else{
  #     stop("Variable length is different ('(weights)')")
  #   }
  # }else{
  #   weightData <- data
  # }

  # lmFull <- lm(formula, data = weightData)

	# table2:
  allVarClass <- attr(lmFull$terms,"dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class", "variable")
  for(i in names(table(allVarClass))){
    classTable[names(table(allVarClass)) %in% i,2] <- paste0(names(allVarClass[allVarClass %in% i]),collapse=" ")
  }
  result$'Variables Type' <- classTable

  # ## detect multicollinearity
  # if(any(allVarClass=="factor")){
  #   factVar <- names(which(allVarClass=="factor"))
  #   for(i in factVar){
  #     weightData[,i] <- as.factor(as.numeric(weightData[,i]))
  #   }
  # }


  # xMatrix <- as.matrix(weightData[,xName])
  # qrXList <- qr(xMatrix,tol=1e-7)
  # rank0 <- qrXList$rank
  # pivot0 <- qrXList$pivot
  # if(rank0 < length(pivot0)){
  #   mulcolX <- colnames(qrXList$qr)[pivot0[(rank0+1):length(pivot0)]]
  #   mulcolMergeName <- paste0(mulcolX,collapse=" ")
  # }else{
  #   mulcolX <- NULL
  #   mulcolMergeName <- "NULL"
  # }

  # x_name_remove_multicol <- setdiff(x_name, multico_x)
  xName <- setdiff(xName,mulcolX)
  Y <- as.matrix(lmFull$model[,yName])
  nY <- ncol(Y)
  nObs <- nrow(data)
  # get sigma for BIC and CP
  if(nY==1){
    approxF <- "F"
  }else{
    approxF <- test_method_linear
    if(any(c(metric)==c("BIC","CP","Rsq","adjRsq"))){
      stop("Can't specify 'BIC','CP','Rsq' or 'adjRsq' when using multivariate multiple regression")
    }
  }
  if((metric=="CP" | metric=='BIC') & lmFull$rank >= nObs){
    stop("'metric' can't specify 'CP' or 'BIC' when variable number is greater than number of observation")
  }
  result <- list()
  ModInf <- matrix(NA,9,1)
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,strategy,metric,sle,sls,approxF,mulcolMergeName,intercept),9,1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters","Value")
  ModInf[,1] <- c("Response Variable",
                  "Included Variable",
                  "Strategy Method",
                  "Metric Criterion",
                  "Entry Significance Level(sle)",
                  "Stay Significance Level(sls)",
                  "Variable significance test",
                  "Multicollinearity Terms",
                  "Intercept")
  if(metric=="SL"){
    if(strategy=="forward"){
      ModInf <- ModInf[-6,]
    }else if(strategy=="backward"){
      ModInf <- ModInf[-5,]
    }else if(strategy=="subset"){
      ModInf <- ModInf[-c(5:6),]
    }
  }else{
    ModInf <- ModInf[-c(5:6),]
  }
  rownames(ModInf) <- 1:nrow(ModInf)
  result$'Summary of Parameters' <- ModInf
  message("test here")
  print(result$'Summary of Parameters')

  if(strategy=="subset"){
  	final_set <- getFinalSetWrapper(input_data, type, metric, x_name, y_name, intercept, include, weights, best_n)
  	result$'Process of Selection' <- final_set

  	## obtain x_name_selected (drop-in replacement for x_model/xModel)
  	x_name_selected <- getXNameSelected(final_set)

    # tempresult <- matrix(NA,1,4)
    # colnames(tempresult) <- c("NoVariable","RankModel",metric,"VariablesEnteredinModel")
    # finalResult <- tempresult
    # if(!is.null(includeName)){
    #   lmIncForm <- reformulate(c(intercept,includeName), yName)
    #   lmInc <- lm(lmIncForm,data=weightData)
    #   tempresult[1,c(1:4)] <- c(length(attr(lmInc$terms,"term.labels")),lmInc$rank,modelFitStat(metric,lmInc,"LeastSquare"),paste(c(intercept,includeName),collapse=" "))
    #   finalResult <- rbind(finalResult,tempresult)
    #   checkX <- xName[!xName %in% includeName]
    # }else{
    #   checkX <- xName
    # }
    # for(nv in 1:length(checkX)){
    #   comTable <- combn(length(checkX),nv)
    #   subSet <- NULL
    #   for(ncom in 1:ncol(comTable)){
    #     comVar <- c(intercept,includeName,checkX[comTable[,ncom]])
    #     tempFormula <- reformulate(comVar, yName)
    #     lmresult <- lm(tempFormula,data=weightData)
    #     tempresult[1,1:4] <- c(length(attr(lmresult$terms,"term.labels")),lmresult$rank,modelFitStat(metric,lmresult,"LeastSquare"),paste(comVar,collapse=" "))
    #     subSet <- rbind(subSet,tempresult)
    #   }
    #
    #   if(nrow(subSet) < best_n){
    #   	best_n <- nrow(subSet)
    #   }
    #
    #   bestSubSet <- as.data.frame(subSet)
    #   bestSubSet[,2] <- as.numeric(bestSubSet[,2])
    #   if(metric=="Rsq" | metric=="adjRsq"){
    #     subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
    #   }else{
    #     subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
    #   }
    #   finalResult <- rbind(finalResult,subResultSort[1:best_n,])
    # }
    # finalResult <- finalResult[-1,]
    # rownames(finalResult) <- 1:nrow(finalResult)
    # result$'Process of Selection' <- finalResult
    # if(metric=="Rsq" | metric=="adjRsq"){
    #   xModel <- unlist(strsplit(finalResult[which.max(as.numeric(finalResult[,3])),4]," "))
    # }else{
    #   xModel <- unlist(strsplit(finalResult[which.min(as.numeric(finalResult[,3])),4]," "))
    # }
  }else{
    subBestPoint <- data.frame(Step=numeric(),
                               EnteredEffect=character(),
                               RemovedEffect=character(),
                               DF=numeric(),
                               NumberEffectIn=numeric(),
                               NumberParmsIn=numeric(),
                               metric=numeric())
    colnames(subBestPoint)[7] <- metric
    bestPoint <- subBestPoint
    if(strategy == "backward"){
      addIdx <- FALSE
      xModel <- c(intercept,includeName,setdiff(xName,includeName))
      xResidual <- NULL
      if (metric == 'SL') {
        PIC <- 1
      }else{
        PIC <- modelFitStat(metric,lmFull,"LeastSquare")
      }
      bestPoint[1,-1] <- c("","","",length(attr(lmFull$terms,"term.labels")),lmFull$rank,PIC)
    }else{
      addIdx <- TRUE
      xModel <- c(intercept,includeName)
      xResidual <- setdiff(xName,includeName)
      fmInt <- reformulate(intercept, yName)
      fitInt <- lm(fmInt,data=weightData)
      if(metric == 'SL') {
        PIC <- 1
      }else{
        if(intercept=='1'){
          PIC <- modelFitStat(metric,fitInt,"LeastSquare")
        }else{
          if(metric %in% c("Rsq","adjRsq")){
            PIC <- 0
          }else{
            PIC <- Inf
          }
        }
      }
      bestPoint[1,] <- c(0,intercept,"",fitInt$rank,length(attr(fitInt$terms,"term.labels")),fitInt$rank,PIC)
      if(!is.null(includeName)){
        fmInc <- reformulate(c(intercept,includeName),yName)
        fitInc <- lm(fmInc,data=weightData)
        if(metric == 'SL') {
          PIC <- anova(fitInc,fitInt,test=approxF)[2,'Pr(>F)']
        }else{
          PIC <- modelFitStat(metric,fitInc,"LeastSquare")
        }
        subBestPoint[1,] <- c(0,mergeIncName,"",anova(fitInc,fitInt,test=approxF)[2,'Df'],length(attr(fitInt$terms,"term.labels")),fitInc$rank,PIC)
        bestPoint <- rbind(bestPoint,subBestPoint)
      }
    }
    while(TRUE){
      fm0 <- reformulate(xModel,yName)
      lmAlt <- lm(fm0,data=weightData)
      if(addIdx==TRUE){
        xCheck <- xResidual
        if(length(xCheck)==0){
          break
        }
        xCheckList <- as.list(xCheck)
        names(xCheckList) <- xCheck
        fmX <- lapply(xCheckList, function(x){reformulate(c(xModel,x),yName)})
      }else{
        xCheck <- setdiff(xModel,c(intercept,includeName))
        if(length(xCheck)==0){
          break
        }
        xCheckList <- as.list(xCheck)
        names(xCheckList) <- xCheck
        fmX <- lapply(xCheckList,function(x){reformulate(setdiff(xModel,x),yName)})
      }
      fitX <- lapply(fmX,function(x){lm(x,data=weightData)})
      if(metric=="SL"){
        PICset <- sapply(fitX,function(x){anova(x,lmAlt,test=approxF)[2,'Pr(>F)']})
        Fset <- sapply(fitX,function(x){anova(x,lmAlt,test=approxF)[2,'F']})
      }else{
        if(addIdx==FALSE & length(xCheck)==1 & intercept=="0"){
          PICset <- Inf
          names(PICset) <- xCheck
        }else{
          PICset <- sapply(fitX,function(x){modelFitStat(metric,x,"LeastSquare")})
        }
      }
      if(metric=="Rsq" | metric=="adjRsq" | (metric=="SL" & addIdx==FALSE)){
        PIC <- max(PICset)
        minmaxVar <- names(which.max(PICset))
        bestLm <- fitX[[minmaxVar]]
      }else{
        PIC <- min(PICset)
        minmaxVar <- names(which.min(PICset))
        bestLm <- fitX[[minmaxVar]]
        if(sum(PICset %in% PIC)>1 & metric=="SL"){
          Fvalue <- max(Fset)
          minmaxVar <- names(which.max(Fset))
          bestLm <- fitX[[minmaxVar]]
          PIC <- PICset[minmaxVar]
        }
      }
      if(bestLm$rank==lmAlt$rank & addIdx==TRUE){
        break
      }else{
        if(metric=='SL'){
          if(addIdx==FALSE){
            indicator <- PIC > sls
          }else{
            indicator <- PIC < sle
          }
        }else if(metric=='Rsq' | metric=='adjRsq'){
          indicator <- PIC > as.numeric(bestPoint[nrow(bestPoint),7])
        }else{
          indicator <- PIC <= as.numeric(bestPoint[nrow(bestPoint),7])
        }
        if(indicator==TRUE){
          #goodness of fit
          smr <- summary(bestLm)
          if(nY==1){
            f <- smr$fstatistic
            if(is.nan(f[1])){
              pval <- NaN
            }else{
              pval <- pf(f[1],f[2],f[3],lower.tail=F)
            }
          }else{
            for(ny in 1:nY){
              f <- smr[[ny]]$fstatistic
              if(is.nan(f[1])){
                pval <- NaN
              }else{
                pval <- pf(f[1],f[2],f[3],lower.tail=F)
              }
            }
          }
          if(is.nan(pval)==TRUE & (metric!='Rsq' | metric!='adjRsq')){
            break
          }
          if(addIdx==TRUE){
            xModel <- append(xModel,minmaxVar)
            xResidual <- setdiff(xResidual,minmaxVar)
            subBestPoint[1,] <- c(as.numeric(bestPoint[nrow(bestPoint),1])+1,minmaxVar,"",anova(lmAlt,bestLm,test=approxF)[2,'Df'],length(attr(bestLm$terms,"term.labels")),bestLm$rank,PIC)
          }else{
            xResidual <- append(xResidual,minmaxVar)
            xModel <- setdiff(xModel,minmaxVar)
            subBestPoint[1,] <- c(as.numeric(bestPoint[nrow(bestPoint),1])+1,"",minmaxVar,anova(bestLm,lmAlt,test=approxF)[2,'Df'],length(attr(bestLm$terms,"term.labels")),bestLm$rank,PIC)
          }
          bestPoint <- rbind(bestPoint,subBestPoint)

          if(strategy == 'bidirection'){
            if(addIdx==FALSE){
              next
            }else{
              addIdx <- FALSE
              next
            }
          }else{
            next
          }
        }else{
          if(strategy == 'bidirection' & addIdx==FALSE) {
            addIdx <- TRUE
            next
          }else{
            break
          }
        }
      }
    }#while
    bestPoint$DF <- abs(as.numeric(bestPoint$DF))
    bestPoint$DF[is.na(bestPoint$DF)] <- ""
    result$'Process of Selection' <- bestPoint
  }
  if(is.null(xModel)){
    parEst <- NULL
  }else{
    parEst <- summary(lm(reformulate(xModel,yName),data=weightData))
    parEstList <- list()
    if(nY>1){
      for(i in names(parEst)){
        subParEst <- parEst[[i]]$coefficients
        subParEst <- data.frame(rownames(subParEst),subParEst)
        colnames(subParEst) <- c("Variable","Estimate","StdError","t.value","P.value")
        parEstList[i] <- list(subParEst)
      }
    }else{
      subParEst <- parEst$coefficients
      subParEst <- data.frame(rownames(subParEst),subParEst)
      colnames(subParEst) <- c("Variable","Estimate","StdError","t.value","P.value")
      parEstList <- list(subParEst)
      names(parEstList) <- yName
    }
  }
  variables <- as.data.frame(t(data.frame(xModel)))
  colnames(variables) <- paste0("variables",1:length(xModel))
  result$'Selected Varaibles' <- variables
  result$'Coefficients of the Selected Variables' <- parEstList
  class(result) <- c("StepReg","list")
  return(result)
}
