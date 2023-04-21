#' Stepwise Cox Proportional Hazards Regression
#' 
#' Stepwise Cox regression analysis selects model based on information criteria and significant test with 'forward', 'backward', 'bidirection' and 'subset' variable selection method.

stepwiseCox <- function(formula,
                        data,
                        include=NULL,
                        strategy=c("forward","backward","bidirection","subset"),
                        metric=c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)"),
                        sle=0.15,
                        sls=0.15,
                        test_method_cox=c("efron","breslow","exact"), 
                        weights=NULL,
                        best_n=NULL){
  strategy <- match.arg(strategy)
  metric <- match.arg(metric)
  test_method_cox <- match.arg(test_method_cox)
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula,data=data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm,"term.labels")
  if(is.character(include)){
    if(!all(include %in% xName)){
      stop("variable in include is not included formula or dataset")
    }else{
      includeName <- include
      mergeIncName <- paste0(includeName,collapse=" ")
    }
  }else if(is.null(include)){
    includeName <- include
    mergeIncName <- "NULL"
  }else{
    stop("include should be character vector indicating variable to be included in all models")
  }
  fmFull <- reformulate(c(xName),yName)
  fitFull <- survival::coxph(fmFull,data=data, weights=weights,method=test_method_cox)
  allVarClass <- attr(fitFull$terms,"dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class","variable")
  for(i in names(table(allVarClass))){
    classTable[names(table(allVarClass)) %in% i,2] <- paste0(names(allVarClass[allVarClass %in% i]),collapse=" ")
  }
  ## detect multicollinearity
  if(any(allVarClass=="factor")){
    factVar <- names(which(allVarClass=="factor"))
    for(i in factVar){
      data[,i] <- as.factor(as.numeric(data[,i]))
    }
  }
  xMatrix <- as.matrix(data[,xName])
  qrXList <- qr(xMatrix,tol=1e-7)
  rank0 <- qrXList$rank
  pivot0 <- qrXList$pivot
  if(rank0 < length(pivot0)){
    mulcolX <- colnames(qrXList$qr)[pivot0[(rank0+1):length(pivot0)]]
    mulcolMergeName <- paste0(mulcolX,collapse=" ")
  }else{
    mulcolX <- NULL
    mulcolMergeName <- "NULL"
  }
  xName <- setdiff(xName,mulcolX)
  n <- nrow(data)
  result <- list()
  ModInf <- matrix(NA,8,1)
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,strategy,metric,sle,sls,test_method_cox,mulcolMergeName),8,1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters","Value")
  ModInf[,1] <- c("Response Variable",
                  "Included Variable",
                  "Selection Method",
                  "Select Criterion",
                  "Entry Significance Level(sle)",
                  "Stay Significance Level(sls)",
                  "Method",
                  "Multicollinearity Terms")
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
  result$'Variables Type' <- classTable
  if(strategy=="subset"){ #subset
  	tem_res <- getXNameSelectedWrapper(input_data, type, metric, x_name, y_name, intercept, include, weights, result, test_method_cox = NULL, best_n = 1)
  	result <- tem_res[[1]]
  	x_name_selected <- tem_res[[2]]
  	
    # bestSubSet <- NULL
    # singSet <- matrix(NA,1,3)
    # colnames(singSet) <- c("NumberOfVariables",metric,"VariablesInModel")
    # finalResult <- singSet
    # if(length(includeName)!=0){
    #   fm <- reformulate(c(includeName), yName)
    #   fit <- survival::coxph(fm,data=data, weights=weights,method=test_method_cox)
    #   if(metric=="SL"){
    #     #PIC <- summary(fit)[[sigMethod]][1]
    #     PIC <- fit$score
    #   }else{
    #     PIC <- modelFitStat(metric,fit,"Likelihood",TRUE)
    #   }
    #   singSet[1,1:3] <- c(length(attr(fit$terms,"term.labels")),PIC,paste0(c(includeName),collapse=" "))
    #   includeSubSet <- singSet
    #   xCheck <- setdiff(xName,includeName)
    # }else{
    #   includeSubSet <- NULL
    #   xCheck <- xName
    # }
    # for(nv in 1:length(xCheck)){
    #   subSet <- NULL
    #   comTable <- combn(xCheck,nv)
    #   for(ncom in 1:ncol(comTable)){
    #     comVar <- c(includeName,comTable[,ncom])
    #     fm <- reformulate(comVar, yName)
    #     fit <- survival::coxph(fm,data = data,weights=weights,method=test_method_cox)
    #     if(metric=="SL"){
    #       PIC <- fit$score
    #     }else{
    #       PIC <- modelFitStat(metric,fit,"Likelihood",TRUE)
    #     }
    #     singSet[1,1:3] <- c(attr(logLik(fit),"df"),PIC,paste0(comVar,collapse=" "))
    #     subSet <- rbind(subSet,singSet)
    #   }
    #   bestSubSet <- as.data.frame(subSet)
    #   bestSubSet[,2] <- as.numeric(bestSubSet[,2])
    #   if(metric=="SL"){
    #     subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
    #   }else{
    #     subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
    #   }
    # 
    #   if(nrow(subSet) < best_n){
    #   	best_n <- nrow(subSet)
    #   }
    #   
    #   finalResult <- rbind(finalResult,subResultSort[1:best_n,])
    # }
    # finalResult <- finalResult[-1,]
    # RegPIC <- rbind(includeSubSet,finalResult)
    # rownames(RegPIC) <- c(1:nrow(RegPIC))
    # result$'Process of Selection' <- RegPIC
    # if(metric=="SL"){
    #   xModel <- unlist(strsplit(RegPIC[which.max(as.numeric(RegPIC[,2])),3]," "))
    # }else{
    #   xModel <- unlist(strsplit(RegPIC[which.min(as.numeric(RegPIC[,2])),3]," "))
    # }
  	
  }else{ #forward # bidirection # backward
    subBestPoint <- data.frame(Step=numeric(),
                               EnteredEffect=character(),
                               RemovedEffect=character(),
                               DF=numeric(),
                               NumberIn=numeric(),
                               metric=numeric())
    colnames(subBestPoint)[6] <- metric
    bestPoint <- subBestPoint
    if(strategy=="backward"){
      addVar <- FALSE
      xModel <- c(includeName,setdiff(xName,includeName))
      xResidual <- NULL
      fmFull <- reformulate(xModel, yName)
      fitFull <- survival::coxph(fmFull,data=data,weights=weights,method=test_method_cox)
      if(metric=="SL"){
        PIC <- 1
      }else{
        PIC <- modelFitStat(metric,fitFull,"Likelihood",TRUE)
      }
      k <- attr(logLik(fitFull),"df")
      bestPoint[1,-1] <- c("","","",k,PIC)
    }else{
      addVar <- TRUE
      xModel <- c(includeName)
      xResidual <- setdiff(xName,includeName)
      if(metric=="SL"){
        PIC <- 1
      }else{
        PIC <- Inf
      }
      bestPoint[1,] <- c(0,"","",0,0,PIC)
      if(!is.null(includeName)){
        fmInt <- reformulate("0",yName)
        fitInt <- survival::coxph(fmInt,data=data,weights=weights,method=test_method_cox)
        fmInc <- reformulate(includeName,yName)
        fitInc <- survival::coxph(fmInc,data=data,weights=weights,method=test_method_cox)
        if(metric=="SL"){
          PIC <- anova(fitInt,fitInc)[2,'Pr(>|Chi|)']
        }else{
          PIC <- modelFitStat(metric,fitInc,"Likelihood",TRUE)
        }
        k <- attr(logLik(fitInc),"df")
        subBestPoint[1,-1] <- c(paste0(includeName,collapse=" "),"",anova(fitInt,fitInc)[2,'Df'],k,PIC)
        bestPoint <- rbind(bestPoint,subBestPoint)
      }
    }
    while(TRUE){
      if(addVar==TRUE){
        if(is.null(xModel)){
          xMod <- "0"
        }else{
          xMod <- xModel
        }
        fm0 <- reformulate(xMod, yName)
        fit0 <- survival::coxph(fm0,data = data,weights=weights,method=test_method_cox)
	if(length(xResidual)==0){
		break
	}
        xResidualList <- as.list(xResidual)
        names(xResidualList) <- xResidual
        fm1 <- lapply(xResidualList,function(x){reformulate(c(xModel,x),yName)})
        fit1 <- lapply(fm1,function(x){survival::coxph(x,data = data,weights=weights,method=test_method_cox)})
        if(metric=="SL"){
          threshold <- sle
          PICset <- sapply(fit1,function(x){anova(fit0,x)[2,'Pr(>|Chi|)']})
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PICset <- sapply(fit1,function(x){modelFitStat(metric,x,"Likelihood",TRUE)})
        }
        mPIC <- min(PICset)
        minmaxVar <- names(which.min(PICset))
        minmaxFit1 <- fit1[[minmaxVar]]
        if(mPIC < threshold){
          indicator <- TRUE
          xModel <- append(xModel,minmaxVar)
          xResidual <- setdiff(xResidual,minmaxVar)
          k <- attr(logLik(minmaxFit1),"df")
          subBestPoint[1,-1] <- c(minmaxVar,"",anova(fit0,minmaxFit1)[2,'Df'],k,mPIC)
          bestPoint <- rbind(bestPoint,subBestPoint)
        }else{
          indicator <- FALSE
        }
      }else{
        fm1 <- reformulate(xModel,yName)
        fit1 <- survival::coxph(fm1,data=data,weights=weights,method=test_method_cox)
        xChcek <- setdiff(xModel,c(includeName))
        if(is.null(xChcek)){
          break
        }else if(length(xChcek)==1){
          fm0 <- list(reformulate("0",yName))
          names(fm0) <- xChcek
        }else{
          xChcekList <- as.list(xChcek)
          names(xChcekList) <- xChcek
          fm0 <- lapply(xChcekList,function(x){reformulate(setdiff(xModel,x),yName)})
        }
        fit0 <- lapply(fm0,function(x){survival::coxph(x,data=data,weights=weights,method=test_method_cox)})
        if(metric=="SL"){
          threshold <- sls
          PIC <- sapply(fit0,function(x){anova(x,fit1)[2,'Pr(>|Chi|)']})
          mPIC <- max(PIC)
          minmaxVar <- names(which.max(PIC))
          if(mPIC > threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PIC <- sapply(fit0,function(x){modelFitStat(metric,x,"Likelihood",TRUE)})
          mPIC <- min(PIC)
          minmaxVar <- names(which.min(PIC))
          if(mPIC < threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }
        if(indicator==TRUE){
          minmaxFit0 <- fit0[[minmaxVar]]
          xResidual <- append(xResidual,minmaxVar)
          xModel <- setdiff(xModel,minmaxVar)
          k <- attr(logLik(minmaxFit0),"df")
          subBestPoint[1,-1] <- c("",minmaxVar,anova(minmaxFit0)[2,'Df'],k,mPIC)
          bestPoint <- rbind(bestPoint,subBestPoint)
        }
      }
      ## change direction or stop for this while loop
      if(indicator==TRUE){
        if(strategy=="bidirection"){
          if(addVar==TRUE){
            addVar <- FALSE
          }else{
            addVar <- TRUE
          }
          next
        }else{
          next
        }
      }else{
        if(strategy=="bidirection" && addVar==TRUE){
          break
        }else if(strategy=="bidirection" && addVar==FALSE){
          addVar <- TRUE
          next
        }else if(strategy != "bidirecion"){
          break
        }
      }
    }#while
    if(strategy!="backward"){
      bestPoint <- bestPoint[-1,]
      if(is.null(includeName)){
        nInc <- 0
      }else{
        nInc <- 1
      }
      if(nInc<nrow(bestPoint)){
        bestPoint[,1] <- c(rep(0,nInc),1:(nrow(bestPoint)-nInc))
      }
    }else{
      bestPoint[,1] <- c(1:nrow(bestPoint)-1)
    }
    result$'Process of Selection' <- bestPoint
  }
  lastModel <- reformulate(xModel,yName)
  lastFit <- survival::coxph(lastModel,data,weights=weights,method=test_method_cox)
  MLE <- coef(summary(lastFit))
  MLE <- as.data.frame(cbind(rownames(MLE),MLE))
  colnames(MLE)[1] <- c("Variable")
  variables <- as.data.frame(t(data.frame(xModel)))
  colnames(variables) <- paste0("variables",1:length(xModel))
  result$'Selected Varaibles' <- variables
  result$'Coefficients of the Selected Variables' <- MLE
  class(result) <- c("StepReg","list")
  return(result)
}
