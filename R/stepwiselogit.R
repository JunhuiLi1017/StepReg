#' Stepwise Logistic Regression
#' 
#' Stepwise logistic regression analysis selects model based on information criteria and Wald or Score test with 'forward', 'backward', 'bidirection' and 'subset' model strategy method.
#' 
#' @param formula (formula) The formula used for model fitting. The formula takes the form of a '~' (tilde) symbol, with the response variable(s) on the left-hand side, and the predictor variable(s) on the right-hand side. The 'lm()' function uses this formula to fit a regression model. A formula can be as simple as 'y ~ x'. For multiple predictors, they must be separated by the '+' (plus) symbol, e.g. 'y ~ x1 + x2'. To include an interaction term between variables, use the ':' (colon) symbol: 'y ~ x1 + x1:x2'. Use the '.' (dot) symbol to indicate that all other variables in the dataset should be included as predictors, e.g. 'y ~ .'. In the case of multiple response variables (multivariate), the formula can be specified as 'cbind(y1, y2) ~ x1 + x2'. By default, an intercept term is always included in the models, to exclude it, include '0' or '- 1' in your formula: 'y ~ 0 + x1', 'y ~ x1 + 0', and 'y ~ x1 - 1'.
#' 
#' @param data (data.frame) A dataset consisting of predictor variable(s) and response variable(s).
#' 
#' @param include (NULL|character) A character vector specifying predictor variables that will always stay in the model. A subset of the predictors in the dataset.
#' 
#' @param strategy (character) The model selection strategy. Choose from 'forward', 'backward', 'bidirectional' and 'subset'. Default is 'forward'. More information, see [StepReg](https://github.com/JunhuiLi1017/StepReg#stepwise-regression)
#' 
#' @param metric (character) The model selection criterion (model fit score). Used for the evaluation of the predictive performance of an intermediate model. Choose from 'AIC', 'AICc', 'SL', 'SBC', 'IC(3/2)', 'IC(1)'. Default is 'AIC'.
#' 
#' @param sle (numeric) Significance Level to Enter. It is the statistical significance level that a predictor variable must meet to be included in the model. E.g. if 'sle = 0.05', a predictor with a P-value less than 0.05 will 'enter' the model. Default is 0.15.
#' 
#' @param sls (numeric) Significance Level to Stay. Similar to 'sle', 'sls' is the statistical significance level that a predictor variable must meet to 'stay' in the model. E.g. if 'sls = 0.1', a predictor that was previously included in the model but whose P-value is now greater than 0.1 will be removed.
#' 
#' @param weights (numeric) A numeric vector specifying the coefficients assigned to the predictor variables. The magnitude of the weights reflects the degree to which each predictor variable contributes to the prediction of the response variable. The range of weights should be from 0 to 1. Values greater than 1 will be coerced to 1, and values less than 0 will be coerced to 0. Default is 1, which means that all weights are set to 1.
#' 
#' @param test_method_logit (character) Test method for univariate logit regression analysis, choose from 'Rao', 'LRT'. Default is 'Rao'.
#' 
#' @param best_n (numeric(integer)) The number of models to keep in the final output. Default is Inf, which means that all models will be displayed.
#' 
#' @references 
#' 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).
#' 
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' 
#' Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.
#' 
#' Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.
#' 
#' Hocking, R. R. (1976). A biometrics invited paper. the analysis and selection of variables in linear regression. Biometrics, 32(1), 1-49.
#' 
#' Hurvich, C. M., & Tsai, C. (1989). Regression and time series model selection in small samples. Biometrika, 76(2), 297-307.
#' 
#' Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.
#' 
#' Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.
#' 
#' Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.
#' 
#' Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.
#' 
#' Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model Selection. Regression and time series model selection /. World Scientific.
#' 
#' Pillai, K. C. S. (2006). Pillai's Trace. Encyclopedia of Statistical Sciences. John Wiley & Sons, Inc.
#' 
#' R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable selection in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.
#' 
#' Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.
#' 
#' Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
#' 
#' @author Junhui Li, Kai Hu
#' 
#' @examples
#' 
#' data(mtcars)
#' formula <- vs ~ .
#' stepwiseLogit(formula,
#'               data=mtcars,
#'               strategy="bidirection",
#'               metric="SL",
#'               sle=0.15,
#'               sls=0.15,
#'               test_method_logit="Rao")
#' 
#' @keywords stepwise logit regression
#'
#' @export

stepwiseLogit <- function(formula,
                          data,
                          include=NULL,
                          strategy=c("forward","backward","bidirection","subset"),
                          metric=c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)"),
                          sle=0.15,
                          sls=0.15,
                          test_method_logit=c("Rao","LRT"),
                          weights=NULL,
                          best_n=NULL){
  strategy <- match.arg(strategy)
  metric <- match.arg(metric)
  test_method_logit <- match.arg(test_method_logit)
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula,data=data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm,"term.labels")
  if(attr(termForm, "intercept")==0){
    intercept <- "0"
  }else{
    intercept <- "1"
  }
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
  #fit <- glm(formula,data=data, weights=weights, family="binomial")
  #https://stackoverflow.com/questions/8218196/object-not-found-error-when-passing-model-formula-to-another-function
  fmFull <- reformulate(c(intercept,xName),yName)
  fitFull <- glm(fmFull,data=data, weights=weights, family="binomial")
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
  ModInf <- matrix(NA,9,1)
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,strategy,metric,sle,sle,test_method_logit,mulcolMergeName,intercept),9,1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters","Value")
  ModInf[,1] <- c("Response Variable",
                  "Included Variable",
                  "Selection Method",
                  "Select Criterion",
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
  result$'Variables Type' <- classTable
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
	    if(is.null(best_n)){
	      nbest <- nrow(subResultSort)
	    }else{
	      if(nrow(subResultSort)<best_n){
	        nbest <- nrow(subResultSort)
	      }else{
	        nbest <- best_n
	      }
	    }
      finalResult <- rbind(finalResult,subResultSort[1:nbest,])
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
      xModel <- c(intercept,includeName,setdiff(xName,includeName))
      xResidual <- NULL
      fmFull <- reformulate(xModel, yName)
      fitFull <- glm(fmFull,data=data,weights=weights,family="binomial")
      if(metric=="SL"){
        PIC <- 1
      }else{
        PIC <- modelFitStat(metric,fitFull,"Likelihood")
      }
      bestPoint[1,-1] <- c("","",fitFull$rank,fitFull$rank,PIC)
    }else{
      addVar <- TRUE
      xModel <- c(intercept,includeName)
      xResidual <- setdiff(xName,includeName)
      fmInt <- reformulate(intercept, yName)
      fitInt <- glm(fmInt,data=data,weights=weights,family="binomial")
      if(metric=="SL"){
        PIC <- 1
      }else{
        if(intercept=="0"){
          PIC <- Inf
        }else{
          PIC <- modelFitStat(metric,fitInt,"Likelihood")
        }
      }
      bestPoint[1,-1] <- c(intercept,"",fitInt$rank,fitInt$rank,PIC)
      if(!is.null(includeName)){
        fmInc <- reformulate(xModel, yName)
        fitInc <- glm(fmInc,data=data,weights=weights,family="binomial")
        if(metric=="SL"){
          PIC <- anova(fitInt,fitInc,test=test_method_logit)[2,'Pr(>Chi)']
        }else{
          PIC <- modelFitStat(metric,fitInc,"Likelihood")
        }
        subBestPoint[1,-1] <- c(paste0(includeName,collapse=" "),"",anova(fitInt,fitInc)[2,'Df'],fitInc$rank,PIC)
        bestPoint <- rbind(bestPoint,subBestPoint)
      }
    }
    while(TRUE){
      if(addVar==TRUE){
        fm0 <- reformulate(xModel, yName)
        fit0 <- glm(fm0,data = data,weights=weights,family="binomial")
        if(length(xResidual)==0){
          break
        }
        xResidualList <- as.list(xResidual)
        names(xResidualList) <- xResidual
        fm1 <- lapply(xResidualList,function(x){reformulate(c(xModel,x),yName)})
        fit1 <- lapply(fm1,function(x){glm(x,data=data,weights=weights,family="binomial")})
        rank1 <- lapply(fit1,function(x){x$rank})
        mulColVar <- names(which(fit0$rank == rank1))
        if(length(mulColVar)>0){
          fit1 <- fit1[!names(fit1) %in% mulColVar]
        }
        if(metric=="SL"){
          threshold <- sle
          PICset <- sapply(fit1,function(x){anova(fit0,x,test=test_method_logit)[2,'Pr(>Chi)']})
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PICset <- sapply(fit1,function(x){modelFitStat(metric,x,"Likelihood")})
        }
        mPIC <- min(PICset)
        minmaxVar <- names(which.min(PICset))
        minmaxFit1 <- fit1[[minmaxVar]]
        if(mPIC < threshold){
          indicator <- TRUE
          xModel <- append(xModel,minmaxVar)
          xResidual <- setdiff(xResidual,minmaxVar)
          subBestPoint[1,-1] <- c(minmaxVar,"",anova(fit0,minmaxFit1)[2,'Df'],minmaxFit1$rank,mPIC)
          bestPoint <- rbind(bestPoint,subBestPoint)
        }else{
          indicator <- FALSE
        }
      }else{
        fm1 <- reformulate(xModel,yName)
        fit1 <- glm(fm1,data=data,weights=weights,family="binomial")
        xChcek <- setdiff(xModel,c(intercept,includeName))
        if(is.null(xChcek)){
          break
        }
        xChcekList <- as.list(xChcek)
        names(xChcekList) <- xChcek
        fm0 <- lapply(xChcekList,function(x){reformulate(setdiff(xModel,x),yName)})
        fit0 <- lapply(fm0,function(x){glm(x,data=data,weights=weights,family="binomial")})
        if(metric=="SL"){
          threshold <- sls
          PIC <- sapply(fit0,function(x){anova(x,fit1,test=test_method_logit)[2,'Pr(>Chi)']})
          mPIC <- max(PIC)
          minmaxVar <- names(which.max(PIC))
          if(mPIC > threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PIC <- sapply(fit0,function(x){modelFitStat(metric,x,"Likelihood")})
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
          subBestPoint[1,-1] <- c("",minmaxVar,anova(minmaxFit0)[2,'Df'],minmaxFit0$rank,mPIC)
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
      if(is.null(includeName)){
        nInc <- 0
      }else{
        nInc <- 1
      }
      if(1+nInc<nrow(bestPoint)){
        bestPoint[,1] <- c(rep(0,1+nInc),1:(nrow(bestPoint)-1-nInc))
      }
    }else{
      bestPoint[,1] <- c(1:nrow(bestPoint))
    }
    result$'Process of Selection' <- bestPoint
  }
  lastModel <- reformulate(xModel,yName)
  lastFit <- glm(lastModel,data=data,weights=weights,family="binomial")
  MLE <- coef(summary(lastFit))
  MLE <- data.frame(rownames(MLE),MLE)
  colnames(MLE) <- c("Variable","Estimate","StdError","t.value","P.value")
  variables <- as.data.frame(t(data.frame(xModel)))
  colnames(variables) <- paste0("variables",1:length(xModel))
  result$'Selected Varaibles' <- variables
  result$'Coefficients of the Selected Variables' <- MLE
  class(result) <- c("StepReg","list")
  return(result)
}
