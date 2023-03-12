#' Stepwise Linear Model Regression
#' 
#' Stepwise linear regression analysis selects model based on information criteria and F or approximate F test with 'forward', 'backward', 'bidirection' and 'subset' model strategy method.
#' 
#' @param formula (formula) The formula used for model fitting. The formula takes the form of a '~' (tilde) symbol, with the response variable(s) on the left-hand side, and the predictor variable(s) on the right-hand side. The 'lm()' function uses this formula to fit a regression model. A formula can be as simple as 'y ~ x'. For multiple predictors, they must be separated by the '+' (plus) symbol, e.g. 'y ~ x1 + x2'. To include an interaction term between variables, use the ':' (colon) symbol: 'y ~ x1 + x1:x2'. Use the '.' (dot) symbol to indicate that all other variables in the dataset should be included as predictors, e.g. 'y ~ .'. In the case of multiple response variables (multivariate), the formula can be specified as 'cbind(y1, y2) ~ x1 + x2'. By default, an intercept term is always included in the models, to exclude it, include '0' or '- 1' in your formula: 'y ~ 0 + x1', 'y ~ x1 + 0', and 'y ~ x1 - 1'.
#' @param data (data.frame) A dataset consisting of predictor variable(s) and response variable(s).
#' @param include (NULL|character) A character vector specifying predictor variables that will always stay in the model. A subset of the predictors in the dataset.
#' @param strategy (character) The model selection strategy. Choose from 'forward', 'backward', 'bidirectional' and 'subset'. Default is 'forward'. More information, see [StepReg](https://github.com/JunhuiLi1017/StepReg#stepwise-regression)
#' @param metric (character) The model selection criterion (model fit score). Used for the evaluation of the predictive performance of an intermediate model. Choose from 'AIC', 'AICc', 'BIC', 'CP', 'HQ', 'HQc', 'Rsq', 'adjRsq', 'SL', 'SBC'. Default is 'AIC'.
#' @param sle (numeric) Significance Level to Enter. It is the statistical significance level that a predictor variable must meet to be included in the model. E.g. if 'sle = 0.05', a predictor with a P-value less than 0.05 will 'enter' the model. Default is 0.15.
#' @param sls (numeric) Significance Level to Stay. Similar to 'sle', 'sls' is the statistical significance level that a predictor variable must meet to 'stay' in the model. E.g. if 'sls = 0.1', a predictor that was previously included in the model but whose P-value is now greater than 0.1 will be removed.
#' @param weights (numeric) A numeric vector specifying the coefficients assigned to the predictor variables. The magnitude of the weights reflects the degree to which each predictor variable contributes to the prediction of the response variable. The range of weights should be from 0 to 1. Values greater than 1 will be coerced to 1, and values less than 0 will be coerced to 0. Default is 1, which means that all weights are set to 1.
#' @param test_method_linear (character) Test method for multivariate linear regression analysis, choose from 'Pillai', 'Wilks', 'Hotelling-Lawley', 'Roy'. Default is 'Pillai'. For univariate regression, 'F-test' will be used. 
#' @param best_n (numeric(integer)) The number of models to keep in the final output. Default is Inf, which means that all models will be displayed.
#' 
#' @author Junhui Li, Kai Hu
#' 
#' @references 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' Dharmawansa, P. , Nadler, B. , & Shwartz, O. . (2014). Roy's largest root under rank-one alternatives:the complex valued case and applications. Statistics.
#' Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.
#' Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.
#' Hocking, R. R. (1976). A biometrics invited paper. the analysis and selection of variables in linear regression. Biometrics, 32(1), 1-49.
#' Hurvich, C. M., & Tsai, C. (1989). Regression and time series model selection in small samples. Biometrika, 76(2), 297-307.
#' Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.
#' Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.
#' Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.
#' Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.
#' Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model Selection. Regression and time series model selection /. World Scientific.
#' Pillai, K. . (1955). Some new test criteria in multivariate analysis. The Annals of Mathematical Statistics, 26(1), 117-121.
#' R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable selection in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.
#' Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.
#' Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
#' 
#' @examples
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- cbind(mpg,drat) ~ . + 0
#' stepwiseLinear(formula=formula,
#'          data=mtcars,
#'          strategy="bidirection",
#'          metric="AIC")
#'          
#' @keywords stepwise linear regression
#' 
#' @importFrom utils combn
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms
#' 
#' @export
#' 
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
  strategy <- match.arg(strategy)
  metric <- match.arg(metric)
  test_method_linear <- match.arg(test_method_linear)
  ## subset and SL
  if(strategy=="subset" & metric=="SL"){
    stop("metric = 'SL' is not allowed when specifing strategy = 'subset'")
  }
  ## extract response, independent variable and intercept
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula,data=data)
  #yName <- rownames(attr(termForm,"factors"))[1]
  #yName <- all.vars(formula)[1]
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
    includeName <- NULL
    mergeIncName <- "NULL"
  }else{
    stop("include should be character vector indicating variable to be included in all models")
  }
  if(!is.null(weights)){
    if(length(weights)==nrow(data)){
      weightData <- data*sqrt(weights)
    }else{
      stop("Variable length is different ('(weights)')")
    }
  }else{
    weightData <- data
  }

  lmFull <- lm(formula,data=weightData)
  allVarClass <- attr(lmFull$terms,"dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class","variable")
  for(i in names(table(allVarClass))){
    classTable[names(table(allVarClass)) %in% i,2] <- paste0(names(allVarClass[allVarClass %in% i]),collapse=" ")
  }
  ## detect multicollinearity
  if(any(allVarClass=="factor")){
    factVar <- names(which(allVarClass=="factor"))
    for(i in factVar){
      weightData[,i] <- as.factor(as.numeric(weightData[,i]))
    }
  }
  xMatrix <- as.matrix(weightData[,xName])
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
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,strategy,metric,sle,sle,approxF,mulcolMergeName,intercept),9,1))
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
  result$'Variables Type' <- classTable
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
      if(is.null(best_n)){
        nbest <- nrow(subSet)
      }else{
        if(nrow(subSet)>best_n){
          nbest <- best_n
        }else{
          nbest <- nrow(subSet)
        }
      }
      bestSubSet <- as.data.frame(subSet)
      bestSubSet[,2] <- as.numeric(bestSubSet[,2])
      if(metric=="Rsq" | metric=="adjRsq"){
        subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
      }else{
        subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
      }
      finalResult <- rbind(finalResult,subResultSort[1:nbest,])
    }
    finalResult <- finalResult[-1,]
    rownames(finalResult) <- 1:nrow(finalResult)
    result$'Process of Selection' <- finalResult
    if(metric=="Rsq" | metric=="adjRsq"){
      xModel <- unlist(strsplit(finalResult[which.max(as.numeric(finalResult[,3])),4]," "))
    }else{
      xModel <- unlist(strsplit(finalResult[which.min(as.numeric(finalResult[,3])),4]," "))
    }
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
      bestPoint[1,] <- c(0,"","","",length(attr(lmFull$terms,"term.labels")),lmFull$rank,PIC)
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
