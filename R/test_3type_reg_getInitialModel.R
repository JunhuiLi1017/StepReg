library(StepReg)
?stepwise
?stepwiseLogit
?stepwiseCox

data(mtcars)
mtcars$yes <- mtcars$wt
#formula <- cbind(mpg,drat) ~ . + 0
formula <- mpg ~ . + 1
fit <- lm(formula,data=mtcars)
attr(fit$terms,"term.labels")

data(mtcars)
mtcars$yes <- mtcars$wt
formula <- mpg ~ . + 1
stepwise(formula=formula,
         data=mtcars,
         include = c("cyl","gear"),
         selection="forward",
         select="Rsq")



data=mtcars
#include = NULL
include = c("cyl","gear")
strategy = c("forward")
selection=strategy
metric = c("Rsq")
select=metric
sle = 0.15
sls = 0.15
multivarStat = c("Pillai")
weights = NULL
best = NULL

# function (formula, data, include = NULL, selection = c("forward", 
#                                                        "backward", "bidirection", "score"), select = c("AIC", "AICc", 
#                                                                                                        "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC"), 
#           sle = 0.15, sls = 0.15, multivarStat = c("Pillai", "Wilks", 
#                                                    "Hotelling-Lawley", "Roy"), weights = NULL, best = NULL) 
# {
  
  if (selection == "score" & select == "SL") {
    stop("select = 'SL' is not allowed when specifing selection = 'score'")
  }
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula, data = data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm, "term.labels")
  if (attr(termForm, "intercept") == 0) {
    intercept <- "0"
  }else {
    intercept <- "1"
  }
  if (is.character(include)) {
    if (!all(include %in% xName)) {
      stop("variable in include is not included formula or dataset")
    }else {
      includeName <- include
      mergeIncName <- paste0(includeName, collapse = " ")
    }
  }else if (is.null(include)) {
    includeName <- NULL
    mergeIncName <- "NULL"
  }else {
    stop("include should be character vector indicating variable to be included in all models")
  }
  if (!is.null(weights)) {
    if (length(weights) == nrow(data)) {
      weightData <- data * sqrt(weights)
    }else {
      stop("Variable length is different ('(weights)')")
    }
  } else {
    weightData <- data
  }
  lmFull <- lm(formula, data = weightData)
  allVarClass <- attr(lmFull$terms, "dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class", "variable")
  for (i in names(table(allVarClass))) {
    classTable[names(table(allVarClass)) %in% i, 2] <- paste0(names(allVarClass[allVarClass %in% 
                                                                                  i]), collapse = " ")
  }
  if (any(allVarClass == "factor")) {
    factVar <- names(which(allVarClass == "factor"))
    for (i in factVar) {
      weightData[, i] <- as.factor(as.numeric(weightData[, 
                                                         i]))
    }
  }
  xMatrix <- as.matrix(weightData[, xName])
  qrXList <- qr(xMatrix, tol = 1e-07)
  rank0 <- qrXList$rank
  pivot0 <- qrXList$pivot
  if (rank0 < length(pivot0)) {
    mulcolX <- colnames(qrXList$qr)[pivot0[(rank0 + 1):length(pivot0)]]
    mulcolMergeName <- paste0(mulcolX, collapse = " ")
  }else {
    mulcolX <- NULL
    mulcolMergeName <- "NULL"
  }
  xName <- setdiff(xName, mulcolX)
  Y <- as.matrix(lmFull$model[, yName])
  nY <- ncol(Y)
  nObs <- nrow(data)
  if (nY == 1) {
    approxF <- "F"
  }else {
    approxF <- multivarStat
    if (any(c(select) == c("BIC", "CP", "Rsq", "adjRsq"))) {
      stop("Can't specify 'BIC','CP','Rsq' or 'adjRsq' when using multivariate multiple regression")
    }
  }
  if ((select == "CP" | select == "BIC") & lmFull$rank >= nObs) {
    stop("'select' can't specify 'CP' or 'BIC' when variable number is greater than number of observation")
  }
  result <- list()
  ModInf <- matrix(NA, 9, 1)
  ModInf <- cbind(ModInf, matrix(c(yName, mergeIncName, selection, 
                                   select, sle, sle, approxF, mulcolMergeName, intercept), 
                                 9, 1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters", "Value")
  ModInf[, 1] <- c("Response Variable", "Included Variable", 
                   "Selection Method", "Select Criterion", "Entry Significance Level(sle)", 
                   "Stay Significance Level(sls)", "Variable significance test", 
                   "Multicollinearity Terms", "Intercept")
  if (select == "SL") {
    if (selection == "forward") {
      ModInf <- ModInf[-6, ]
    }
    else if (selection == "backward") {
      ModInf <- ModInf[-5, ]
    }
    else if (selection == "score") {
      ModInf <- ModInf[-c(5:6), ]
    }
  }else {
    ModInf <- ModInf[-c(5:6), ]
  }
  rownames(ModInf) <- 1:nrow(ModInf)
  result$"Summary of Parameters" <- ModInf
  result$"Variables Type" <- classTable
  
  subBestPoint <- data.frame(Step = numeric(), EnteredEffect = character(), 
                             RemovedEffect = character(), DF = numeric(), NumberEffectIn = numeric(), 
                             NumberParmsIn = numeric(), select = numeric())
  colnames(subBestPoint)[7] <- select
  bestPoint <- subBestPoint
  if (selection == "backward") {
    addIdx <- FALSE
    xModel <- c(intercept, includeName, setdiff(xName, 
                                                includeName))
    xResidual <- NULL
    if (select == "SL") {
      PIC <- 1
    }else {
      PIC <- modelFitStat(select, lmFull, "LeastSquare")
    }
    bestPoint[1, -1] <- c("", "", "", "", length(attr(lmFull$terms, 
                                                   "term.labels")), lmFull$rank, PIC)
  }else {
    addIdx <- TRUE
    xModel <- c(intercept, includeName)
    xResidual <- setdiff(xName, includeName)
    fmInt <- reformulate(intercept, yName)
    fitInt <- lm(fmInt, data = weightData)
    if (select == "SL") {
      PIC <- 1
    }else {
      if (intercept == "1") {
        PIC <- modelFitStat(select, fitInt, "LeastSquare")
      }else {
        if (select %in% c("Rsq", "adjRsq")) {
          PIC <- 0
        }else {
          PIC <- Inf
        }
      }
    }
    bestPoint[1,] <- c("", intercept, "", fitInt$rank, 
                        length(attr(fitInt$terms, "term.labels")), fitInt$rank, 
                        PIC)
    if (!is.null(includeName)) {
      fmInc <- reformulate(c(intercept, includeName), 
                           yName)
      fitInc <- lm(fmInc, data = weightData)
      if (select == "SL") {
        PIC <- anova(fitInc, fitInt, test = approxF)[2, 
                                                     "Pr(>F)"]
      }
      else {
        PIC <- modelFitStat(select, fitInc, "LeastSquare")
      }
      subBestPoint[1, ] <- c(0, mergeIncName, "", anova(fitInc, 
                                                        fitInt, test = approxF)[2, "Df"], length(attr(fitInt$terms, 
                                                                                                      "term.labels")), fitInc$rank, PIC)
      bestPoint <- rbind(bestPoint, subBestPoint)
    }
  }


  
  
x_name <- xName
include <- includeName
y_name <- yName
type="linear"



data(mtcars)
formula <- vs ~ .
fit <- glm(formula,data=mtcars)
fit <- glm(formula,data=mtcars,family="binomial")


data=mtcars
include = NULL
include = c("cyl","gear")
strategy = c("forward")
selection=strategy
metric = c("AIC")
select=metric
sle = 0.15
sls = 0.15
sigMethod = c("Rao")
weights = NULL
best = NULL

#stepwiseLogit

#function (formula, data, include = NULL, selection = c("forward", 
#                                                       "backward", "bidirection", "score"), select = c("SL", "AIC", 
#                                                                                                       "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)"), sle = 0.15, 
#          sls = 0.15, sigMethod = c("Rao", "LRT"), weights = NULL, 
#          best = NULL) 
#{

  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula, data = data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm, "term.labels")
  if (attr(termForm, "intercept") == 0) {
    intercept <- "0"
  }else {
    intercept <- "1"
  }
  if (is.character(include)) {
    if (!all(include %in% xName)) {
      stop("variable in include is not included formula or dataset")
    }else {
      includeName <- include
      mergeIncName <- paste0(includeName, collapse = " ")
    }
  }else if (is.null(include)) {
    includeName <- include
    mergeIncName <- "NULL"
  }else {
    stop("include should be character vector indicating variable to be included in all models")
  }
  fmFull <- reformulate(c(intercept, xName), yName)
  fitFull <- glm(fmFull, data = data, weights = weights, family = "binomial")
  allVarClass <- attr(fitFull$terms, "dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class", "variable")
  for (i in names(table(allVarClass))) {
    classTable[names(table(allVarClass)) %in% i, 2] <- paste0(names(allVarClass[allVarClass %in% 
                                                                                  i]), collapse = " ")
  }
  if (any(allVarClass == "factor")) {
    factVar <- names(which(allVarClass == "factor"))
    for (i in factVar) {
      data[, i] <- as.factor(as.numeric(data[, i]))
    }
  }
  xMatrix <- as.matrix(data[, xName])
  qrXList <- qr(xMatrix, tol = 1e-07)
  rank0 <- qrXList$rank
  pivot0 <- qrXList$pivot
  if (rank0 < length(pivot0)) {
    mulcolX <- colnames(qrXList$qr)[pivot0[(rank0 + 1):length(pivot0)]]
    mulcolMergeName <- paste0(mulcolX, collapse = " ")
  }else {
    mulcolX <- NULL
    mulcolMergeName <- "NULL"
  }
  xName <- setdiff(xName, mulcolX)
  n <- nrow(data)
  result <- list()
  ModInf <- matrix(NA, 9, 1)
  ModInf <- cbind(ModInf, matrix(c(yName, mergeIncName, selection, 
                                   select, sle, sle, sigMethod, mulcolMergeName, intercept), 
                                 9, 1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters", "Value")
  ModInf[, 1] <- c("Response Variable", "Included Variable", 
                   "Selection Method", "Select Criterion", "Entry Significance Level(sle)", 
                   "Stay Significance Level(sls)", "Variable significance test", 
                   "Multicollinearity Terms", "Intercept")
  if (select == "SL") {
    if (selection == "forward") {
      ModInf <- ModInf[-6, ]
    }else if (selection == "backward") {
      ModInf <- ModInf[-5, ]
    }else if (selection == "score") {
      ModInf <- ModInf[-c(5:6), ]
    }
  }else {
    ModInf <- ModInf[-c(5:6), ]
  }
  rownames(ModInf) <- 1:nrow(ModInf)
  result$"Summary of Parameters" <- ModInf
  result$"Variables Type" <- classTable
  if (selection == "score") {
  }else {
    subBestPoint <- data.frame(Step = numeric(), EnteredEffect = character(), 
                               RemovedEffect = character(), DF = numeric(), NumberIn = numeric(), 
                               select = numeric())
    colnames(subBestPoint)[6] <- select
    bestPoint <- subBestPoint
    if (selection == "backward") {
      addVar <- FALSE
      xModel <- c(intercept, includeName, setdiff(xName, 
                                                  includeName))
      xResidual <- NULL
      fmFull <- reformulate(xModel, yName)
      fitFull <- glm(fmFull, data = data, weights = weights, 
                     family = "binomial")
      if (select == "SL") {
        PIC <- 1
      }else {
        PIC <- modelFitStat(select, fitFull, "Likelihood")
      }
      bestPoint[1, -1] <- c("", "", fitFull$rank, fitFull$rank, 
                            PIC)
    }else {
      addVar <- TRUE
      xModel <- c(intercept, includeName)
      xResidual <- setdiff(xName, includeName)
      fmInt <- reformulate(intercept, yName)
      fitInt <- glm(fmInt, data = data, weights = weights, 
                    family = "binomial")
      if (select == "SL") {
        PIC <- 1
      }else {
        if (intercept == "0") {
          PIC <- Inf
        }else {
          PIC <- modelFitStat(select, fitInt, "Likelihood")
        }
      }
      bestPoint[1, -1] <- c(intercept, "", fitInt$rank, 
                            fitInt$rank, PIC)
      if (!is.null(includeName)) {
        fmInc <- reformulate(xModel, yName)
        fitInc <- glm(fmInc, data = data, weights = weights, 
                      family = "binomial")
        if (select == "SL") {
          PIC <- anova(fitInt, fitInc, test = sigMethod)[2, 
                                                         "Pr(>Chi)"]
        }else {
          PIC <- modelFitStat(select, fitInc, "Likelihood")
        }
        subBestPoint[1, -1] <- c(paste0(includeName, 
                                        collapse = " "), "", anova(fitInt, fitInc)[2, 
                                                                                   "Df"], fitInc$rank, PIC)
        bestPoint <- rbind(bestPoint, subBestPoint)
      }
    }
  }

x_name <- xName
include <- includeName
y_name <- yName
type="logit"
  













library(survival)
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
data <- my.data
formula = Surv(time, status1) ~ . - status 
fit <- survival::coxph(formula,data=data)


#include = NULL
include = c("age","sex")
strategy = c("forward")
selection=strategy
metric = c("AIC")
select=metric
sle = 0.15
sls = 0.15
method = c("efron")
weights = NULL
best = NULL
# stepwiseCox
# function (formula, data, include = NULL, selection = c("forward", 
#                                                        "backward", "bidirection", "score"), select = c("SL", "AIC", 
#                                                                                                        "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)"), sle = 0.15, 
#           sls = 0.15, method = c("efron", "breslow", "exact"), weights = NULL, 
#           best = NULL) 
# {
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula, data = data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm, "term.labels")
  if (is.character(include)) {
    if (!all(include %in% xName)) {
      stop("variable in include is not included formula or dataset")
    }else {
      includeName <- include
      mergeIncName <- paste0(includeName, collapse = " ")
    }
  }else if (is.null(include)) {
    includeName <- include
    mergeIncName <- "NULL"
  }else {
    stop("include should be character vector indicating variable to be included in all models")
  }
  fmFull <- reformulate(c(xName), yName)
  fitFull <- survival::coxph(fmFull, data = data, weights = weights, 
                             method = method)
  allVarClass <- attr(fitFull$terms, "dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class", "variable")
  for (i in names(table(allVarClass))) {
    classTable[names(table(allVarClass)) %in% i, 2] <- paste0(names(allVarClass[allVarClass %in% 
                                                                                  i]), collapse = " ")
  }
  if (any(allVarClass == "factor")) {
    factVar <- names(which(allVarClass == "factor"))
    for (i in factVar) {
      data[, i] <- as.factor(as.numeric(data[, i]))
    }
  }
  xMatrix <- as.matrix(data[, xName])
  qrXList <- qr(xMatrix, tol = 1e-07)
  rank0 <- qrXList$rank
  pivot0 <- qrXList$pivot
  if (rank0 < length(pivot0)) {
    mulcolX <- colnames(qrXList$qr)[pivot0[(rank0 + 1):length(pivot0)]]
    mulcolMergeName <- paste0(mulcolX, collapse = " ")
  }else {
    mulcolX <- NULL
    mulcolMergeName <- "NULL"
  }
  xName <- setdiff(xName, mulcolX)
  n <- nrow(data)
  result <- list()
  ModInf <- matrix(NA, 8, 1)
  ModInf <- cbind(ModInf, matrix(c(yName, mergeIncName, selection, 
                                   select, sle, sle, method, mulcolMergeName), 8, 1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters", "Value")
  ModInf[, 1] <- c("Response Variable", "Included Variable", 
                   "Selection Method", "Select Criterion", "Entry Significance Level(sle)", 
                   "Stay Significance Level(sls)", "Method", "Multicollinearity Terms")
  if (select == "SL") {
    if (selection == "forward") {
      ModInf <- ModInf[-6, ]
    }else if (selection == "backward") {
      ModInf <- ModInf[-5, ]
    }else if (selection == "score") {
      ModInf <- ModInf[-c(5:6), ]
    }
  }else {
    ModInf <- ModInf[-c(5:6), ]
  }
  rownames(ModInf) <- 1:nrow(ModInf)
  result$"Summary of Parameters" <- ModInf
  result$"Variables Type" <- classTable
  if (selection == "score") {
  }else {
    subBestPoint <- data.frame(Step = numeric(), EnteredEffect = character(), 
                               RemovedEffect = character(), DF = numeric(), NumberIn = numeric(), 
                               select = numeric())
    colnames(subBestPoint)[6] <- select
    bestPoint <- subBestPoint
    if (selection == "backward") {
      addVar <- FALSE
      xModel <- c(includeName, setdiff(xName, includeName))
      xResidual <- NULL
      fmFull <- reformulate(xModel, yName)
      fitFull <- survival::coxph(fmFull, data = data, weights = weights, 
                                 method = method)
      if (select == "SL") {
        PIC <- 1
      }else {
        PIC <- modelFitStat(select, fitFull, "Likelihood", 
                            TRUE)
      }
      k <- attr(logLik(fitFull), "df")
      bestPoint[1, -1] <- c("", "", "", k, PIC)
    }else {
      addVar <- TRUE
      xModel <- c(includeName)
      xResidual <- setdiff(xName, includeName)
      if (select == "SL") {
        PIC <- 1
      }else {
        PIC <- Inf
      }
      bestPoint[1, ] <- c(0, "", "", 0, 0, PIC)
      if (!is.null(includeName)) {
        fmInt <- reformulate("0", yName)
        fitInt <- survival::coxph(fmInt, data = data, 
                                  weights = weights, method = method)
        fmInc <- reformulate(includeName, yName)
        fitInc <- survival::coxph(fmInc, data = data, 
                                  weights = weights, method = method)
        if (select == "SL") {
          PIC <- anova(fitInt, fitInc)[2, "Pr(>|Chi|)"]
        }else {
          PIC <- modelFitStat(select, fitInc, "Likelihood", 
                              TRUE)
        }
        k <- attr(logLik(fitInc), "df")
        subBestPoint[1, -1] <- c(paste0(includeName, 
                                        collapse = " "), "", anova(fitInt, fitInc)[2, 
                                                                                   "Df"], k, PIC)
        bestPoint <- rbind(bestPoint, subBestPoint)
      }
    }
  }

  x_name <- xName
  include <- includeName
  y_name <- yName
  type="cox"
  intercept='0'
  