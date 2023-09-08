include = NULL
sle = 0.15
sls = 0.15
test_method_linear = c("Pillai")
test_method_logit = c("Rao")
test_method_cox = c("efron")
tolerance = 1e-7
weight = NULL
best_n = Inf
excel_name = NULL



res_v1_4_4 <- readRDS(system.file("tests/data","res_v1_4_4.rds", package = "StepReg"))
data(mtcars)
mtcars$yes <- mtcars$wt
mod=names(res_v1_4_4)[1]

  type <- unlist(stringr::str_split(mod,"_"))[1]
  if(mod=="cox_model1"){
    lung <- survival::lung %>% na.omit()
    mydata <- lung
  }else if(mod %in% c("linear_model1","linear_model2","logit_model1")){
    data(mtcars)
    mtcars$yes <- mtcars$wt
    mydata <- mtcars
  }

  linear_model1 <- mpg ~ . + 1
  linear_model2 <- cbind(mpg, drat) ~ . + 0
  logit_model1 <- vs ~ .
  cox_model1 <- Surv(time, status) ~ .
  
type = "linear"
formula=get(mod)
data=mydata
strategy="forward"
metric="Rsq"

traceback()

source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwise.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwiseUtils.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/validateUtils.R")

a <- stepwise1(type = type,
          formula=get(mod),
          data=mydata,
          strategy="backward",
          metric="SL")








