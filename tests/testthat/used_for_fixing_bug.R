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
type = "logit"
formula="mpg~."
data=mydata
strategy="forward"
metric="BIC"

traceback()

source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwise.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwiseUtils.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/validateUtils.R")
?anova.glm

linear_metric <- c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")
logit_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
cox_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")


##note that in logit stepwise regression, forward use Rao to calculate p value and backward & bidirection use other method to calculate p value
stepwise1(type = "logit",
          formula=logit_model1,
          data=mtcars,
          strategy="subset",
          metric="SL")

traceback()

fm <- lm(formula=linear_model1,data=mtcars)
sigma(fm)



