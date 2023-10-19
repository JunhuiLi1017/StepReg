test_method_linear <- "Pillai"
test_method_logit <- "Rao"
test_method_cox <- "efron"

## linear stepwise
data(mtcars)
mtcars$yes <- mtcars$wt
formula <- cbind(mpg,drat) ~ . + 0

data = mtcars
strategy = "bidirection"
metric = "SBC"
include = NULL
sle = 0.15
sls = 0.15
tolerance = 1e-7
weight = NULL
best_n = Inf
excel_name = NULL
#weight = NULL

a <- stepwise(formula = formula,
         data = mtcars,
         type  =  "linear",
         strategy = "bidirection",
         metric = "AIC")


StepReg::stepwise(formula = formula,
          data = mtcars,
          selection = "bidirection",
          select = "AIC")      

## logit stepwise
data(mtcars)
formula <- vs ~ .

stepwise(formula,
         data = mtcars,
         type = "logit",
         strategy = "forward",
         metric = "SBC")

?stepwiseLogit
data(mtcars)
formula <- vs ~ .
StepReg::stepwiseLogit(formula,
              data=mtcars,
              selection="forward",
              select="SBC",
              sigMethod="Rao")


## cox stepwise
library(survival)
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
formula = Surv(time, status1) ~ . - status 

a2 <- stepwise(formula = formula,
         data = my.data,
         type = "cox",
         strategy = "subset",
         metric = "AIC")
#?stepwiseCox
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
data <- my.data
formula = Surv(time, status1) ~ . - status 

StepReg::stepwiseCox(formula=formula,
            data=my.data,
            selection="score",
            select="AIC",
            method="efron")



source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwise.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwiseUtils.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/validateUtils.R")

