test_method_linear <- "Pillai"
test_method_logit <- "Rao"
test_method_cox <- "efron"
library(StepReg)

## linear stepwise
data(mtcars)
mtcars$yes <- mtcars$wt
formula <- cbind(mpg,drat) ~ . + 0

stepwise2(type  =  "linear",
         formula = formula,
         data = mtcars,
         strategy = "bidirection",
         metric = "AIC")

StepReg::stepwise(formula = formula,
          data = mtcars,
          selection = "bidirection",
          select = "AIC")      

## logit stepwise
data(mtcars)
formula <- vs ~ .
type = "logit"
data = mtcars
strategy = "forward"
metric = "SBC"
include = NULL
sle = 0.15
sls = 0.15
tolerance = 1e-7
weights = NULL
best_n = Inf
output_excel = NULL

stepwise2(type = "logit",
         formula,
         data = mtcars,
         strategy = "forward",
         metric = "SBC")

?stepwiseLogit
data(mtcars)
formula <- vs ~ .
stepwiseLogit(formula,
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

a2 <- stepwise2(type = "cox",
         formula = formula,
         data = my.data,
         strategy = "bidirection",
         metric = "AIC")
#?stepwiseCox
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
data <- my.data
formula = Surv(time, status1) ~ . - status 

stepwiseCox(formula=formula,
            data=my.data,
            selection="bidirection",
            select="AIC",
            method="efron")

