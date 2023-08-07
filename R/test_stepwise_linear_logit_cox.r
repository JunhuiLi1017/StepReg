library(purrr)
library(stringr)
library(dplyr)
library(StepReg)
test_method_cox <- "efron"
test_method_linear <- "F"
test_method_logit <- "Rao"
strategy <- "bidirection"


data(mtcars)
mtcars$yes <- mtcars$wt
data <- mtcars
formula <- cbind(mpg,drat) ~ . + 0
stepwise(formula=formula,
         data=mtcars,
         selection="bidirection",
         select="AIC")
include <- NULL
type="linear"
x_name <- setdiff(getXname(formula,data),include)
y_name <- getYname(formula,data)
intercept <- getIntercept(formula,data,type="linear")
metric = "AIC"
strategy = "bidirection"
weights=NULL
## get intial stepwise model
out_init_stepwise <- getInitialStepwise(data,type,strategy,metric,weights,x_name,y_name,intercept,include,method=test_method_cox)
add_or_remove <- out_init_stepwise$add_or_remove
x_in_model <- out_init_stepwise$x_in_model
x_notin_model <- out_init_stepwise$x_notin_model
best_point <- out_init_stepwise$best_point

## get final stepwise model
out_final_stepwise <- getFinalStepModel(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method_cox,test_method_linear,test_method_logit)
x_in_model <- out_final_stepwise$x_in_model
best_point <- out_final_stepwise$best_point

## get table3-table5
table3 <- formatTable(best_point, tbl_name = "Table 3. Process of Selection")
table4 <- getTBLFianlVariable(include,x_in_model)
table5 <- getTBLCoefModel(type,intercept,include,x_in_model,y_name,n_y=2,data,test_method_cox)

list(table3,table4,table5)




?stepwiseLogit
data(mtcars)
data <- mtcars
formula <- vs ~ .
stepwiseLogit(formula,
              data=mtcars,
              include="gear",
              selection="bidirection",
              select="AIC",
              sle=0.15,
              sls=0.15,
              sigMethod="Rao")
type="logit"
strategy="bidirection"
metric="AIC"
weights=NULL
y_name="vs"
include="gear"
x_name <- setdiff(getXname(formula,data),include)
y_name <- getYname(formula,data)
intercept <- getIntercept(formula,data,type="logit")

## get intial stepwise model
out_init_stepwise <- getInitialStepwise(data,type,strategy,metric,weights,x_name,y_name,intercept,include,method=test_method_logit)
add_or_remove <- out_init_stepwise$add_or_remove
x_in_model <- out_init_stepwise$x_in_model
x_notin_model <- out_init_stepwise$x_notin_model
best_point <- out_init_stepwise$best_point

## get final stepwise model
out_final_stepwise <- getFinalStepModel(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method_cox,test_method_linear,test_method_logit)
x_in_model <- out_final_stepwise$x_in_model
best_point <- out_final_stepwise$best_point

## get table3-table5
table3 <- formatTable(best_point, tbl_name = "Table 3. Process of Selection")
table4 <- getTBLFianlVariable(include,x_in_model)
table5 <- getTBLCoefModel(type,intercept,include,x_in_model,y_name,n_y=2,data,test_method_cox)

list(table3,table4,table5)



library(survival)
?stepwiseCox
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
data <- my.data
formula = Surv(time, status1) ~ . - status 

stepwiseCox(formula=formula,
            data=my.data,
            include="sex",
            selection="bidirection",
            select="HQ",
            method="efron")

type="cox"
strategy="bidirection"
metric="HQ"
weights=NULL
y_name="Surv(time, status1)"
include="sex"
x_name <- setdiff(getXname(formula,data),include)
y_name <- getYname(formula,data)
intercept <- getIntercept(formula,data,type="cox")


## get intial stepwise model
out_init_stepwise <- getInitialStepwise(data,type,strategy,metric,weights,x_name,y_name,intercept,include,method=test_method_cox)
add_or_remove <- out_init_stepwise$add_or_remove
x_in_model <- out_init_stepwise$x_in_model
x_notin_model <- out_init_stepwise$x_notin_model
best_point <- out_init_stepwise$best_point

## get final stepwise model
out_final_stepwise <- getFinalStepModel(add_or_remove,data,type,metric,weights,y_name,x_in_model,x_notin_model,intercept, include,test_method_cox,test_method_linear,test_method_logit)
x_in_model <- out_final_stepwise$x_in_model
best_point <- out_final_stepwise$best_point

## get table3-table5
table3 <- formatTable(best_point, tbl_name = "Table 3. Process of Selection")
table4 <- getTBLFianlVariable(include,x_in_model)
table5 <- getTBLCoefModel(type,intercept,include,x_in_model,y_name,n_y=1,data,test_method_cox)

list(table3,table4,table5)

summary(lm(formula,data))
## note1: add_or_remove to add_or_remove = c("add","remove")
## note2: getTable function: use meaningful function name based on key words
## note3: column context and name. juzhong
## note4: Title of each table should be letf duiqi
