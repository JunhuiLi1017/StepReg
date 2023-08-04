library(StepReg)
?stepwise

data(mtcars)
mtcars$yes <- mtcars$wt
formula <- cbind(mpg,drat) ~ . + 0
stepwise(formula=formula,
         data=mtcars,
         selection="bidirection",
         select="AIC")



fit_reduced <- getModel(data=mtcars, type="linear", x_name=c(include,x_in_model), y_name="mpg", weights=weights, intercept=intercept)
fit_full <- getModel(data=mtcars, type="linear", x_name=c(include,x_in_model,"am"), y_name="mpg", weights=weights, intercept=intercept)
a <- anova(fit_reduced, fit_full)
fit_reduced <- getModel(data=mtcars, type="linear", x_name=c(include,x_in_model), y_name="cbind(mpg,drat)", weights=weights, intercept=intercept)
fit_full <- getModel(data=mtcars, type="linear", x_name=c(include,x_in_model,"am"), y_name="cbind(mpg,drat)", weights=weights, intercept=intercept)
anova(fit_reduced, fit_full,test="Pillai")
anova(fit_reduced, fit_full,test="Wilks")
anova(fit_reduced, fit_full,test="Hotelling-Lawley")
anova(fit_reduced, fit_full,test="Roy")[2,"approx F"]


?stepwiseLogit
data(mtcars)
data <- mtcars
formula <- vs ~ .
stepwiseLogit(formula,
              data=mtcars,
              selection="bidirection",
              select="SL",
              sle=0.15,
              sls=0.15,
              sigMethod="Rao")
fit_reduced <- getModel(data=mtcars, type="logit", x_name=c(include,x_in_model), y_name="vs", weights=weights, intercept=intercept)
fit_full <- getModel(data=mtcars, type="logit", x_name=c(include,x_in_model,"am"), y_name="vs", weights=weights, intercept=intercept)
anova(fit_reduced, fit_full, test = "LRT")
anova(fit_reduced, fit_full, test = "Rao")



?stepwiseCox
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
data <- my.data
formula = Surv(time, status1) ~ . - status 

stepwiseCox(formula=formula,
            data=my.data,
            selection="bidirection",
            select="HQ",
            method="efron")

library(survival)
include <- c("ph.karno")
x_in_model <- c("age","sex")
fit_reduced <- getModel(data=data, type="cox", x_name=c(include,x_in_model), y_name="Surv(time, status1)", weights=weights, intercept=intercept)
fit_full <- getModel(data=data, type="cox", x_name=c(include,x_in_model,"ph.ecog"), y_name="Surv(time, status1)", weights=weights, intercept=intercept)
anova(fit_reduced, fit_full, test = "LRT")
anova(fit_reduced, fit_full, test = "")

