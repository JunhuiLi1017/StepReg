
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwise.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwiseUtils.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/validateUtils.R")


  


final_model_poisson_005 <- step(object = model_full_poisson,
                                k = qchisq(p = 0.1, df = 1, lower.tail = FALSE),
                                trace = 0,
                                direction="backward")
summary(final_model_poisson_005)

final_model_poisson_stepreg <- stepwise1(formula = model_full_poisson$formula,
          data = model_full_poisson$model,
          type  =  "poisson",
          strategy = "bidirection",
          metric = "SL",
          sle=0.05,
          sls=0.05,
          include="Meta_Disease")


stepwise1(formula = model_full_poisson$formula,
          data = model_full_poisson$model,
          type  =  "poisson",
          strategy = "backward",
          metric = "SL",
          sle=0.05,
          sls=0.05,
          include="Meta_Disease")


library(StepReg)

test_method_linear <- "Pillai"
test_method_logit <- "Rao"
test_method_cox <- "efron"

## linear stepwise
data(mtcars)
mtcars$yes <- mtcars$wt
formula <- cbind(mpg,drat) ~ . + 0

data = mtcars
strategy = "bidirection"
metric = "SL"
include = NULL
sle = 0.15
sls = 0.15
tolerance = 1e-7
weight = NULL
best_n = Inf
excel_name = NULL
#weight = NULL

stepwise1(formula = formula,
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

a2 <- stepwise1(formula = formula,
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
















##--------------------------
## dataset for linear regression
##--------------------------
data(mtcars)

## build two models: reduced and full model
linear_reduced0 <- lm(mpg ~ 1, data=mtcars)
linear_reduced1 <- lm(mpg ~ 1 + qsec, data=mtcars)
linear_full <- lm(mpg ~ 1 + qsec + hp, data=mtcars)

anova(linear_reduced1,linear_full,test="F")
anova(linear_reduced1,linear_full,test="LRT")

##--------------------------
## dataset for logistic regression
##--------------------------
cancer.data <- read.table("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data/cancer_remission.csv",sep=',',header=T)
dim(cancer.data)
colnames(cancer.data)
head(cancer.data)

## build two models: reduced and full model
logit_reduced0 <- glm(remiss ~ 1, data=cancer.data, family = "binomial")
logit_reduced1 <- glm(remiss ~ 1 + li, data=cancer.data, family = "binomial")
logit_full <- glm(remiss ~ 1 + li + temp, data=cancer.data, family = "binomial")

anova(logit_reduced1,logit_full,test="Rao")
anova(logit_reduced1,logit_full,test="LRT")


##--------------------------
## dataset for survival analysis
##--------------------------
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)

## build two models: reduced and full model
library(survival)
surv_redeuced0 <- survival::coxph(Surv(time, status) ~ 1,data=lung)
surv_redeuced1 <- survival::coxph(Surv(time, status) ~ sex,data=lung)
surv_full <- survival::coxph(Surv(time, status) ~ sex+age,data=lung)

#Rao(Score chi square) is valid for logistic regression; 
# anova only do LRT test no matter what parameter in test=" " 
anova(surv_redeuced1,surv_full,test="Rao")
anova(surv_redeuced1,surv_full,test="F")
anova(surv_redeuced1,surv_full,test="LRT")

#we can find Score (logrank) test in summary(surv_full), but in summary() funcion,
#reduced model is always coxph(formula = Surv(time, status1) ~ 1
summary(surv_redeuced1)   #surv_redeuced1 vs surv_redeuced0
summary(surv_full)   #vs coxph(formula = Surv(time, status1) ~ 1

#full model
#coxph(formula = Surv(time, status1) ~ ph.ecog + inst + 1
#
#reduced model
#coxph(formula = Surv(time, status1) ~ 1
#
# reduced model we are looking for
# coxph(formula = Surv(time, status1) ~ ph.ecog + 1







