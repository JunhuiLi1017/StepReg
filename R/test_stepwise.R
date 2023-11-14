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

library(rms)
?stepwiseCox
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwise.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/stepwiseUtils.R")
source("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/R/validateUtils.R")


0.4825/0.1323 

library(rms)
library(survival)
my.data <- read.table("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data/cancer_remission.csv",sep=',',header=T)
formula <- remiss ~ .

my.data <- my.data[1:9,]
m1 <- survival::coxph(Surv(time, status1) ~ 0,data=my.data)
m2 <- survival::coxph(Surv(time, status1) ~ inst,data=my.data)
m3 <- survival::coxph(Surv(time, status1) ~ ph.ecog + inst,data=my.data)
m4 <- survival::coxph(Surv(time, status1) ~ ph.karno,data=my.data)

surv_diff <- survdiff(Surv(time, status1) ~ ph.karno, data = my.data)
surv_diff

4.5/4

summary(m1)
m2a <- summary(m2)
m3a <- summary(m3)
m4a <- summary(m4)
m4a$sctest
m3a$sctest-m2a$sctest

dim(my.data)
m11 <- rms::cph(Surv(time, status1) ~ 1,data=my.data)
m22 <- rms::cph(Surv(time, status1) ~ age,data=my.data)
m33 <- rms::cph(Surv(time, status1) ~ age + meal.cal,data=my.data)

formula=Surv(time, status1) ~ age
data=my.data
m2a <- summary(m22)
m3a <- summary(m33)
anova(m22,m33)
m2a

install.packages("glmglrt")
library(glmglrt)
?ScoreTest
data(diabetes)
ScoreTest(diabetes)


logLik(m2)


m33$loglik
m33$score

anova(m11,m22)


anova(m1,m2,test="")


log(lik1/lik2)=loglik1-loglik2

15.104-13.217
log2(3.7748)


colnames(my.data)

lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)


my.data <- my.data[1:50,]

formula = Surv(time, status1) ~ . - status 

stepwise1(formula = formula,
                data = my.data,
                type = "cox",
                strategy = "backward",
                metric = "SL")
offset<- attr(Terms, "offset")

head(lung)
surv_diff <- survdiff(Surv(time, status) ~ inst, data = lung)
surv_diff
surv_fit <- survival::coxph(Surv(time, status) ~ sex,data=lung)
surv_fit
surv_fit$score
a <- summary(surv_fit)
a$sctest

(112-91.6)^2/91.6
(112-91.6)^2/10.3
(53-73.4)^2/10.3


