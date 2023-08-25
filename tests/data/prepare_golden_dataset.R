# older one
data(mtcars)
mtcars$yes <- mtcars$wt
formula1 <- mpg ~ . + 1

output_linear_stepwise <- list()
#note1: output_linear_stepwise.Rdata
#data: mtcars$yes <- mtcars$wt
#formula: mpg ~ . + 0
#method: version 1.4.4
for(strategy in c("forward","backward","bidirection")){
  for(metric in c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")){
    message(strategy,metric)
    output_old <- NA
    
    try(output_old <- stepwise(formula=formula1,
                               data=mtcars,
                               selection=strategy,
                               select=metric)[[3]][,c(2,3,7)],silent = TRUE)
    
    output_linear_stepwise[[strategy]][[metric]] <- output_old
  }
}
setwd("~/Dropbox (UMass Medical School)/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/")
saveRDS(output_linear_stepwise,file="output_linear_stepwise.Rdata")

# older one
data(mtcars)
formula2 <- vs ~ .
output_logit_stepwise <- list()
#note1: linearStepwiseAllCombination.Rdata
#data: mtcars$yes <- mtcars$wt
#formula: mpg ~ . + 0
#method: version 1.4.4
for(strategy in c("forward","backward","bidirection")){
  for(metric in c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")){
    message(strategy,"\t",metric)
    
    output_old <- NA
    try(output_old <- stepwiseLogit(formula2,
                                    data=mtcars,
                                    selection=strategy,
                                    select=metric,
                                    sle=0.15,
                                    sls=0.15,
                                    sigMethod="Rao")[[3]][,c(2,3,6)],silent = TRUE)
    
    output_logit_stepwise[[strategy]][[metric]] <- output_old
  }
}
setwd("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data")
saveRDS(output_logit_stepwise,file="output_logit_stepwise.Rdata")

# cox
#note1: output_cox_stepwise.Rdata
#data: lung <- survival::lung %>% na.omit()
#formula: Surv(time, status) ~ .
#method: version 1.4.4

library(StepReg)
library(dplyr)
library(survival)

output_cox_stepwise <- list()
lung <- survival::lung %>% na.omit()
formula3 = Surv(time, status) ~ .

for (strategy in c("forward","backward","bidirection")){
  for (metric in c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")){
    message(strategy,"\t",metric)
    output_old <- NA
    try(output_old <- stepwiseCox(formula = formula3, 
                                  data = lung,
                                  selection = strategy,
                                  select = metric,
                                  method = "efron")[[3]][,c(2,3,5)],silent = TRUE)
    output_cox_stepwise[[strategy]][[metric]] <- output_old
  }
}

setwd("F:/git_xiaohuan/StepReg/tests/data")
saveRDS(output_cox_stepwise,file="output_cox_stepwise.Rdata")
