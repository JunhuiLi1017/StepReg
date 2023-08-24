# older one
data(mtcars)
mtcars$yes <- mtcars$wt
formula1 <- mpg ~ . + 1

test_data1 <- list()
#note1: linearStepwiseAllCombination.Rdata
#data: mtcars$yes <- mtcars$wt
#formula: mpg ~ . + 0
#method: version 1.4.4
for(strategy in c("forward","backward","bidirection")){
  for(metric in c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")){
    cat(strategy)
    cat(metric)
    output_old <- NA
    
    try(output_old <- stepwise(formula=formula1,
                               data=mtcars,
                               selection=strategy,
                               select=metric)[[3]][,c(2,3,7)],silent = TRUE)
    
    test_data1[[strategy]][[metric]] <- output_old
  }
}
setwd("~/Dropbox (UMass Medical School)/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/")
saveRDS(test_data1,file="test_data1.Rdata")
