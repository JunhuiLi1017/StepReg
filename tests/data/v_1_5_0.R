# obtain gold res using v1.5.0
# output (res_v1_5_0) contains results from all combinations using the following models:
# linear_model1: mpg ~ . + 0 (mtcars)
# linear_model2: cbind(mpg, drat) ~ . + 1 (mtcars)
# logit_model1: vs ~ . (mtcars)
# cox_model1:  Surv(time, status) ~ . (lung)
# output is a list which can be queried as: res_v1_5_0[["linear_model1"]][["forward"]][["AIC"]] (res_v1_5_0[[model]][[strategy]][[metric]])

library(StepReg)
library(dplyr)
library(stringr)

# models to test
data(mtcars)
mtcars$yes <- mtcars$wt
lung <- survival::lung %>% na.omit()

linear_model1 <- mpg ~ . + 1
linear_model2 <- cbind(mpg, drat) ~ . + 0
logit_model1 <- vs ~ .
cox_model1 <- Surv(time, status) ~ .

# result list name
res_v1_5_0 <- list()

# allowed metrics for different types
linear_metric <- c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")
logit_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
cox_metric <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")

#model="linear_model2"
for (model in c("linear_model1", "linear_model2", "logit_model1", "cox_model1")) {
  # 1: obtain type
  type <- str_split(model, "_")[[1]][1]
  
  # 2: obtain corresponding test function according to type
  test_func <- switch(type,
                      "linear" = stepwise,
                      "logit" = stepwiseLogit,
                      "cox" = stepwiseCox
  )
  
  # 3: obtain corresponding dataset according to model 
  dataset <- switch(model,
                    "linear_model1" = mtcars,
                    "linear_model2" = mtcars,
                    "logit_model1" = mtcars,
                    "cox_model1" = lung
  )
  
  # 4: obtain metrics according to type
  linear_metrics <- c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")
  logit_metrics <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
  cox_metrics <- c("SL", "AIC", "AICc", "SBC", "HQ", "HQc", "IC(3/2)", "IC(1)")
  metrics <- switch(type,
                    "linear" = linear_metrics,
                    "logit" = logit_metrics,
                    "cox" = cox_metrics)
  
  # 5: perform test
  #strategy="forward"
  for (strategy in c("forward", "backward", "bidirection", "subset")) {
    if (type == "linear" && strategy == "subset") {
      metrics <- metrics[metrics != "SL"]
    }
    if (model == "linear_model2") {
      metrics <- metrics[!metrics %in% c("BIC","CP","Rsq","adjRsq")]
    }
    #metric="BIC"
    for (metric in metrics) {
      message(paste0(model, ":", strategy, ":", metric))
      res <- NA
      try(res <- stepwise1(formula = get(model),
                           data = dataset,
                           type=type,
                           strategy = strategy,
                           metric = metric)[[3]], silent = TRUE)
      message(length(res))
      res_v1_5_0[[model]][[strategy]][[metric]] <- res
    }
  }
}

#outdir <- system.file("R/tests/data", package = "StepReg")
outdir <- system.file("tests/data", package = "StepReg")
#outdir <- "~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data"
saveRDS(res_v1_5_0, paste0(outdir, "/res_v1_5_0.rds"))
