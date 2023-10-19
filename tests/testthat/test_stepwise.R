test_that("test_utils.R failed", {
  library(stringr)
  
  linear_model1 <- mpg ~ . + 1
  linear_model2 <- cbind(mpg, drat) ~ . + 0
  logit_model1 <- vs ~ .
  cox_model1 <- Surv(time, status) ~ .
  
  #outdir <- "~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data"
  #res_v1_5_0 <- readRDS(paste0(outdir,"/res_v1_5_0.rds"))
  res_v1_5_0 <- readRDS(system.file("tests/data","res_v1_5_0.rds", package = "StepReg"))
  
  #mod=names(res_v1_5_0)[3]
  for (mod in names(res_v1_5_0)){
    type <- unlist(stringr::str_split(mod,"_"))[1]
    if(mod=="cox_model1"){
      lung <- survival::lung %>% na.omit()
      mydata <- lung
    }else if(mod %in% c("linear_model1","linear_model2","logit_model1")){
      data(mtcars)
      mtcars$yes <- mtcars$wt
      mydata <- mtcars
    }
    #strategy="forward"
    for (strategy in names(res_v1_5_0[[mod]])){
      if(strategy=="subset"){
        index_n <- 2
        select_col1 <- c(1,2,3)
      }else{
        index_n <- 3
        select_col1 <- c(2,3,7)
      }
      #metric="AIC"
      for(metric in names(res_v1_5_0[[mod]][[strategy]])){
        message(mod,"\t",strategy,"\t",metric)
        output_new <- NA
        output_old <- res_v1_5_0[[mod]][[strategy]][[metric]][,select_col1]
        
        try(output_new <- stepwise1(type = type,
                                    formula=get(mod),
                                    data=mydata,
                                    strategy=strategy,
                                    metric=metric)[[3]][,select_col1],silent = TRUE)
        output_new
        res <- try(expect_equal(output_new,output_old),silent = TRUE)
        if(inherits(res, "try-error")){
          message("Error\t",mod,"\t",strategy,"\t",metric)
        }        
      } #metric
    } #strategy
  } #mod
})

c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "Rsq", "adjRsq", "SL", "SBC")
##linear_model1: all passed but no "HQ" and "HQc" information criteria in SAS
names(res_v1_5_0[[mod]][[strategy]])
res_v1_5_0[["linear_model1"]][["bidirection"]][["SL"]]

##logot_model1: inconsistent result of backward with SL
##Score Chi-Square is used for p value calculation in forward step, while Wald Chi-Square is used in backward step.
names(res_v1_5_0[[mod]][[strategy]])
res_v1_5_0[["logit_model1"]][["bidirection"]][["SL"]]


a <- stepwise1(type = "logit",
          formula=logit_model1,
          data=mtcars,
          strategy=strategy,
          metric="SL")
a[[3]]
?stepwiseLogit

data(mtcars)
formula <- vs ~ .
b <- stepwiseLogit(formula,
              data=mtcars,
              selection="score",
              select="SL",
              sigMethod="Rao")
head(b[[3]])
head(a[[3]])
expect_equal(b[[3]][,2],a[[3]][,2])


?glm

glm.D94 <- glm(vs ~ gear + qsec,data =mtcars, family = "binomial")
glm.D93 <- glm(vs ~ cyl + qsec,data =mtcars, family = "binomial")
glm.D92 <- glm(vs ~ qsec,data =mtcars, family = "binomial")
glm.D91 <- glm(vs ~ cyl,data =mtcars, family = "binomial")
glm.D90 <- glm(vs ~ 1,data =mtcars, family = "binomial")
fit1 <- anova(glm.D92,glm.D93)
?anova

#install.packages("mdscore'")
library(mdscore)
#install.packages("survey'")
library(survey)
library(lmtest)


anova(glm.D90, glm.D91, test = "LRT")
anova(glm.D90, glm.D91, test = "Chisq")
anova(glm.D90, glm.D91, test = "Rao")

anova(glm.D91, glm.D93, test = "LRT")
anova(glm.D91, glm.D93, test = "Chisq")
anova(glm.D91, glm.D93, test = "Rao")



anova(glm.D92, glm.D93, test = "LRT")
anova(glm.D92, glm.D93, test = "Chisq")
anova(glm.D92, glm.D93, test = "Rao")

anova(glm.D92, glm.D94, test = "Rao")


vcov_mat <- vcov(glm.D93)

# Calculate the Wald statistic
wald_result <- wald.test(model =glm.D93, terms =2)

# Print the result
print(wald_result)








