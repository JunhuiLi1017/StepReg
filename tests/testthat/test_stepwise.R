test_that("test_utils.R failed", {
  library(stringr)
  
  linear_model1 <- mpg ~ . + 1
  linear_model2 <- cbind(mpg, drat) ~ . + 0
  logit_model1 <- remiss ~ .
  cox_model1 <- Surv(time, status) ~ .
  
  #outdir <- "~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data"
  #res_v1_5_0 <- readRDS(paste0(outdir,"/res_v1_5_0.rds"))
  res_v1_5_0 <- readRDS(system.file("tests/data","res_v1_5_0.rds", package = "StepReg"))
  
  #mod=names(res_v1_5_0)[4]
  for (mod in names(res_v1_5_0)){
    type <- unlist(stringr::str_split(mod,"_"))[1]
    if(mod=="cox_model1"){
      lung <- survival::lung %>% na.omit()
      mydata <- lung
    }else if(mod %in% c("linear_model1","linear_model2")){
      data(mtcars)
      mtcars$yes <- mtcars$wt
      mydata <- mtcars
    }else if(mod %in% "logit_model1"){
      mydata <- read.table("cancer_remission.csv",sep=",",header=T)
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
      #metric="SL"
      for(metric in names(res_v1_5_0[[mod]][[strategy]])){
        message(mod,"\t",strategy,"\t",metric)
        output_new <- NA
        output_old <- res_v1_5_0[[mod]][[strategy]][[metric]][,select_col1]
        
        try(output_new <- stepwise1(type = type,
                                          formula=get(mod),
                                          data=mydata,
                                          strategy="backward",
                                          metric="SL",
                                    sle=0.05,sls=0.05)[[3]],silent = TRUE)
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

library(My.stepwise)
?glm
?mtcars
glm.D94 <- glm(am ~ gear + qsec,data =mtcars, family = "binomial")
glm.D93 <- glm(vs ~ cyl,data =mtcars, family = "binomial")
glm.D92 <- glm(vs ~ qsec,data =mtcars, family = "binomial")
glm.D91 <- glm(am ~ cyl,data =mtcars, family = "binomial")
glm.D90 <- glm(vs ~ 1,data =mtcars, family = "binomial")
fit1 <- anova(glm.D92,glm.D93)

data(mtcars)
model0 <- glm(vs ~ 1,data =mtcars, family = "binomial")
model1 <- glm(vs ~ cyl,data =mtcars, family = "binomial")
model2 <- glm(vs ~ cyl + qsec,data =mtcars, family = "binomial")


summary(glm.D93)
model3 <- glm(am ~ qsec,data =mtcars, family = "binomial")
allvar <- colnames(mtcars)
mtcars <- mtcars[,!colnames(mtcars) %in% "gear"]

pset <- rep(0,length(allvar[!allvar %in% c("vs")]))
names(pset) <- allvar[!allvar %in% c("vs")]
for(i in allvar[!allvar %in% c("vs")]){
  formu <- as.formula(paste0("vs ~ 1 +",i))
  model2 <- glm(formu,data =mtcars, family = "binomial")
  pset[i] <- anova(model0,model2,test="Rao")[2,"Pr(>Chi)"]
}
anova(model2,test="Rao")

summary(model1)
anova(model1,model2,test="Rao")


library(car)
model1 <- glm(vs ~ 1 + cyl,data =mtcars, family = "binomial")
summary(model1)
Anova(model1, type="III", test="Wald")

model2 <- glm(vs ~ 1 + cyl + qsec,data =mtcars, family = "binomial")

summary(model2)
Anova(model2, type="III", test="Wald")




?glm

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


setwd("~/dropbox/Project/UMMS/Github/JunhuiLi1017/StepReg/tests/data/")
dat <- read.table("cancer_remission.csv",sep=",",header=TRUE)
dim(dat)

dat1 <- data.frame(dat,v2)
model0 <- glm(remiss ~ 1,data =dat, family = "binomial")
model1 <- glm(remiss ~ 1 + li ,data =dat, family = "binomial")
model2 <- glm(remiss ~ 1 + li + v2,data =dat1, family = "binomial")


model0 <- glm(remiss ~ 1,data =dat, family = "binomial")
model1 <- glm(remiss ~ 1 + li ,data =dat, family = "binomial")

anova(model1,model2,test="Rao")

summary(model2)
Anova(model2, type="III", test="Wald")




mydata <- dat
model0 <- glm(remiss ~ 1,data =mydata, family = "binomial")
model1 <- glm(remiss ~ 1 + li,data =mydata, family = "binomial")
model2 <- glm(remiss ~ 1 + li + temp,data =mydata, family = "binomial")
allvar <- colnames(mydata)

pset <- rep(0,length(allvar[!allvar %in% c("remiss","li")]))
names(pset) <- allvar[!allvar %in% c("remiss","li")]
for(i in allvar[!allvar %in% c("remiss","li")]){
  formu <- as.formula(paste0("remiss ~ 1 + li + ",i))
  model_temp <- glm(formu,data =mydata, family = "binomial")
  pset[i] <- anova(model1,model_temp,test="Rao")[2,"Pr(>Chi)"]
}

stat_table <- coef(summary(model2))
stat_table <- coef(summary(fit_reduced))


f_pic_vec <- getAnovaStat(add_or_remove="remove", intercept="1", fit_full=model2,type="logit",test_method="Rao")
pic_set <- f_pic_vec[2]
f_set <- f_pic_vec[1]
var_set <- f_pic_vec[3]
names(pic_set) <- var_set
names(f_set) <- var_set







output_new <- stepwise1(type = type,
                        formula=get(mod),
                        data=mydata,
                        strategy=strategy,
                        metric=metric)













