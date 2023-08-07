# Function name: camel
# parameter name: snake
# other future package: snake


library(StepReg)

stepwise()

devtools::load_all("~/GitHub/StepReg") # if we have wrapped function, must use load_all to refresh the function names (lazy loading) for wrapper function to use the updated one, if you directed call the wrapped function, it is okay
?stepwise 

data(mtcars)
mtcars$yes <- mtcars$wt
formula <- cbind(mpg,drat) ~ . + 0
devtools::load_all("~/GitHub/StepReg")
res <- stepwise(formula=formula,
				 data=mtcars,
				 strategy="bidirection",
				 metric="AIC")


my_data <- data.frame(x = 1:3, y = c("a", "b", "c"), z = c(TRUE, FALSE, TRUE))

char_cols <- sapply(my_data, class) == "character"
my_data[, char_cols] <- lapply(my_data[, char_cols], function(x) as.numeric(as.factor(x)))
