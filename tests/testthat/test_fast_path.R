## The fast candidate evaluators (R/fastUtils.R) must give the same selection
## process as refitting every candidate model with lm()/glm(). The refitting
## path is forced with options(StepReg.fast = FALSE).

comparable <- function(res) {
  strat <- attr(res, "nonhidden")
  list(overview = res$overview, detail = res$detail, performance = res$performance,
       coef = lapply(res[strat], function(s) lapply(s, coef)))
}

both_paths <- function(...) {
  set.seed(1)
  fast <- comparable(stepwise(...))
  old <- getOption("StepReg.fast")
  options(StepReg.fast = FALSE)
  on.exit(options(StepReg.fast = old))
  set.seed(1)
  slow <- comparable(stepwise(...))
  list(fast = fast, slow = slow)
}

test_that("linear models: fast path matches refitting for all metrics and strategies", {
  data(mtcars)
  r <- both_paths(formula = mpg ~ ., data = mtcars, type = "linear",
                  strategy = c("forward", "backward", "bidirection"),
                  metric = c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  r <- both_paths(formula = mpg ~ ., data = mtcars, type = "linear", strategy = "subset",
                  metric = c("AIC", "SL", "adjRsq"), best_n = 2)
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
})

test_that("linear models: no intercept, include, factors and nested effects", {
  data(mtcars)
  mt <- mtcars
  mt$am <- factor(mt$am); mt$cyl <- factor(mt$cyl)
  r <- both_paths(formula = mpg ~ . + 0, data = mtcars, type = "linear",
                  strategy = c("forward", "bidirection"), metric = c("AIC", "SL", "adjRsq"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  r <- both_paths(formula = mpg ~ ., data = mtcars, type = "linear", include = c("wt", "hp"),
                  strategy = c("forward", "backward"), metric = c("AIC", "SL"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  r <- both_paths(formula = mpg ~ ., data = mt, type = "linear",
                  strategy = c("forward", "backward", "bidirection"), metric = c("AIC", "SL"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  r <- both_paths(formula = mpg ~ am + cyl + wt:am + disp:am + hp:am, data = mt, type = "linear",
                  strategy = c("forward", "bidirection"), metric = c("AIC", "SL"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
})

test_that("multivariate linear models: information criteria use the fast path, SL tests match", {
  data(mtcars)
  r <- both_paths(formula = cbind(mpg, drat) ~ . + 0, data = mtcars, type = "linear",
                  strategy = c("forward", "backward", "bidirection"), metric = c("AIC", "SL", "SBC"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
})

test_that("logistic, poisson and gamma models: fast path matches refitting", {
  data(remission)
  r <- both_paths(formula = remiss ~ ., data = remission, type = "logit",
                  strategy = c("forward", "backward", "bidirection"),
                  metric = c("SL", "AIC", "AICc", "SBC", "HQ", "IC(3/2)", "IC(1)"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  r <- both_paths(formula = remiss ~ ., data = remission, type = "logit",
                  strategy = c("forward", "bidirection"), metric = "SL", test_method_glm = "LRT")
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  r <- both_paths(formula = remiss ~ ., data = remission, type = "logit", strategy = "subset",
                  metric = c("AIC", "SL"), best_n = 2)
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  data(affairs)
  r <- both_paths(formula = affairs ~ ., data = affairs, type = "poisson",
                  strategy = c("forward", "backward"), metric = c("SL", "AIC"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  data(mtcars)
  r <- both_paths(formula = mpg ~ ., data = mtcars, type = "gamma",
                  strategy = c("forward", "backward"), metric = "AIC")
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
})

test_that("first-step statistics agree with anova() on refitted models", {
  data(remission)
  res <- stepwise(remiss ~ ., data = remission, type = "logit", strategy = "forward", metric = "SL")
  step1 <- res$detail$forward$SL
  step1 <- step1[step1$step == 2, ]
  null_fit <- glm(remiss ~ 1, data = remission, family = binomial)
  p_anova <- sapply(step1$variable, function(v) {
    anova(null_fit, glm(reformulate(v, "remiss"), data = remission, family = binomial), test = "Rao")[2, "Pr(>Chi)"]
  })
  expect_equal(unname(step1$value), unname(p_anova), tolerance = 1e-8)

  data(mtcars)
  res <- stepwise(mpg ~ ., data = mtcars, type = "linear", strategy = "forward", metric = "SL")
  step1 <- res$detail$forward$SL
  step1 <- step1[step1$step == 2, ]
  null_fit <- lm(mpg ~ 1, data = mtcars)
  p_anova <- sapply(step1$variable, function(v) {
    anova(null_fit, lm(reformulate(v, "mpg"), data = mtcars))[2, "Pr(>F)"]
  })
  expect_equal(unname(step1$value), unname(p_anova), tolerance = 1e-8)
})

test_that("backward elimination can remove the last variable and Gamma SL works on both directions", {
  set.seed(7)
  d <- data.frame(y = rnorm(60), a = rnorm(60), b = rnorm(60))
  res <- stepwise(y ~ a + b, data = d, type = "linear", strategy = "backward", metric = "SL", sls = 0.05)
  expect_equal(nrow(res$overview$backward$SL), 3)
  data(mtcars)
  res <- stepwise(mpg ~ ., data = mtcars, type = "gamma", strategy = "bidirection", metric = "SL")
  expect_s3_class(res$bidirection$SL, "glm")
})

test_that("weighted linear models use the weighted residual sum of squares", {
  data(mtcars)
  w <- (1:32) / 32
  r <- both_paths(formula = mpg ~ ., data = mtcars, type = "linear", weight = w,
                  strategy = c("forward", "backward", "bidirection"), metric = c("AIC", "SL", "adjRsq", "SBC"))
  expect_equal(r$fast, r$slow, tolerance = 1e-8)
  res <- stepwise(mpg ~ ., data = mtcars, type = "linear", strategy = "forward", metric = "AIC", weight = w)
  ## first row of the overview is the intercept-only model: n log(SSE/n) + 2p + nY(nY + 1) + n with the deviance as SSE
  dev0 <- deviance(lm(mpg ~ 1, data = mtcars, weights = w))
  expect_equal(as.numeric(res$overview$forward$AIC$AIC[1]), 32 * log(dev0 / 32) + 2 + 2 + 32, tolerance = 1e-6)
})

test_that("negative binomial regression with metric = 'SL' uses a score test for entry", {
  data(affairs)
  res <- stepwise(affairs ~ ., data = affairs, type = "negbin", strategy = "forward", metric = "SL")
  expect_s3_class(res$forward$SL, "negbin")
  step1 <- res$detail$forward$SL
  step1 <- step1[step1$step == 2, ]
  ## score test with theta held at the intercept-only estimate: weighted least
  ## squares of the working residuals on the candidate design
  fit0 <- MASS::glm.nb(affairs ~ 1, data = affairs)
  r <- fit0$residuals; w <- fit0$weights
  nulldev <- sum(w * (r - sum(w * r) / sum(w))^2)
  p_score <- sapply(step1$variable, function(v) {
    X <- model.matrix(reformulate(v), affairs)
    score <- nulldev - sum(w * lm.wfit(X, r, w)$residuals^2)
    pchisq(score, ncol(X) - 1, lower.tail = FALSE)
  })
  expect_equal(unname(step1$value), unname(p_score), tolerance = 1e-6)
  res <- stepwise(affairs ~ ., data = affairs, type = "negbin", strategy = "bidirection", metric = "SL", test_method_glm = "LRT")
  expect_s3_class(res$bidirection$SL, "negbin")
})

test_that("tied p-values are broken by the largest test statistic", {
  set.seed(11)
  n <- 8000
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$yb <- rbinom(n, 1, plogis(4 * d$x1 + 3.5 * d$x2))
  res <- suppressWarnings(stepwise(yb ~ ., data = d, type = "logit", strategy = "forward", metric = "SL"))
  expect_equal(res$overview$forward$SL$EffectEntered, c("1", "x1", "x2"))
})

test_that("feature_ratio = 1 is deterministic and leaves the random number generator alone", {
  data(mtcars)
  set.seed(1); a <- runif(1)
  set.seed(1); invisible(stepwise(mpg ~ ., data = mtcars, strategy = "forward", metric = "AIC")); b <- runif(1)
  expect_identical(a, b)
  set.seed(5); r1 <- stepwise(mpg ~ ., data = mtcars, strategy = "bidirection", metric = "SL")$detail
  set.seed(6); r2 <- stepwise(mpg ~ ., data = mtcars, strategy = "bidirection", metric = "SL")$detail
  expect_identical(r1, r2)
})
