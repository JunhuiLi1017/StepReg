# StepReg NEWS

## Version 1.6.8 (2026-09-20)

### Performance

- **NEW**: The design matrix is built once per `stepwise()` call and candidate models are evaluated on column subsets of it (`R/fastUtils.R`) instead of refitting a complete `lm()`/`glm()` per candidate and step.
  - linear models: all candidates of a step are scored from one QR decomposition of the current model (entry: orthogonalised candidate columns; removal: closed form from the current coefficients and (X'X)^-1).
  - glm (logit, poisson, gamma): candidates are fitted with `glm.fit()` on the prebuilt matrix; with `metric = "SL"` the Rao score test for entry is computed from the current fit alone (as `anova.glm()` does) and the Wald test for removal from the current coefficient table (as `summary.glm()` does), so no candidate refits are needed.
  - the current model is carried from step to step instead of being refitted (all model types, including Cox and negative binomial).
  - the reduced model of the subset strategy with `metric = "SL"` is fitted once instead of once per subset size.
  - The selection results are unchanged; a regression test (`tests/testthat/test_fast_path.R`) compares the new evaluators with the refitting path, which can still be forced with `options(StepReg.fast = FALSE)`. The refitting path is used automatically for Cox and negative binomial models, weighted linear models, data with missing values, multivariate responses with `metric = "SL"`, and factor codings that depend on the terms in the model.

### Dependencies

- **REMOVED**: `pROC` from Imports; the AUC in `performance()` is computed internally (Mann-Whitney form, identical to the trapezoidal ROC AUC). AUC values below 0.5 are now reported as such instead of being flipped by `pROC::roc(direction = "auto")`, and the `performance` data frame stores plain numbers instead of `pROC` objects.
- **MOVED**: `flextable` from Imports to Suggests; it is only used by `report()`, which now stops with an installation hint when the package is missing.
- **REMOVED**: the unused `survAUC::AUC.uno()` and `AUC.hc()` calls in the Cox performance summary (only `auc_sh` was ever reported); `survAUC` is still imported for `AUC.sh()`.

### Bug fixes

- **FIX**: weighted linear regression computed the information criteria (AIC, BIC, SBC, CP, ...) from the unweighted residuals while `sigma_value` and adjusted R-squared were weighted. The weighted residual sum of squares (the deviance, as SAS does with a WEIGHT statement) is now used, with n counting the observations with non-zero weight; `metric = "SL"` and `"adjRsq"` were already weighted and are unchanged.
- **FIX**: negative binomial regression with `metric = "SL"` failed for `strategy = "forward"`/`"bidirection"` ("incorrect number of dimensions"). The entry test is now a Rao score test with theta held at its estimate in the current model (`test_method_glm = "Rao"`) or the likelihood ratio test of `MASS::anova.negbin()` (`"LRT"`).
- **FIX**: when several candidates had exactly the same p-value (typically underflow to 0 on large data sets), the tie was broken with `which.max()` on statistics that `anova(candidate, current)` reports with a negative sign, so the weakest candidate of all was chosen; with two strong predictors forward selection could stop at the intercept-only model. The tie is now broken by the largest test statistic among the tied candidates.
- **FIX**: with the default `feature_ratio = 1`, `stepwise()` still called `sample()` at every step, which permuted the candidates (results in tied cases depended on the random number generator state) and advanced the user's random number generator. Candidates are now sampled only when `feature_ratio < 1`.
- **FIX**: when a variable was removed and immediately re-entered, the last step was dropped from the overview table but not from the detail table, which made `stepwise()` fail with "replacement has ... rows".
- **FIX**: `c-index_test` for Cox models was computed with the wrong direction (`concordance()` without `reverse = TRUE`) and reported 1 minus the correct value.
- **FIX**: `accuracy_test` for logistic models used inverted class labels (`ifelse(p > 0.5, 0, 1)`).
- **FIX**: backward/bidirection elimination of a linear model no longer fails with "argument is of length zero" when the last variable is removed and only the intercept remains.
- **FIX**: Gamma regression with `metric = "SL"` and `strategy = "backward"`/`"bidirection"` no longer fails ("subscript out of bounds"); the Wald test uses t-based p-values as `summary.glm()` does for families with an estimated dispersion.
- **FIX**: removed a stray top-level `?data.frame` call from `R/stepwiseUtils.R`.

## Version 1.6.2 (2025-10-30)
- **UPDATE**: Removed "+ 0" , "-1", and "+1" in final cox regression model


## Version 1.6.1 (2025-10-22)

### Major Changes

- **BREAKING CHANGE**: Shiny application components moved to separate StepRegShiny package
- **REMOVED**: `StepRegShinyApp()` function and all Shiny-related dependencies
- **REMOVED**: `inst/shiny/` directory and all Shiny application files
- **UPDATED**: Package description to reference companion StepRegShiny package
- **CLEANED**: Removed Shiny-related imports from NAMESPACE and DESCRIPTION

### Migration Guide

Users who were using the Shiny application should now install the separate StepRegShiny package:

```r
# Install the new Shiny package
install.packages("StepRegShiny")

# Use the Shiny application
StepRegShiny::StepRegGUI()
```

The core StepReg functionality remains unchanged. Only the Shiny interface has been moved to a separate package for better modularity and independent development.

## Version 1.6.0 (2025-09-29)

### New Features

- **Strata Variables for Cox Regression**: Added support for `strata()` function in Cox regression formulas. This allows users to fit stratified Cox models where separate baseline hazard functions are estimated for different groups while sharing regression coefficients across strata.

- **Continuous-Nested-Within-Class Effects**: Added support for continuous-nested-within-class effects using the `:` operator in formulas. This allows modeling how continuous variables' effects vary across different levels of categorical variables.

 - **train and test validation**: this feature is used for valid inference when `test_ratio` is set between 0-1.

- **feature ratio**:  Proportion of candidate features sampled uniformly at random during forward selection (default = 1). This randomized selection helps identify the best variables while reducing the risk of overfitting, and is only valid when strategy is "forward"..


### Enhancements

- Updated validation functions to properly handle interaction terms with different variable orders (e.g., `X:A` vs `A:X`)
- Enhanced formula parsing to support complex nested effects
- Improved documentation with comprehensive examples for new features
- update vote() to performance() which return a summary of performance of all final models.

### Documentation

- Added new section "Advanced Features" to the vignette with detailed examples
- Updated function documentation with new examples
- Enhanced README with quick start examples for new features
- Updated package description to highlight new capabilities

## Version 1.5.8

- Bug fixes and performance improvements
- Enhanced multicollinearity detection
- Updated documentation

## Version 1.0.0

- Initial CRAN release
- Basic stepwise regression functionality
- Support for linear regression types
- Multiple selection strategies and metrics 