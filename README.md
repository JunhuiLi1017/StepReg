# StepReg <a href="https://github.com/JunhuiLi1017/StepReg"><img src="man/figures/logo.png" align="right" height="138" /></a>
---
- #### An R package for stepwise regression analysis
---

StepReg is an R package that streamlines stepwise regression analysis by supporting multiple regression types, incorporating popular selection strategies, and offering essential metrics.

## Key Features

- **Multiple Regression Types**: Linear, logistic, Cox, Poisson, and Gamma regression
- **Selection Strategies**: Forward selection, backward elimination, bidirectional elimination, and best subsets
- **Selection Metrics**: AIC, AICc, BIC, CP, HQ, adjRsq, SL, SBC, IC(3/2), IC(1)
- **Advanced Features**: 
  - Strata variables for Cox regression
  - Continuous-nested-within-class effects
  - multivariable multiple linear stepwise regression
- **Multicollinearity Detection**: Automatic detection and handling of multicollinearity
- **Visualization**: Plot functions for variable selection processes
- **Reporting**: Export results in various formats (HTML, DOCX, XLSX, PPTX)
- **Shiny App**: Interactive web interface for non-programmers

## Installation

### Install from CRAN
```r
pak::pkg_install("StepReg")
```

or

```r
install.packages("StepReg")
```

### Or install from GitHub
```r
devtools::install_github("JunhuiLi1017/StepReg")
```

## Quick Start

```r
library(StepReg)

# Basic linear regression
data(mtcars)
formula <- mpg ~ .
res <- stepwise(
  formula = formula,
  data = mtcars,
  type = "linear",
  strategy = "bidirection",
  metric = "AIC"
)

# View results
res
summary(res$bidirection$AIC)
```

## Advanced Features

### Strata Variables in Cox Regression

```r
library(survival)
data(lung)
lung$sex <- factor(lung$sex)

# Cox regression with strata
formula <- Surv(time, status) ~ age + sex + ph.ecog + strata(inst)
res <- stepwise(
  formula = formula,
  data = lung,
  type = "cox",
  strategy = "forward",
  metric = "AIC"
)
```

### Continuous-Nested-Within-Class Effects

```r
data(mtcars)
mtcars$am <- factor(mtcars$am)

# Nested effects
formula <- mpg ~ am + wt:am + disp:am + hp:am
res <- stepwise(
  formula = formula,
  data = mtcars,
  type = "linear",
  strategy = "bidirection",
  metric = "AIC"
)
```

## Documentation

- [Vignette](https://cran.r-project.org/web/packages/StepReg/vignettes/StepReg.html) - Comprehensive guide with examples
- [Reference Manual](https://cran.r-project.org/web/packages/StepReg/StepReg.pdf) - Function documentation

## Important Note

StepReg should **NOT** be used for statistical inference unless the variable selection process is explicitly accounted for, as it can compromise the validity of the results. This limitation does not apply when StepReg is used for prediction purposes.

## Citation

If you use StepReg in your research, please cite:

```r
citation("StepReg")
```

## Detailed usage
For released version, refer to CRAN [vignettes](https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html).

For development version, refer to this [vignettes](https://mccbbioinfo.github.io/tutorials/StepReg).

## Questions?
Please raise an issue [here](https://github.com/JunhuiLi1017/StepReg/issues/new).
