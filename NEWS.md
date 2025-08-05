# StepReg NEWS

## Version 1.5.9 (2025-08-04)

### New Features

- **Strata Variables for Cox Regression**: Added support for `strata()` function in Cox regression formulas. This allows users to fit stratified Cox models where separate baseline hazard functions are estimated for different groups while sharing regression coefficients across strata.

- **Continuous-Nested-Within-Class Effects**: Added support for continuous-nested-within-class effects using the `:` operator in formulas. This allows modeling how continuous variables' effects vary across different levels of categorical variables.

### Enhancements

- Updated validation functions to properly handle interaction terms with different variable orders (e.g., `X:A` vs `A:X`)
- Enhanced formula parsing to support complex nested effects
- Improved documentation with comprehensive examples for new features

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