# mixedSSA

## Functions

This package currently contains a single function, `update_dist`, that allows the user to update the parameters of their chosen movement distribution from the fitted values of an ISSA model (using `glmmTMB`).

Currently the package supports updating the following step-length distributions:
- gamma
- exponential
- half-normal
- log normal

And the following turn angle distributions:
- Von Mises
- NOT SUPPORTED YET: uniform

## Installing
```r
devtools::install_github("smthfrmn/mixedSSA")
```
