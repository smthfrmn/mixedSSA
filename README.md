# mixedSSA

## Functions

This package currently contains a two functions, `update_dist` and `plot_updated_dist`, that allows the user to update and plot the parameters of their chosen movement distribution from the fitted values of an ISSA model (using `glmmTMB`).

Currently the package supports updating the following step-length distributions:
- gamma
- exponential
- half-normal
- log normal

And the following turn angle distributions:
- Von Mises
- NOT SUPPORTED YET: uniform

## Vignettes

[Using MixedSSA to Update Movement Distribution Parameters
from ISSA models](https://conservancy.umn.edu/server/api/core/bitstreams/5a2245ec-b285-4b2b-b749-e126165e168d/content): This vignette is paired with the methods paper [Modelling individual variability in habitat selection and movement using integrated step-selection analysis](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14321) 

## Installing
```r
devtools::install_github("smthfrmn/mixedSSA", timeout = 400)   # add the timeout variable if the package fails to download
```
