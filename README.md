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

- There is one vignette contained within this package that includes simple examples of how to fit an ISSA using `glmmTMB` and interpret and plot the fitted model using this package. To view this vignette, run `vignette("using_mixedssa", package = "mixedSSA")`. But make sure you installed `mixedSSA` as instructed below in order for this to work.
- It may also be helpful to look at the vignettes association with Fieberg Lab's methods paper: [Modelling individual variability in habitat selection and movement using integrated step-selection analysis](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14321):
  - [Using MixedSSA to Update Movement Distribution Parameters
from ISSA models](https://conservancy.umn.edu/server/api/core/bitstreams/5a2245ec-b285-4b2b-b749-e126165e168d/content)
  - [Delta-method for standard error calculation](https://conservancy.umn.edu/server/api/core/bitstreams/af84ac27-d03a-47d9-92eb-d4e82c7490ae/content)

## Installing
```r
devtools::install_github("smthfrmn/mixedSSA", build_vignettes = TRUE)
```
