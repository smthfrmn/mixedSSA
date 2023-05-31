# movement-utilities

## Functions

This package currently contains two functions that allow the user to update the parameters of their chosen movement distribution from the fitted values of an iSSF model (using `glmmTMB`).

Currently the package supports updating the following step-length distributions:
- gamma
- exponential
- half-normal
- log normal

And the following turn angle distributions:
- Von Mises
- NOT SUPPORTED YET, BUT ADDING CURRENTLY: uniform

### Categorical
A user can update a given distribution with fitted values from a model fit to a categorical covariate with an interaction with movement parameters, step-lengths and turn-angles.

**Function Name:** `update_distributions_by_categorical_var`

**Args:**
- `model`: an iSSF model fit using `glmmTMB`
- `dist_name`: the distribution to be updated, valid values match those in `amt` and are `gamma`, `exp`, `hnorm`, `lnorm`, and `vonmises` (`uniform` coming shortly).
- `interaction_var_name`: the name of the categorical variable whose interaction coefficients will be used to update the distribution
- `coef_names`: the names (in order! this is the diciest part of the ux I think...) of the movement parameters in the fitted model. Note: if the default `amt` nomenclature is used when fitting this model (e.g. `sl_`, `cos_ta_`, etc.), the user does not need to pass this argument (this also might be dicey, probably better to just make it explicit...).

**Returns:** A tibble (currently changing this to return an object of a custom class that can be passed to other functions, like plotting etc) containing one row with the "observed" distribution parameters (i.e. the distribution parameters determined from fitting the given movement distribution to the movement data, step lengths or turn angles) and one row for each of the categories, containing the updated distribution parameters _and_ the values passed to the `amt` update distribution functions.

**Example:**

Using the fisher data (`fisher_data`) from `amt`, let's suppose we have a model like so,

```r
model <- glmmTMB(case_ ~ sl_ + log_sl_ + sl_:sex + log_sl_:sex, data = fisher_data)
updated_steps_dist <- update_distributions_by_categorical_var(
  model = model,
  dist_name = "gamma",
  interaction_var_name = "sex",
  coef_names = c("sl_", "log_sl_")
)

print(updated_steps_dist)

>>  category  beta_sl beta_log_sl    shape    scale
1 observed       NA          NA 0.626594 358.7777
2        F -1.4e-05     0.00015 0.626744 356.9791
3        M -1.0e-06    -0.00050 0.626093 358.6590
```


### Continuous

A user can also update a given distribution with fitted values from a model fit to a continuous covariate with an interaction with movement parameters, step-lengths and turn-angles.

**Function Name:** `update_distributions_by_continuous_var`

**Args:**
- `model`: an iSSF model fit using `glmmTMB`
- `dist_name`: the distribution to be updated, valid values match those in `amt` and are `gamma`, `exp`, `hnorm`, `lnorm`, and `vonmises` (`uniform` coming shortly).
- `interaction_var_name`: the name of the continuous variable whose interaction coefficients will be used to update the distribution
- `quantiles`: the quantiles of the interaction variable to be used to update the distribution. This is an optional argument that defaults to `c(0.05, 0.5, 0.75, 0.95)` if the user doesn't pass anything.  
- `coef_names`: the names (in order! this is the diciest part of the ux I think...) of the movement parameters in the fitted model. Note: if the default `amt` nomenclature is used when fitting this model (e.g. `sl_`, `cos_ta_`, etc.), the user does not need to pass this argument (this also might be dicey, probably better to just make it explicit...).

**Returns:** A tibble (currently changing this to return an object of a custom class that can be passed to other functions, like plotting etc) containing one row with the "observed" distribution parameters (i.e. the distribution parameters determined from fitting the given movement distribution to the movement data, step lengths or turn angles) and one row for each of the quantiles, containing the updated distribution parameters _and_ the values passed to the `amt` update distribution functions.

**Example:**

Using the fisher data (`fisher_data`) from `amt`, let's suppose we have a model like so,

```r
model <- glmmTMB(case_ ~ sl_ + log_sl_ + sl_:elevation + log_sl_:elevation, data = fisher_data)
updated_ta_dist <- update_distributions_by_continuous_var(
  model = model,
  dist_name = "vonmises",
  interaction_var_name = "elevation",
  quantiles = c(0.5, 0.75, 0.95),
  coef_names = c("cos_ta_")
)

print(updated_ta_dist)

>>  quantile beta_cos_ta    kappa mu
1 observed          NA 1.647097  0
2      0.5   -0.020313 1.626785  0
3     0.75   -0.017580 1.629518  0
4     0.95   -0.012400 1.634697  0
```

## Installing

Since this is a private repo, there are two ways to install, using a github personal access token (PAT) or by downloading the package and installing locally.

**Using a PAT (reccomended):**

Follow this 30 second video for creating a PAT (if you don't know how) and then run the code below: [VIDEO]
```r
devtools::install_github("smthfrmn/movement-utilities", auth_token = "<PAT>")
```

**Installing Locally (less recommended but still totally okay if the PAT way feels too heavy-handed):**
