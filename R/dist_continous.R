DEFAULT_QUANTILES <- c(0.05, 0.5, 0.75, 0.95)


#' @import assertive
validate_continuous_args <- function(quantiles) {
  if (any(quantiles > 1 | quantiles < 0)) {
    stop("argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)")
  }
}


get_quantiles_coef_values <- function(interaction_data, quantiles,
                                      target_coefs) {

  quantile_multipliers <- stats::quantile(interaction_data, probs = quantiles, na.rm = T)
  names(quantile_multipliers) <- NULL

  quantile_coef_values <- target_coefs[,1] + (target_coefs[,2] * quantile_multipliers)
  return(quantile_coef_values)
}


#' @import tibble
get_quantile_coefs <- function(interaction_data, coefs, coef_name, interaction_var_name, quantiles) {
  target_coefs <- get_coefs(
    coefs = coefs,
    coef_name = coef_name,
    interaction_var_name = interaction_var_name
  )


  nrows <- length(quantiles)

  coef_name_vector <- rep(coef_name, nrows)

  args_tibble <- tibble(
    quantile = quantiles,
    coef_name = coef_name_vector,
    coef_value = get_quantiles_coef_values(
      interaction_data, quantiles,
      target_coefs
    )
  )

  row.names(args_tibble) <- NULL

  return(args_tibble)
}


#' @import tibble
get_quantile_coefs_all <- function(interaction_data, coefs, coef_names,
                                   interaction_var_name, quantiles) {
  quantile_coefs_tibble <- tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    quantile_coefs_tibble <- rbind(
      quantile_coefs_tibble,
      get_quantile_coefs(
        interaction_data = interaction_data,
        coefs = coefs,
        coef_name = coef_name,
        interaction_var_name = interaction_var_name,
        quantiles = quantiles
      )
    )
  }

  return(quantile_coefs_tibble)
}



#' Update movement distributions based on fitted models
#'
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_dist_by_continuous_var <- function(model,
                                          dist_name,
                                          random_effects_var,
                                          interaction_var_name,
                                          coef_names,
                                          quantiles) {
  validate_continuous_args(
    quantiles = quantiles
  )

  coefs <- get_coefs_from_model(model = model,
                                random_effects_var_name = random_effects_var)

  quantile_coefs_tibble <- get_quantile_coefs_all(
    interaction_data = model$frame[[interaction_var_name]],
    coefs = coefs,
    coef_names = coef_names,
    interaction_var_name = interaction_var_name,
    quantiles = quantiles
  )

  movement_coef_name <- coef_names[1]
  updated_parameters <- get_updated_parameters(
    model = model,
    movement_coef_name = movement_coef_name,
    dist_name = dist_name,
    coefs_tibble = quantile_coefs_tibble,
    grouping = "quantile"
  )

  return(updated_parameters)
}
