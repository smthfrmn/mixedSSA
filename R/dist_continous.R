get_quantiles_coef_values <- function(quantilees, interaction_coef_values, coef_value_vector) {
  # TODO: todo...
  return(quantiles)
}


#' @import tibble
get_quantile_coefs <- function(coefs, coef_name, interaction_var_name, quantiles) {
  interaction_coefs <- coefs[grepl(str_interp("^${coef_name}:${interaction_var_name}"), names(coefs))]

  interaction_coef_values <- unname(interaction_coefs)
  nrows <- length(interaction_coef_values)

  coef_name_vector <- rep(coef_name, nrows)
  coef_value_vector <- rep(coefs[coef_name], nrows)

  args_tibble <- tibble(
    quantile = quantiles,
    coef_name = coef_name_vector,
    coef_value = get_quantiles_coef_values(quantiles, interaction_coef_values, coef_value_vector)
  )

  row.names(args_tibble) <- NULL

  return(args_tibble)
}


#' @import assertive
validate_continuous_args <- function(data, model, dist_name, interaction_var_name, coef_names, quantiles) {
  validate_base_args(data, model, dist_name, coef_names, interaction_var_name)

  if (!assertive::is_numeric(model$frame[[interaction_var_name]])) {
    stop(str_interp("argument 'interaction_var_name' with value '${interaction_var_name}' must be a numeric (i.e. continuous) variable."))
  }


  if (!assertive::is_numeric(quantiles)) {
    stop("argument 'quantiles' must be a numeric vector")
  }

  if (any(quantiles > 1 | quantiles < 0)) {
    stop("argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)")
  }
}


#' @import tibble
get_quantile_coefs_all <- function(coefs, coef_names, interaction_var_name, quantiles) {
  quantile_coefs_tibble <- tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    quantile_coefs_tibble <- rbind(
      quantile_coefs_tibble,
      get_quantile_coefs(
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
#' @export
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_distributions_by_continuous_var <- function(data, model,
                                                   dist_name,
                                                   interaction_var_name,
                                                   quantiles = c(0.5, 0.5, 0.95),
                                                   coef_names = NULL) {
  validate_continuous_args(
    data = data,
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    interaction_var_name = interaction_var_name,
    quantiles = quantiles
  )

  coefs <- glmmTMB::fixef(model)$cond
  coef_names <- if (is.null(coef_names)) get_default_coef_names(dist_name) else coef_names

  quantile_coefs_tibble <- get_quantile_coefs_all(
    coefs = coefs,
    coef_names = coef_names,
    interaction_var_name = interaction_var_name,
    quantiles = quantiles
  )

  updated_parameters_tibble <- get_updated_parameters(
    data = data,
    dist_name = dist_name,
    quantile_coefs_tibble = quantile_coefs_tibble
  )

  return(updated_parameters_tibble)
}
