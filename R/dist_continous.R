DEFAULT_QUANTILES <- c(0.05, 0.5, 0.75, 0.95)


validate_continuous_args <- function(quantiles) {
  if (any(quantiles > 1 | quantiles < 0)) {
    stop("argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)")
  }
}


get_quantile_coefs_all <- function(interaction_data, coefs, coef_names,
                                   random_effects_var_name, interaction_var_name, quantiles) {
  coef_names_str <- paste(make_regex_safe(coef_names), collapse = "|")
  interaction_str <- make_regex_safe(
    stringr::str_interp("${interaction_var_name}")
  )

  if (!is.null(random_effects_var_name)) {
    random_effects <- rownames(coefs)
  } else {
    random_effects <- rep(NA, nrow(coefs))
  }

  all_coefs <- coefs |>
    mutate(
      random_effect = random_effects
    ) |>
    tidyr::pivot_longer( # get sl_, log_sl_ etc in one column
      cols = all_of(coef_names),
      names_to = "coef_name",
      values_to = "coef_base_value"
    ) |>
    tidyr::pivot_longer( # then get the interaction variables in one column
      cols = tidyselect::matches(stringr::str_interp("^${coef_names_str}:${interaction_str}")),
      names_to = "coef_name_add",
      values_to = "coef_value_add"
    ) |>
    mutate( # only add together the rows that are the same parameter, sl_ w/sl_ etc.
      coef_name_add_prefix = stringr::str_extract(
        coef_name_add,
        stringr::str_interp("^(${coef_names_str})(?=:${interaction_str})")
      )
    ) |>
    filter( # filter to same ones
      coef_name == coef_name_add_prefix
    )


  quantile_multipliers <- stats::quantile(interaction_data,
    probs = quantiles, na.rm = T
  )

  quantiles_df <- all_coefs |>
    tidyr::expand(random_effect, grouping = quantiles)

  final_df <- full_join(all_coefs, quantiles_df,
    by = "random_effect",
    relationship = "many-to-many"
  ) |>
    mutate(
      quantile_multiplier = stats::quantile(interaction_data, probs = grouping, na.rm = T),
      coef_value = coef_base_value + (coef_value_add * quantile_multiplier)
    ) |>
    dplyr::select(grouping, random_effect, coef_name, coef_value) |>
    arrange(grouping, random_effect)

  return(final_df)
}


#' @noRd
update_dist_by_continuous_var <- function(model,
                                          dist_name,
                                          random_effects_var_name,
                                          interaction_var_name,
                                          coef_names,
                                          quantiles,
                                          tentative_dist) {
  validate_continuous_args(
    quantiles = quantiles
  )

  coefs <- get_coefs_from_model(
    model = model,
    random_effects_var_name = random_effects_var_name
  )

  quantile_coefs_tibble <- get_quantile_coefs_all(
    interaction_data = model$frame[[interaction_var_name]],
    coefs = coefs,
    coef_names = coef_names,
    random_effects_var_name = random_effects_var_name,
    interaction_var_name = interaction_var_name,
    quantiles = quantiles
  )

  movement_coef_name <- coef_names[1]
  updated_parameters <- get_updated_parameters(
    model = model,
    movement_coef_name = movement_coef_name,
    dist_name = dist_name,
    coefs_tibble = quantile_coefs_tibble,
    tentative_dist = tentative_dist,
    grouping_type = "quantile",
    interaction_var = interaction_var_name,
    random_effect = random_effects_var_name
  )

  return(updated_parameters)
}
