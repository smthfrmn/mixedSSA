DEFAULT_QUANTILES <- c(0.05, 0.5, 0.75, 0.95)


validate_continuous_args <- function(quantiles) {
  if (any(quantiles > 1 | quantiles < 0)) {
    stop("argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)")
  }
}


get_quantiles_coef_values <- function(interaction_data, quantiles,
                                      target_coefs) {
  quantile_multipliers <- stats::quantile(interaction_data,
    probs = quantiles, na.rm = T
  )
  quantile_coef_values <- target_coefs[, 1] + sapply(
    quantile_multipliers,
    function(q) q * target_coefs[, 2]
  )

  if (is.null(colnames(quantile_coef_values))) {
    quantile_coef_values <- data.frame(t(quantile_coef_values))
  }
  colnames(quantile_coef_values) <- quantiles
  return(quantile_coef_values)
}


get_quantile_coefs <- function(interaction_data, coefs,
                               coef_name, random_effects_var_name,
                               interaction_var_name, quantiles) {
  target_coefs <- get_coefs(
    coefs = coefs,
    coef_name = coef_name,
    interaction_var_name = interaction_var_name
  )

  args_tibble <- get_quantiles_coef_values(
    interaction_data, quantiles,
    target_coefs
  ) %>%
    tibble::as_tibble()

  args_tibble$coef_name <- coef_name

  if (!is.null(random_effects_var_name)) {
    args_tibble[[random_effects_var_name]] <- rownames(target_coefs)
  }

  args_tibble_pivoted <- args_tibble %>%
    tidyr::pivot_longer(
      cols = as.character(quantiles),
      names_to = "quantile",
      values_to = "coef_value"
    )

  return(args_tibble_pivoted)
}


get_quantile_coefs_all <- function(interaction_data, coefs, coef_names,
                                   random_effects_var_name, interaction_var_name, quantiles) {
  quantile_coefs_tibble <- tibble::tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    quantile_coefs_tibble <- rbind(
      quantile_coefs_tibble,
      get_quantile_coefs(
        interaction_data = interaction_data,
        coefs = coefs,
        coef_name = coef_name,
        random_effects_var_name = random_effects_var_name,
        interaction_var_name = interaction_var_name,
        quantiles = quantiles
      )
    )
  }

  return(quantile_coefs_tibble)
}




get_quantile_coefs_all_new <- function(interaction_data, coefs, coef_names,
                                       random_effects_var_name, interaction_var_name, quantiles) {
  coef_names_str <- paste(coef_names, collapse = "|")
  interaction_str <- gsub(
    "([.|()\\^{}+$*?]|\\[|\\])",
    "\\\\\\1",
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
    tidyr::expand(random_effect, interaction_var = quantiles)

  final_df <- full_join(all_coefs, quantiles_df,
    by = "random_effect",
    relationship = "many-to-many"
  ) |>
    mutate(
      quantile_multiplier = stats::quantile(interaction_data, probs = interaction_var, na.rm = T),
      coef_value = coef_base_value + (coef_value_add * quantile_multiplier)
    ) |>
    dplyr::select(interaction_var, random_effect, coef_name, coef_value) |>
    arrange(interaction_var, random_effect)

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

  quantile_coefs_tibble <- get_quantile_coefs_all_new(
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
    grouping = "quantile",
    interaction_var = interaction_var_name,
    random_effect = random_effects_var_name
  )

  return(updated_parameters)
}
