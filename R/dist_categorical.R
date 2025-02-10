get_summed_coefs_all <- function(model, coefs, coef_names, interaction_var_name, random_effects_var_name) {
  coef_names_str <- paste(coef_names, collapse = "|")
  interaction_str <- gsub(
    "([.|()\\^{}+$*?]|\\[|\\])",
    "\\\\\\1",
    stringr::str_interp("${interaction_var_name}")
  )

  all_categories <- levels(model$frame[[interaction_var_name]])

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
    ) |>
    mutate( # get the new values and the category for each row
      coef_value = coef_base_value + coef_value_add,
      interaction_var = stringr::str_extract(coef_name_add, paste(
        all_categories[order(-nchar(all_categories))],
        collapse = "|"
      ))
    )


  # calculate reference class separately because it is just the coefficient
  reference_class_df <- all_coefs |>
    dplyr::select(
      random_effect, coef_name, coef_base_value
    ) |>
    distinct() |>
    mutate(
      interaction_var = all_categories[[1]]
    ) |>
    rename(
      coef_value = coef_base_value
    )

  final_df <- rbind(
    all_coefs |>
      dplyr::select(interaction_var, random_effect, coef_name, coef_value),
    reference_class_df
  ) |>
    arrange(match(interaction_var, all_categories), random_effect, )


  return(final_df)
}


#' @noRd
update_dist_by_categorical_var <- function(model,
                                           dist_name,
                                           random_effects_var_name,
                                           interaction_var_name,
                                           coef_names,
                                           tentative_dist) {
  coefs <- get_coefs_from_model(model, random_effects_var_name)

  summed_coefs_tibble <- get_summed_coefs_all(
    model = model,
    coefs = coefs,
    coef_names = coef_names,
    interaction_var_name = interaction_var_name,
    random_effects_var_name = random_effects_var_name
  )


  movement_coef_name <- coef_names[1]
  updated_parameters <- get_updated_parameters(
    model = model,
    movement_coef_name = movement_coef_name,
    dist_name = dist_name,
    coefs_tibble = summed_coefs_tibble,
    tentative_dist = tentative_dist,
    grouping = "category",
    interaction_var = interaction_var_name,
    random_effect = random_effects_var_name
  )

  return(updated_parameters)
}
