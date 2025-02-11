get_no_interaction_coefs <- function(coefs, coef_names, random_effects_var_name) {
  if (!is.null(random_effects_var_name)) {
    random_effects <- rownames(coefs)
  } else {
    random_effects <- rep(NA, nrow(coefs))
  }

  final_df <- coefs |>
    mutate(
      random_effect = random_effects,
      grouping = NA
    ) |>
    tidyr::pivot_longer( # get sl_, log_sl_ etc in one column
      cols = all_of(coef_names),
      names_to = "coef_name",
      values_to = "coef_value"
    ) |>
    dplyr::select(grouping, random_effect, coef_name, coef_value)

  return(final_df)
}



#' @noRd
update_dist_no_interaction <- function(model,
                                       dist_name,
                                       coef_names,
                                       random_effects_var_name,
                                       tentative_dist) {
  coefs <- get_coefs_from_model(
    model = model,
    random_effects_var_name = random_effects_var_name
  )

  coefs_tibble <- get_no_interaction_coefs(
    coefs = coefs,
    coef_names = coef_names,
    random_effects_var_name = random_effects_var_name
  )

  movement_coef_name <- coef_names[1]
  updated_parameters <- get_updated_parameters(
    model = model,
    movement_coef_name = movement_coef_name,
    dist_name = dist_name,
    coefs_tibble = coefs_tibble,
    tentative_dist = tentative_dist,
    grouping_type = "no_interaction",
    interaction_var = NULL,
    random_effect_var_name = random_effects_var_name
  )

  return(updated_parameters)
}
