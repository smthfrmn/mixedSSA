get_no_interaction_coefs <- function(coefs, coef_names, random_effects_var_name) {

  browser()
  coefs_tibble <- tibble::tibble()

  for (i in 1:length(coef_names)) {
    target_coef_name <- coef_names[i]

    args_tibble <- get_coefs(
      coefs = coefs,
      coef_name = target_coef_name,
      interaction_var_name = NULL
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        "coef_value" = sym(target_coef_name)  # sym to repress warning
      )

    args_tibble$coef_name <- target_coef_name

    if(!is.null(random_effects_var_name)) {
      args_tibble[[random_effects_var_name]] <- rownames(coefs)
    }

    coefs_tibble <- rbind(coefs_tibble, args_tibble)
  }

  coefs_tibble <- coefs_tibble %>%
    tibble::add_column(grouping = NA, .before = "coef_value")

  return(coefs_tibble)
}



get_no_interaction_coefs_new <- function(coefs, coef_names, random_effects_var_name) {

  browser()
  if (!is.null(random_effects_var_name)) {
    random_effects <- rownames(coefs)
  } else {
    random_effects <- rep(NA, nrow(coefs))
  }

  final_df <- coefs |>
    mutate(
      random_effect = random_effects
    ) |>
    tidyr::pivot_longer( # get sl_, log_sl_ etc in one column
      cols = all_of(coef_names),
      names_to = "coef_name",
      values_to = "coef_value"
    ) |>
    mutate(
      interaction_var = NA
    ) |>
    dplyr::select(interaction_var, random_effect, coef_name, coef_value)

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

  coefs_tibble <- get_no_interaction_coefs_new(
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
    grouping = "no_interaction",
    random_effect_var_name = random_effects_var_name
  )

  return(updated_parameters)
}
