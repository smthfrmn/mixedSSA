#' @import tibble
get_non_interaction_coefs <- function(coefs, coef_names, random_effects_var_name) {
  coefs_tibble <- tibble()

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
    add_column(grouping = NA, .before = "coef_value")
  return(coefs_tibble)
}



#' @noRd
#' @import dplyr
#' @import amt
update_dist_non_interaction <- function(model,
                                        dist_name,
                                        coef_names,
                                        random_effects_var_name,
                                        tentative_dist) {

  coefs <- get_coefs_from_model(
    model = model,
    random_effects_var_name = random_effects_var_name
  )

  coefs_tibble <- get_non_interaction_coefs(
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
    grouping = "no_interaction"
  )

  return(updated_parameters)
}
