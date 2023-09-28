#' @import tibble
get_coefs_tibble <- function(coefs, coef_names, random_effects_var_name) {
  coefs_tibble <- tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    args_tibble <- get_coefs(
      coefs = coefs,
      coef_name = coef_name,
      interaction_var_name = NULL
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        "coef_value" = coef_name
      )

    args_tibble$coef_name <- coef_name

    if(!is.null(random_effects_var_name)) {
      args_tibble[[random_effects_var_name]] <- rownames(target_coefs)
    }
    browser()

    coefs_tibble <- rbind(coefs_tibble, args_tibble)
  }

  return(coefs_tibble)
}



#' @noRd
#' @import dplyr
#' @import amt
update_dist_non_interaction <- function(model,
                                        dist_name,
                                        random_effects_var_name,
                                        coef_names,
                                        tentative_dist) {

  browser()
  coefs <- get_coefs_from_model(
    model = model,
    random_effects_var_name = random_effects_var_name
  )

  coefs_tibble <- get_coefs_tibble(
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
