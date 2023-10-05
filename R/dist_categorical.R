get_categories_from_coefs <- function(interaction_coefs, interaction_var_name) {
  interaction_coef_names <- names(interaction_coefs)

  categories <- sapply(interaction_coef_names, function(name) {
    regex_str <- stringr::str_interp("(?<=:${interaction_var_name}).*")
    return(stringr::str_extract(name, stringr::regex(regex_str)))
  })

  return(unname(categories))
}


get_summed_coefs <- function(model, coefs, coef_name, interaction_var_name, random_effects_var_name) {
  target_coefs <- get_coefs(
    coefs = coefs,
    coef_name = coef_name,
    interaction_var_name = interaction_var_name
  )

  all_categories <- levels(model$frame[[interaction_var_name]])
  reference_category <- all_categories[1]
  non_ref_categories <- all_categories[2:length(all_categories)]

  nrows <- length(all_categories) * nrow(target_coefs)

  reference_category_rows <- tibble::tibble(
    category = reference_category,
    coef_name = coef_name,
    coef_value = target_coefs[[coef_name]]
  )

  if(!is.null(random_effects_var_name)) {
    reference_category_rows[[random_effects_var_name]] <- rownames(target_coefs)
  }

  target_coef_sums <- rowSums(target_coefs)
  non_reference_category_rows <- tibble(
    category = non_ref_categories,
    coef_name = coef_name,
    coef_value = rowSums(target_coefs)
  )

  if(!is.null(random_effects_var_name)) {
    non_reference_category_rows[[random_effects_var_name]] <- rownames(target_coefs)
  }

  args_tibble <- rbind(
    reference_category_rows,
    non_reference_category_rows
  )

  return(args_tibble)
}


get_summed_coefs_all <- function(model, coefs, coef_names, interaction_var_name, random_effects_var_name) {
  summed_coefs_tibble <- tibble::tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    summed_coefs_tibble <- rbind(
      summed_coefs_tibble,
      get_summed_coefs(
        model = model,
        coefs = coefs,
        coef_name = coef_name,
        interaction_var_name = interaction_var_name,
        random_effects_var_name = random_effects_var_name
      )
    )
  }

  return(summed_coefs_tibble)
}


get_coefs_from_model <- function(model, random_effects_var_name) {
  if (is.null(random_effects_var_name)) {
    fixed_effects <- as.data.frame(t(unlist(glmmTMB::fixef(model)$cond)))
    return(fixed_effects)
  }

  random_effects <- coef(model)$cond[[random_effects_var_name]]
  return(random_effects)
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
    tentative_dist = tentative_dist
  )

  return(updated_parameters)
}
