# TODO:
# - include random effects in update functions
# - add unif distribution support

get_categories_from_coefs <- function(interaction_coefs, interaction_var_name) {
  interaction_coef_names <- names(interaction_coefs)

  categories <- sapply(interaction_coef_names, function(name) {
    regex_str <- stringr::str_interp("(?<=:${interaction_var_name}).*")
    return(stringr::str_extract(name, stringr::regex(regex_str)))
  })

  return(unname(categories))
}


#' @import tibble
get_summed_coefs <- function(model, coefs, coef_name, interaction_var_name) {
  interaction_coefs <- get_interaction_coefs(
    coefs = coefs,
    coef_name = coef_name,
    interaction_var_name = interaction_var_name
  )

  categories <- get_categories_from_coefs(interaction_coefs, interaction_var_name)
  reference_category <- levels(model$frame[[interaction_var_name]])[1]

  interaction_coef_values <- unname(interaction_coefs)
  nrows <- length(interaction_coef_values)

  coef_name_vector <- rep(coef_name, nrows)
  coef_value_vector <- rep(coefs[coef_name], nrows)

  reference_category_row <- tibble(
    category = reference_category,
    coef_name = coef_name,
    coef_value = unname(coefs[coef_name])
  )

  non_reference_category_rows <- tibble(
    category = categories,
    coef_name = coef_name_vector,
    coef_value = unname(interaction_coef_values + coef_value_vector)
  )

  args_tibble <- rbind(
    non_reference_category_rows,
    reference_category_row
  )

  row.names(args_tibble) <- NULL

  return(args_tibble)
}


#' @import tibble
get_summed_coefs_all <- function(model, coefs, coef_names, interaction_var_name) {
  summed_coefs_tibble <- tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    summed_coefs_tibble <- rbind(
      summed_coefs_tibble,
      get_summed_coefs(
        model = model,
        coefs = coefs,
        coef_name = coef_name,
        interaction_var_name = interaction_var_name
      )
    )
  }

  return(summed_coefs_tibble)
}


#' @import assertive
validate_categorical_args <- function(model, dist_name, coef_names, interaction_var_name) {
  interaction_var <- model$frame[[interaction_var_name]]

  if (!assertive::is_factor(interaction_var)) {
    stop(stringr::str_interp("argument 'interaction_var_name' with value '${interaction_var_name}' must be a factor (i.e. categorical) variable."))
  }
}


#' Update movement distributions based on fitted models
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_dist_by_categorical_var <- function(model,
                                           dist_name,
                                           interaction_var_name,
                                           coef_names) {

  validate_categorical_args(
    model = model,
    dist_name = dist_name,
    interaction_var_name = interaction_var_name,
    coef_names = coef_names
  )

  coefs <- glmmTMB::fixef(model)$cond

  summed_coefs_tibble <- get_summed_coefs_all(
    model = model,
    coefs = coefs,
    coef_names = coef_names,
    interaction_var_name = interaction_var_name
  )

  movement_coef_name <- coef_names[1]
  updated_parameters <- get_updated_parameters(
    model = model,
    movement_coef_name = movement_coef_name,
    dist_name = dist_name,
    coefs_tibble = summed_coefs_tibble
  )

  return(updated_parameters)
}
