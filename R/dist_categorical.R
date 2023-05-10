# TODO:
# - include random effects in update functions
# - continuous updating distributions:
#     - 5, 50, 95%
# - add unif distribution support
# - delta method

get_categories_from_coefs <- function(interaction_coefs, interaction_var_name) {
  interaction_coef_names <- names(interaction_coefs)

  categories <- sapply(interaction_coef_names, function(name) {
    regex_str <- str_interp("(?<=:${interaction_var_name}).*")
    return(str_extract(name, stringr::regex(regex_str)))
  })

  return(unname(categories))
}


#' @import tibble
get_summed_coefs <- function(coefs, coef_name, interaction_var_name, reference_category) {
  interaction_coefs <- coefs[grepl(stringr::str_interp("^${coef_name}:${interaction_var_name}"), names(coefs))]

  categories <- get_categories_from_coefs(interaction_coefs, interaction_var_name)
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
get_summed_coefs_all <- function(coefs, coef_names, interaction_var_name, reference_category) {
  summed_coefs_tibble <- tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    summed_coefs_tibble <- rbind(
      summed_coefs_tibble,
      get_summed_coefs(
        coefs = coefs,
        coef_name = coef_name,
        interaction_var_name = interaction_var_name,
        reference_category = reference_category
      )
    )
  }

  return(summed_coefs_tibble)
}


#' @import assertive
validate_categorical_args <- function(data, model, dist_name, coef_names, interaction_var_name, reference_category) {
  validate_base_args(data, model, dist_name, coef_names, interaction_var_name)

  if (!assertive::is_factor(model$frame[[interaction_var_name]])) {
    stop(str_interp("argument 'interaction_var_name' with value '${interaction_var_name}' must be a factor (i.e. categorical) variable."))
  }

  if (!assertive::is_a_string(reference_category)) {
    stop("argument 'reference_category' must be a string")
  }
}


#' Update movement distributions based on fitted models
#'
#' @export
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_distributions_by_categorical_var <- function(data, model,
                                                    dist_name,
                                                    interaction_var_name,
                                                    coef_names = NULL,
                                                    reference_category = "reference_category") {
  validate_categorical_args(
    data = data,
    model = model,
    dist_name = dist_name,
    interaction_var_name = interaction_var_name,
    coef_names = coef_names,
    reference_category = reference_category
  )

  coefs <- glmmTMB::fixef(model)$cond
  coef_names <- if (is.null(coef_names)) get_default_coef_names(dist_name) else coef_names

  summed_coefs_tibble <- get_summed_coefs_all(
    coefs = coefs,
    coef_names = coef_names,
    interaction_var_name = interaction_var_name,
    reference_category = reference_category
  )

  updated_parameters_tibble <- get_updated_parameters(
    data = data,
    dist_name = dist_name,
    coefs_tibble = summed_coefs_tibble
  )

  return(updated_parameters_tibble)
}
