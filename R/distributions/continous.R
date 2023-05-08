#' @import tibble
get_summed_coefs <- function(coefs, coef_name, quantile) {
  interaction_coefs <- coefs[grepl(str_interp("^${coef_name}:"), names(coefs))]

  categories <- get_categories_from_coefs(interaction_coefs)
  interaction_coef_values <- unname(interaction_coefs)
  nrows <- length(interaction_coef_values)

  coef_name_vector <- rep(coef_name, nrows)
  coef_value_vector <- rep(coefs[coef_name], nrows)

  reference_category_row <- tibble(
    category = reference_category,
    coef_name = coef_name,
    coef_value_sum = unname(coefs[coef_name])
  )

  non_reference_category_rows <- tibble(
    category = categories,
    coef_name = coef_name_vector,
    coef_value_sum = unname(interaction_coef_values + coef_value_vector)
  )

  args_tibble <- rbind(
    non_reference_category_rows,
    reference_category_row
  )

  row.names(args_tibble) <- NULL

  return(args_tibble)
}


#' @import tibble
get_summed_coefs_all <- function(coefs, coef_names, quantiles) {
  summed_coefs_tibble <- tibble()

  for (i in 1:length(coef_names)) {
    coef_name <- coef_names[i]
    summed_coefs_tibble <- rbind(
      summed_coefs_tibble,
      get_summed_coefs(
        coefs = coefs,
        coef_name = coef_name,
        reference_category = reference_category
      )
    )
  }

  return(summed_coefs_tibble)
}



#' Update movement distributions based on fitted models
#'
#' @export
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_distributions_by_continuous_var <- function(data, model,
                                                   dist_name,
                                                   continuous_var_name,
                                                   quantiles = c(0.5, 0.5, 0.95),
                                                   coef_names = NULL) {
  validate_continuous_args(
    data = data,
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    continuous_var_name = continuous_var_name,
    quantiles = quantiles
  )

  coefs <- glmmTMB::fixef(model)$cond
  coef_names <- if (is.null(coef_names)) get_default_coef_names(dist_name) else coef_names

  summed_coefs_tibble <- get_summed_coefs_all(
    coefs = coefs,
    coef_names = coef_names,
    reference_category = reference_category
  )

  updated_parameters_tibble <- get_updated_parameters(
    data = data,
    dist_name = dist_name,
    summed_coefs_tibble = summed_coefs_tibble
  )

  return(updated_parameters_tibble)
}
