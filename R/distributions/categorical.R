# TODO:
# - include random effects in update functions
# - continuous updating distributions:
#     - 5, 50, 95%
# - add unif distribution support
# - what is a normal amount to round to?
# - delta method

get_categories_from_coefs <- function(interaction_coefs) {
  interaction_coef_names <- names(interaction_coefs)

  categories <- sapply(interaction_coef_names, function(name) {
    return(str_extract(name, regex("(?<=:).*")))
  })

  return(unname(categories))
}


#' @import stringr
#' @import tidyr
get_updated_parameters <- function(data, dist_name, summed_coefs_tibble) {
  pivoted_args_tibble <- summed_coefs_tibble |>
    pivot_wider(
      names_from = "coef_name",
      values_from = "coef_value_sum"
    )

  observed_fitted_distribution <- fit_distribution(
    data = data,
    dist_name = dist_name,
    na_rm = TRUE
  )

  update_fn_and_args <- get_update_distribution_function_and_args(
    dist_name = dist_name
  )
  update_fn <- update_fn_and_args$fn
  update_fn_arg_names <- update_fn_and_args$args

  # rename the column headers to match the amt arg names
  colnames(pivoted_args_tibble) <- c(
    "category",
    update_fn_arg_names[2:length(update_fn_arg_names)]
  )

  all_updated_parameters <- apply(pivoted_args_tibble,
    1, update_parameters,
    dist = observed_fitted_distribution,
    update_fn = update_fn
  )

  observed_params <- observed_fitted_distribution$params
  observed_row <- c(
    "observed",
    rep(NA, ncol(pivoted_args_tibble) - 1),
    unlist(observed_params)
  )

  updated_parameters_tibble <- rbind(
    observed_row,
    cbind(
      pivoted_args_tibble,
      dplyr::bind_rows(all_updated_parameters)
    )
  ) %>%
    mutate(across(
      -category,
      function(x) {
        round(as.numeric(x), 6)
      }
    ))
  return(updated_parameters_tibble)
}


#' @import tibble
get_summed_coefs <- function(coefs, coef_name, reference_category) {
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
get_summed_coefs_all <- function(coefs, coef_names, reference_category) {
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
update_distributions_by_categorical_var <- function(data, model,
                                                    dist_name,
                                                    coef_names = NULL,
                                                    reference_category = "reference_category") {
  validate_args(
    data = data,
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    reference_category = reference_category
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
