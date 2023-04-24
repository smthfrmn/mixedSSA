# TODO:
# - include random effects in update functions
# - continuous updating distributions:
#     - 5, 50, 95%


#' @import amt
#' @import methods
get_update_distribution_function_and_args <- function(distribution) {
  update_fn <- methods::getFunction(str_interp("update_${distribution}"))
  update_fn_args <- methods::formalArgs(update_fn)
  return(list(
    fn = update_fn,
    args = update_fn_args
  ))
}


update_parameters <- function(args_tibble_row, dist, update_fn) {
  args <- c(list(dist = dist), sapply(args_tibble_row[2:length(args_tibble_row)], as.numeric))
  updated_parameters <- do.call(update_fn, args)$params
  return(updated_parameters)
}


get_default_coefficient_names <- function(distribution) {
  update_fn_args <- get_update_distribution_function_and_args(distribution)$args
  variable_args <- update_fn_args[2:length(update_fn_args)]
  return(sapply(variable_args, function(variable_arg) {
    variable <- str_interp(
      '${str_extract(variable_arg, regex("(?<=beta_).*"))}_'
    )
    return(variable)
  }, USE.NAMES = FALSE))
}


validate_coefficient_names <- function(model, distribution, coefficient_names) {
  nvar_names <- length(get_default_coefficient_names(distribution))
  npassed_var_names <- length(coefficient_names)

  if (npassed_var_names != nvar_names) {
    param_string <- ifelse(nvar_names == 1, "parameter", "parameters")
    stop(str_interp("distribution ${distribution} expects ${nvar_names} ${param_string} to be passed, not ${npassed_var_names}."))
  }

  coef_names <- names(glmmTMB::fixef(model)$cond)
  for (i in 1:npassed_var_names) {
    coefficient_name <- coefficient_names[i]
    if (!coefficient_name %in% coef_names) {
      stop(str_interp(
        "argument 'coefficient_names' not valid. Variable name ${coefficient_name} not found in model with coefficient names ${coef_names}"
      ))
    }
  }
}


#' @import stringr
#' @import assertive
#' @import glmmTMB
validate_args <- function(data, model, distribution, coefficient_names, reference_category) {
  if (!assertive::is_numeric(data)) {
    stop("argument 'data' must be a vector of type numeric. Make sure you are passing either the step lengths column (e.g. sl_) or turn angles (e.g. cos_ta_).")
  }

  if (!class(model) == "glmmTMB") {
    stop("argument 'model' must be of class 'glmmTMB'")
  }

  valid_distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")

  if (!distribution %in% valid_distributions) {
    stop(stringr::str_interp("argument 'distribution' must be one of ${valid_distributions}"))
  }

  if (!is.null(coefficient_names)) {
    validate_coefficient_names(model, distribution, coefficient_names)
  }

  if (!assertive::is_a_string(reference_category)) {
    stop("argument 'reference_category' must be a string")
  }
}


get_categories_from_coefficients <- function(interaction_coefficients) {
  interaction_coefficient_names <- names(interaction_coefficients)

  categories <- sapply(interaction_coefficient_names, function(name) {
    return(str_extract(name, regex("(?<=:).*")))
  })

  return(unname(categories))
}


#' @import stringr
get_updated_parameters <- function(distribution, summed_coefficients_tibble) {
  # use group_by here instead
  pivoted_args_tibble <- summed_coefficients_tibble |>
    mutate(
      coefficient_value_sum = as.numeric(coefficient_value_sum)
    ) |>
    pivot_wider(
      names_from = coefficient_name,
      values_from = coefficient_value_sum
    ) |>
    group_by(category) |>
    summarize(across(coefficient_names, mean, na.rm = TRUE))

  colnames(pivoted_args_tibble) <- c("category", update_fn_and_args$args[2:length(update_fn_and_args$args)])

  observed_fitted_distribution <- amt::fit_distr(data, distribution, na.rm = TRUE)
  update_fn_and_args <- get_update_distribution_function_and_args(distribution)
  update_fn <- update_fn_and_args$fn
  update_fn_arg_names <- update_fn_and_args$args

  all_updated_parameters <- apply(pivoted_args_tibble,
    1, update_parameters,
    dist = observed_fitted_distribution,
    update_fn = update_fn
  )

  observed_params <- observed_fitted_distribution$params
  observed_row <- c("observed", NA, NA, unlist(observed_params))

  updated_parameters_tibble <- rbind(
    observed_row,
    cbind(pivoted_args_tibble, rbindlist(all_updated_parameters))
  )

  return(updated_parameters_tibble)
}


#' @import purrr
get_summed_coefficients <- function(coefs, coefficient_name, reference_category) {
  interaction_coefficients <- names(coefs) %>%
    str_detect(pattern = str_interp("^${coefficient_name}:")) %>%
    purrr::keep(coefs, .)

  categories <- get_categories_from_coefficients(interaction_coefficients)
  interaction_coefficient_values <- unname(interaction_coefficients)
  nrows <- length(interaction_coefficient_values)

  coefficient_name_vector <- rep(coefficient_name, nrows)
  coefficient_value_vector <- rep(coefs[coefficient_name], nrows)

  reference_category_row <- tibble::tibble(
    category = reference_category,
    coefficient_name = coefficient_name,
    coefficient_value_sum = unname(coefs[coefficient_name])
  )

  non_reference_category_rows <- tibble::tibble(
    category = categories,
    coefficient_name = coefficient_name_vector,
    coefficient_value_sum = unname(interaction_coefficient_values + coefficient_value_vector)
  )

  args_tibble <- rbind(
    non_reference_category_rows,
    reference_category_row
  )

  row.names(args_tibble) <- NULL

  return(tibble::as_tibble(args_tibble))
}



get_summed_coefficients_all <- function(coefs, coefficient_names, reference_category) {
  summed_coefficients_tibble <- tibble::tibble()

  for (i in 1:length(coefficient_names)) {
    coefficient_name <- coefficient_names[i]
    summed_coefficients_tibble <- rbind(
      summed_coefficients_tibble,
      get_summed_coefficients(
        coefs = coefs,
        coefficient_name = coefficient_name,
        reference_category = reference_category
      )
    )
  }

  return(summed_coefficients_tibble)
}



#' Update movement distributions based on fitted models
#'
#' @export
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_distributions_by_categorical_var <- function(data, model,
                                                    distribution,
                                                    coefficient_names = NULL,
                                                    reference_category = "reference_category") {
  validate_args(
    data = data,
    model = model,
    distribution = distribution,
    coefficient_names = coefficient_names,
    reference_category = reference_category
  )

  coefs <- glmmTMB::fixef(model)$cond
  coefficient_names <- if (is.null(coefficient_names)) get_default_coefficient_names(distribution) else coefficient_names

  summed_coefficients_tibble <- get_summed_coefficients_all(coefs, coefficient_names)
  updated_parameters_tibble <- get_updated_parameters(distribution, summed_coefficients_tibble)

  return(updated_parameters_tibble)
}
