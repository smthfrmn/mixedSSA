# TODO:
# - include random effects in update functions
# - continuous updating distributions:
#     - 5, 50, 95%
# - add unif distribution support
# - what is a normal amount to round to?
# - delta method

GAMMA <- "gamma"
EXP <- "exp"
HNORM <- "hnorm"
LNORM <- "lnorm"
VONMISES <- "vonmises"

TURN_ANGLE_DISTRIBUTIONS <- c(VONMISES)
STEP_LENGTH_DISTRIBUTIONS <- c(GAMMA, EXP, HNORM, LNORM)

SUPPORTED_DISTRIBUTIONS <- c(
  STEP_LENGTH_DISTRIBUTIONS,
  TURN_ANGLE_DISTRIBUTIONS
)

#' @import amt
#' @import methods
get_update_distribution_function_and_args <- function(dist_name) {
  update_fn <- methods::getFunction(str_interp("update_${dist_name}"))
  update_fn_args <- methods::formalArgs(update_fn)
  return(list(
    fn = update_fn,
    args = update_fn_args
  ))
}


update_parameters <- function(args_tibble_row, dist, update_fn) {
  args <- c(list(dist = dist), sapply(args_tibble_row[2:length(args_tibble_row)], as.numeric))

  response <- do.call(update_fn, args)
  updated_parameters <- response$params

  if (response$name == VONMISES) {
    updated_parameters$mu <- updated_parameters$mu[[1]]
  }

  return(updated_parameters)
}


get_default_coef_names <- function(dist_name) {
  update_fn_args <- get_update_distribution_function_and_args(dist_name)$args
  variable_args <- update_fn_args[2:length(update_fn_args)]
  return(sapply(variable_args, function(variable_arg) {
    variable <- str_interp(
      '${str_extract(variable_arg, regex("(?<=beta_).*"))}_'
    )
    return(variable)
  }, USE.NAMES = FALSE))
}


validate_coef_names <- function(model, dist_name, coef_names) {
  nvar_names <- length(get_default_coef_names(dist_name))
  npassed_var_names <- length(coef_names)

  if (npassed_var_names != nvar_names) {
    param_string <- ifelse(nvar_names == 1, "parameter", "parameters")
    stop(str_interp("distribution ${dist_name} expects ${nvar_names} ${param_string} to be passed, not ${npassed_var_names}."))
  }

  actual_coef_names <- names(glmmTMB::fixef(model)$cond)
  for (i in 1:npassed_var_names) {
    coef_name <- coef_names[i]
    if (!coef_name %in% actual_coef_names) {
      stop(str_interp(
        "argument 'coef_names' not valid. Variable name ${coef_name} not found in model with coef names ${coef_names}"
      ))
    }
  }
}


#' @import stringr
#' @import assertive
#' @import glmmTMB
validate_args <- function(data, model, dist_name, coef_names, reference_category) {
  if (!assertive::is_numeric(data)) {
    stop("argument 'data' must be a vector of type numeric. Make sure you are passing either the step lengths column (e.g. sl_) or turn angles (e.g. cos_ta_).")
  }

  if (!is(model, "glmmTMB")) {
    stop("argument 'model' must be of class 'glmmTMB'")
  }

  if (!dist_name %in% SUPPORTED_DISTRIBUTIONS) {
    stop(stringr::str_interp("argument 'distribution' must be one of ${SUPPORTED_DISTRIBUTIONS}"))
  }

  if (!is.null(coef_names)) {
    validate_coef_names(model, dist_name, coef_names)
  }

  if (!assertive::is_a_string(reference_category)) {
    stop("argument 'reference_category' must be a string")
  }
}


get_categories_from_coefs <- function(interaction_coefs) {
  interaction_coef_names <- names(interaction_coefs)

  categories <- sapply(interaction_coef_names, function(name) {
    return(str_extract(name, regex("(?<=:).*")))
  })

  return(unname(categories))
}


fit_distribution <- function(data, dist_name, na_rm) {
  return(amt::fit_distr(data,
    dist_name = dist_name,
    na.rm = na_rm
  ))
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
