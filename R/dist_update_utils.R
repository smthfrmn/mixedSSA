# TODO: add support for uniform dist

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


get_update_distribution_function_and_args <- function(dist_name) {
  update_fn <- methods::getFunction(stringr::str_interp("update_${dist_name}"))
  update_fn_args <- methods::formalArgs(update_fn)
  return(list(
    fn = update_fn,
    args = update_fn_args
  ))
}


validate_coef_names <- function(model, dist_name, coef_names) {
  nvar_names <- length(get_default_coef_names(dist_name))
  npassed_var_names <- length(coef_names)

  if (npassed_var_names != nvar_names) {
    param_string <- ifelse(nvar_names == 1, "parameter", "parameters")
    stop(stringr::str_interp("distribution ${dist_name} expects ${nvar_names} ${param_string} to be passed, not ${npassed_var_names}."))
  }

  actual_coef_names <- names(glmmTMB::fixef(model)$cond)
  for (i in 1:npassed_var_names) {
    coef_name <- coef_names[i]
    if (!coef_name %in% actual_coef_names) {
      stop(stringr::str_interp(
        "argument 'coef_names' not valid. Variable name ${coef_name} not found in model with coef names ${coef_names}."
      ))
    }
  }
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
    variable <- stringr::str_interp(
      '${stringr::str_extract(variable_arg, stringr::regex("(?<=beta_).*"))}_'
    )
    return(variable)
  }, USE.NAMES = FALSE))
}


validate_interaction_coefficients <- function(model, interaction_var_name) {
  actual_coef_names <- names(glmmTMB::fixef(model)$cond)
  valid_var_name <- any(grepl(stringr::str_interp(":${interaction_var_name}"), actual_coef_names))

  if (!valid_var_name) {
    stop(stringr::str_interp("argument 'interaction_var_name' with value ${interaction_var_name} does not appear to be part of an interaction coefficient in the provided model."))
  }
}


validate_base_args <- function(data, model, dist_name, coef_names, interaction_var_name) {
  if (!assertive::is_numeric(data)) {
    stop("argument 'data' must be a vector of type numeric. Make sure you are passing either the step lengths column (e.g. sl_) or turn angles (e.g. cos_ta_).")
  }

  if (!is(model, "glmmTMB")) {
    stop("argument 'model' must be of class 'glmmTMB'.")
  }

  if (!dist_name %in% SUPPORTED_DISTRIBUTIONS) {
    stop(stringr::str_interp("argument 'dist_name' must be one of ${SUPPORTED_DISTRIBUTIONS}."))
  }

  if (!is.null(coef_names)) {
    validate_coef_names(model, dist_name, coef_names)
  }

  if (!assertive::is_a_string(interaction_var_name)) {
    stop(stringr::str_interp("argument 'interaction_var_name' must be a string."))
  }

  validate_interaction_coefficients(model, interaction_var_name)
}

#' @export
fit_distribution <- function(data, dist_name, na_rm) {
  return(amt::fit_distr(data,
    dist_name = dist_name,
    na.rm = na_rm
  ))
}


get_updated_parameters <- function(data, dist_name, coefs_tibble, grouping = "category") {
  pivoted_args_tibble <- coefs_tibble %>%
    tidyr::pivot_wider(
      names_from = "coef_name",
      values_from = "coef_value"
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
    grouping,
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
      -grouping,
      function(x) {
        round(as.numeric(x), 6)
      }
    ))
  return(updated_parameters_tibble)
}
