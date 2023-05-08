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



validate_continuous_args <- function(data, model, dist_name, continuous_var_name, coef_names, quantiles) {
  validate_base_args(data, model, dist_name, coef_names)

  actual_coef_names <- names(glmmTMB::fixef(model)$cond)
  if (!continuous_var_name %in% actual_coef_names) {
    stop("argument 'continuous_var_name' is not a covariate in the supplied model")
  }

  if (!is_numeric(quantiles)) {
    stop("argument 'quantiles' must be a numeric vector")
  }

  if (any(quantiles > 1 | quantiles < 0)) {
    stop("argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)")
  }

}


validate_categorical_args <- function(data, model, dist_name, coef_names, reference_category) {
  validate_base_args(data, model, dist_name)

  if (!assertive::is_a_string(reference_category)) {
    stop("argument 'reference_category' must be a string")
  }
}


#' @import stringr
#' @import assertive
#' @import glmmTMB
validate_base_args <- function(data, model, dist_name, coef_names) {
  if (!assertive::is_numeric(data)) {
    stop("argument 'data' must be a vector of type numeric. Make sure you are passing either the step lengths column (e.g. sl_) or turn angles (e.g. cos_ta_).")
  }

  if (!is(model, "glmmTMB")) {
    stop("argument 'model' must be of class 'glmmTMB'")
  }

  if (!dist_name %in% SUPPORTED_DISTRIBUTIONS) {
    stop(stringr::str_interp("argument 'dist_name' must be one of ${SUPPORTED_DISTRIBUTIONS}"))
  }

  if (!is.null(coef_names)) {
    validate_coef_names(model, dist_name, coef_names)
  }
}


fit_distribution <- function(data, dist_name, na_rm) {
  return(amt::fit_distr(data,
                        dist_name = dist_name,
                        na.rm = na_rm
  ))
}
