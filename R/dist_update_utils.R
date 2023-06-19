GAMMA <- "gamma"
EXP <- "exp"
HNORM <- "hnorm"
LNORM <- "lnorm"
VONMISES <- "vonmises"
UNIF <- "unif"

STEP_LENGTH_DISTRIBUTIONS <- c(GAMMA, EXP, HNORM, LNORM)
TURN_ANGLE_DISTRIBUTIONS <- c(VONMISES, UNIF)

SUPPORTED_DISTRIBUTIONS <- c(
  STEP_LENGTH_DISTRIBUTIONS,
  TURN_ANGLE_DISTRIBUTIONS
)

UPDATED_DISTRIBUTION_PARAMETERS <- "updatedDistributionParameters"

updatedDistributionParameters <- setClass(
  Class = UPDATED_DISTRIBUTION_PARAMETERS,
  slots = list(
    updated_parameters = "data.frame",
    distribution_name = "character",
    grouping = "character",
    movement_data = "numeric",
    model = "ANY"
  )
)


update_unif <- function(dist, beta_cos_ta) {
  new_dist <- update_vonmises(make_vonmises_distr(kappa = 0),
    beta_cos_ta = beta_cos_ta
  )
  return(new_dist)
}


get_update_distribution_function_and_args <- function(dist_name) {
  update_fn <- methods::getFunction(stringr::str_interp("update_${dist_name}"))
  update_fn_args <- methods::formalArgs(update_fn)

  return(list(
    fn = update_fn,
    args = update_fn_args
  ))
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


update_parameters <- function(args_tibble_row, dist, update_fn) {
  args <- c(list(dist = dist), sapply(args_tibble_row[2:length(args_tibble_row)], as.numeric))

  response <- do.call(update_fn, args)
  updated_parameters <- response$params

  if (response$name == VONMISES) {
    updated_parameters$mu <- updated_parameters$mu[[1]]
  }

  return(updated_parameters)
}


get_update_fn_nvars <- function(dist_name) {
  update_fn_args <- get_update_distribution_function_and_args(dist_name)$args
  variable_args <- update_fn_args[2:length(update_fn_args)]
  return(length(variable_args))
}


validate_coef_names <- function(model, dist_name, coef_names) {
  if (!assertive::is_character(coef_names)) {
    stop(stringr::str_interp("argument 'coef_names' must be a vector of characters."))
  }


  nvar_names <- get_update_fn_nvars(dist_name)
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
        "argument 'coef_names' not valid. Variable name '${coef_name}' not found in model with coef names '${coef_names}'."
      ))
    }
  }

  sapply(coef_names, function(coef_name) {
    if (!assertive::is_numeric(model$frame[[coef_name]])) {
      stop("argument 'coef_names' must contain names that map to numeric data. Make sure you are passing either the name of the step length (e.g. sl_, log_sl_, sl_sq_) or turn angle (e.g. cos_ta_) movement coefficients in the argument 'coef_names'.")
    }
  })
}


validate_interaction_coefficients <- function(model, interaction_var_name) {
  if (!assertive::is_a_string(interaction_var_name)) {
    stop(stringr::str_interp("argument 'interaction_var_name' must be a string."))
  }

  actual_coef_names <- names(glmmTMB::fixef(model)$cond)
  valid_var_name <- any(grepl(stringr::str_interp(":${interaction_var_name}"), actual_coef_names))

  if (!valid_var_name) {
    stop(stringr::str_interp("argument 'interaction_var_name' with value '${interaction_var_name}' does not appear to be part of an interaction coefficient in the provided model."))
  }
}


validate_gamma <- function(data, coef_names) {
  actual_sl_ <- data[[coef_names[1]]]
  actual_log_sl_ <- data[[coef_names[2]]]

  raise_error <- FALSE
  tryCatch(
    expr = {
      all_equal <- suppressWarnings(all(
        berryFunctions::almost.equal(log(actual_sl_),
          actual_log_sl_,
          tolerance = 0.001
        )
      ))
      if (!all_equal) {
        raise_error <- TRUE
      }
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      raise_error <- TRUE
    },
    finally = {
      if (raise_error) {
        stop(stringr::str_interp("To update the 'gamma' distribution you need to pass a model that is fit to step lengths and log step lengths. The passed arg 'coef_names' with value '${coef_names}' are not valid variables for this distribution."))
      }
    }
  )
}


validate_exp <- function(data, coef_names) {
  actual_sl_ <- data[[coef_names[1]]]
  raise_error <- FALSE
  tryCatch(
    expr = {
      if (any(!actual_sl_ >= 0)) {
        raise_error <- TRUE
      }
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      raise_error <- TRUE
    },
    finally = {
      if (raise_error) {
        stop(stringr::str_interp("To update the 'hnorm' distribution you need to pass a model that is fit to step lengths.
                    The passed arg 'coef_names' with value '${coef_names}' are not valid variables for this distribution."))
      }
    }
  )
}


validate_hnorm <- function(data, coef_names) {
  actual_sl_sq_ <- data[[coef_names[1]]]

  raise_error <- FALSE
  tryCatch(
    expr = {
      if (any(!actual_sl_sq_ >= 0)) {
        raise_error <- TRUE
      }
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      raise_error <- TRUE
    },
    finally = {
      if (raise_error) {
        stop(stringr::str_interp("To update the 'hnorm' distribution you need to pass a model that is fit to step lengths squared.
                    The passed arg 'coef_names' with value '${coef_names}' are not valid variables for this distribution."))
      }
    }
  )
}


validate_lnorm <- function(data, coef_names) {
  actual_log_sl_ <- data[[coef_names[1]]]
  actual_log_sl_sq_ <- data[[coef_names[2]]]

  raise_error <- FALSE
  tryCatch(
    expr = {
      all_equal <- all(
        berryFunctions::almost.equal(actual_log_sl_^2,
          actual_log_sl_sq_,
          tolerance = 0.001
        )
      )
      if (!all_equal) {
        raise_error <- TRUE
      }
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      raise_error <- TRUE
    },
    finally = {
      if (raise_error) {
        stop(stringr::str_interp("To update the 'lnorm' distribution you need to pass a model that is fit to log step lengths and log step lengths squared.
                    The passed arg 'coef_names' with value '${coef_names}' are not valid variables for this distribution."))
      }
    }
  )
}


validate_ta_dist <- function(data, coef_names, dist_name) {
  actual_cos_ta_ <- data[[coef_names[1]]]

  raise_error <- FALSE
  tryCatch(
    expr = {
      if (any(!actual_cos_ta_ <= 1 | !actual_cos_ta_ >= -1)) {
        raise_error <- TRUE
      }
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      raise_error <- TRUE
    },
    finally = {
      if (raise_error) {
        stop(stringr::str_interp("To update the '${dist_name}' distribution you need to pass a model that is fit to cosine of turn angles.
                    The passed arg 'coef_names' with value '${coef_names}' are not valid variables for this distribution."))
      }
    }
  )

}


validate_vonmises <- function(data, coef_names) {
  validate_ta_dist(data, coef_names, VONMISES)
}


validate_unif <- function(data, coef_names) {
  validate_ta_dist(data, coef_names, UNIF)
}


validate_movement_data <- function(model, dist_name, coef_names) {
  validate_fn <- stringr::str_interp("validate_${dist_name}")
  do.call(validate_fn, args = list(
    data = model$frame,
    coef_names = coef_names
  ))
}


validate_base_args <- function(model, dist_name, coef_names, interaction_var_name) {
  if (!is(model, "glmmTMB")) {
    stop("argument 'model' must be of class 'glmmTMB'.")
  }

  if (!dist_name %in% SUPPORTED_DISTRIBUTIONS) {
    stop(stringr::str_interp("argument 'dist_name' must be one of ${SUPPORTED_DISTRIBUTIONS}."))
  }

  validate_coef_names(model, dist_name, coef_names)
  validate_interaction_coefficients(model, interaction_var_name)
  validate_movement_data(model, dist_name, coef_names)
}


transform_movement_data <- function(data, dist_name) {
  transformation_fns <- hash::hash(
    "gamma" = function(x) {
      return(x)
    },
    "exp" = function(x) {
      return(x)
    },
    "hnorm" = function(x) {
      return(sqrt(x))
    },
    "lnorm" = function(x) {
      return(exp(x))
    },
    "vonmises" = function(x) {
      return(acos(x))
    },
    "unif" = function(x) {
      return(acos(x))
    }
  )

  transformation_fn <- transformation_fns[[dist_name]]
  return(transformation_fn(data))
}


#' @export
fit_distribution <- function(movement_data, dist_name, na_rm) {
  return(amt::fit_distr(
    movement_data,
    dist_name = dist_name,
    na.rm = na_rm
  ))
}


get_movement_data <- function(model, movement_coef_name, dist_name) {
  data <- model$frame
  case_var <- as.character(
    attr(model$modelInfo$terms$cond$fixed, which = "variables")[2])

  movement_data <- data %>%
    dplyr::filter(!!sym(case_var) == TRUE) %>%
    dplyr::pull(movement_coef_name)

  transformed_data <- transform_movement_data(movement_data, dist_name)
  return(transformed_data)
}


get_updated_parameters <- function(model, movement_coef_name, dist_name,
                                   coefs_tibble, grouping = "category") {
  pivoted_args_tibble <- coefs_tibble %>%
    tidyr::pivot_wider(
      names_from = "coef_name",
      values_from = "coef_value"
    )


  movement_data <- get_movement_data(model = model,
                                     movement_coef_name = movement_coef_name,
                                     dist_name = dist_name)

  tentative_fitted_distribution <- fit_distribution(
    movement_data = movement_data,
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
    dist = tentative_fitted_distribution,
    update_fn = update_fn
  )

  tentative_params <- tentative_fitted_distribution$params
  tentative_row <- c(
    "tentative",
    rep(NA, ncol(pivoted_args_tibble) - 1),
    unlist(tentative_params)
  )

  updated_parameters_tibble <- rbind(
    tentative_row,
    cbind(
      pivoted_args_tibble,
      dplyr::bind_rows(all_updated_parameters)
    )
  ) %>%
    mutate(across(
      -as.character(grouping),
      function(x) round(as.numeric(x), 6)
    ))

  updated_parameters <- updatedDistributionParameters(
    updated_parameters = updated_parameters_tibble,
    distribution_name = dist_name,
    grouping = grouping,
    movement_data = movement_data,
    model = model
  )
  return(updated_parameters)
}


get_interaction_coefs <- function(coefs, coef_name, interaction_var_name) {
  regex_str <- gsub(
    "([.|()\\^{}+$*?]|\\[|\\])",
    "\\\\\\1",
    stringr::str_interp("${coef_name}:${interaction_var_name}")
  )

  interaction_coefs <- coefs[grepl(
    stringr::str_interp("^${regex_str}"), names(coefs)
  )]

  return(interaction_coefs)
}
