#library(data.table)
#library(tidyverse)
#library(here)
#library(assertive)
# data("deer")

# TODO:
# - include random effects in update functions
# - include log relative selection function
# - what is the relative probability selecting one location over another?
# - delta method
# - continuous updating distributions:
#     - 5, 50, 95%
# - plotting updated distributions


# ssf1 <- deer |>
#   steps_by_burst() |>
#   random_steps() |>
#   mutate(
#     log_sl_ = log(sl_),
#     cos_ta_ = cos(ta_)
#   )
#
# ssf1$animal <- sample(c("cat", "dog", "mouse", "snake"), size = nrow(ssf1), replace = TRUE)
#
# model <- glmmTMB(case_ ~ sl_ + log_sl_ + cos_ta_ + sl_:animal + log_sl_:animal + cos_ta_:animal, data = ssf1)
# model <- readRDS(here("Snapper.ssf_sum_6min_all.rds"))

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


get_updated_parameters <- function(args_df_row, dist, update_fn) {
  args <- c(list(dist = dist), sapply(args_df_row[2:length(args_df_row)], as.numeric))
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


# TODO: add support for means coding

#' Update movement distributions based on fitted models
#'
#' @export
#'
#' @import dplyr
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
  observed_fitted_distribution <- amt::fit_distr(data, distribution, na.rm = TRUE)

  coefficient_names <- if (is.null(coefficient_names)) get_default_coefficient_names(distribution) else coefficient_names

  update_fn_and_args <- get_update_distribution_function_and_args(distribution)
  update_fn <- update_fn_and_args$fn
  update_fn_arg_names <- update_fn_and_args$args

  args_df <- data.frame()

  for (i in 1:length(coefficient_names)) {
    args_str <- coefficient_names[i]
    interaction_coefficients <- names(coefs) %>%
      str_detect(pattern = str_interp("^${args_str}:")) %>%
      keep(coefs, .)

    interaction_coefficient_name <- names(interaction_coefficients)

    category <- sapply(interaction_coefficient_name, function(name) {
      return(str_extract(name, regex("(?<=:).*")))
    })

    interaction_coefficient_value <- unname(interaction_coefficients)
    nrows <- length(interaction_coefficient_value)

    coefficient_name <- rep(args_str, nrows)
    coefficient_value <- rep(coefs[args_str], nrows)

    non_interaction_row <- cbind(
      interaction_coefficient_name = args_str,
      category = reference_category,
      interaction_coefficient_value = 0,
      coefficient_name = args_str,
      coefficient_value = coefs[args_str]
    )

    args_df <- rbind(
      args_df,
      cbind(
        interaction_coefficient_name,
        category,
        interaction_coefficient_value,
        coefficient_name, coefficient_value
      ),
      non_interaction_row
    )
  }

  pivoted_args_df <- args_df %>%
    mutate(
      interaction_coefficient_value = as.numeric(interaction_coefficient_value),
      coefficient_value = as.numeric(coefficient_value),
      coefficient_value_sum = interaction_coefficient_value + coefficient_value
    ) %>%
    pivot_wider(
      names_from = coefficient_name,
      values_from = coefficient_value_sum
    ) %>%
    group_by(category) %>%
    summarize(across(coefficient_names, mean, na.rm = TRUE))

  colnames(pivoted_args_df) <- c("category", update_fn_and_args$args[2:length(update_fn_and_args$args)])
  all_updated_parameters <- apply(pivoted_args_df, 1,
    get_updated_parameters_,
    dist = observed_fitted_distribution,
    update_fn = update_fn
  )

  observed_params <- observed_fitted_distribution$params
  observed_row <- c("observed", NA, NA, observed_params$shape, observed_params$scale)
  final_df <- rbind(
    observed_row,
    cbind(pivoted_args_df, rbindlist(all_updated_parameters))
  )


  return(final_df)
}
