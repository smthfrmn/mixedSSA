is_categorical <- function(model, interaction_var_name) {
  interaction_var <- model$frame[[interaction_var_name]]
  return(is.factor(interaction_var))
}


is_continuous <- function(model, interaction_var_name) {
  interaction_var <- model$frame[[interaction_var_name]]
  return(is.numeric(interaction_var))
}


get_update_dist_fn <- function(model, interaction_var_name) {
  if (is.null(interaction_var_name)) {
    return("update_dist_no_interaction")
  }

  if (is_categorical(model, interaction_var_name)) {
    return("update_dist_by_categorical_var")
  } else if (is_continuous(model, interaction_var_name)) {
    return("update_dist_by_continuous_var")
  } else {
    stop("argument 'interaction_var_name' must be a variable name of a factor or continuous variable")
  }
}


get_update_dist_args <- function(args) {
  dist_name <- args$dist_name
  coef_names <- switch(as.character(dist_name),
    "gamma" = c(args$beta_sl, args$beta_log_sl),
    "exp" = c(args$beta_sl),
    "hnorm" = c(args$beta_sl_sq),
    "lnorm" = c(args$beta_log_sl, args$beta_log_sl_sq),
    "vonmises" = c(args$beta_cos_ta),
    stop(stringr::str_interp("arg 'dist_name' must be one of ${SUPPORTED_DISTRIBUTIONS}"))
  )

  update_dist_args <- list(
    model = args$model,
    dist_name = args$dist_name,
    coef_names = coef_names,
    random_effects_var_name = args$random_effects_var_name,
    tentative_dist = args$tentative_dist
  )

  if (!is.null(args$interaction_var_name)) {
    update_dist_args$interaction_var_name <- args$interaction_var_name

    if (is_continuous(args$model, args$interaction_var_name)) {
      if (!is.null(args$quantiles)) {
        update_dist_args$quantiles <- args$quantiles
      } else {
        update_dist_args$quantiles <- DEFAULT_QUANTILES
      }
    }
  }


  return(update_dist_args)
}


#' Update movement distributions based on fitted models
#'
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
#' @importFrom stats setNames
#' @importFrom tibble add_column
#' @importFrom methods is new
#' @importFrom stats coef
#'
#' @export
#'
#' @param model An ISSA model fitted using glmmTMB
#' @param dist_name The string name of the distribution for which you want to update parameters. Currently supported: 'gamma', 'exp', 'hnorm', 'lnorm', 'vonmises'.
#' @param beta_sl The string name of the step length coefficient, leave as NULL if not updating 'gamma' or 'exp' distributions.
#' @param beta_log_sl The string name of the log step length coefficient, leave as NULL if not updating 'gamma' or 'lnorm' distributions.
#' @param beta_sl_sq The string name of the step length squared coefficient, leave as NULL if not updating 'hnorm' distribution.
#' @param beta_log_sl_sq The string name of the log step length squared coefficient, leave as NULL if not updating 'lnorm' distribution.
#' @param beta_cos_ta The string name of the cos(turn angle) coefficient, leave as NULL if not updating 'vonmises' distribution.
#' @param random_effects_var_name The string name of the random effects var by which you want to update the distribution parameters.
#' @param interaction_var_name The string name of the variable in your model that is interacted with the movement variables. Can be a categorical or numerical variable.
#' @param quantiles A number vector containing the quantiles of the given interaction variable for which you want to calculate new distribution parameters. Must be between 0 and 1. Only relevant if the string passed to `interaction_var_name` points to a numeric variable.
#' @param tentative_dist a tentative distribution fit using amt:fit_distr, required when dist_name is 'lnorm', 'hnorm', or 'vonmises'.
#'
#' @return updatedDistributionParameters instance
#'
#' @examples
#'
#' library(dplyr)
#' library(amt)
#' library(glmmTMB)
#'
#' model <- glmmTMB(
#'   case_ ~ sl_ + log_sl_ + sl_:sex + log_sl_:sex,
#'   data = mixedssa_fisher_data
#' )
#'
#' updated_params <- update_dist(model,
#'   dist_name = "gamma",
#'   interaction_var_name = "sex",
#'   beta_sl = "sl_",
#'   beta_log_sl = "log_sl_"
#' )
#'
#' model <- glmmTMB(
#'   case_ ~ cos_ta_ + cos_ta_:elevation,
#'   data = mixedssa_fisher_data
#' )
#'
#' tentative_dist_data <- mixedssa_fisher_data %>%
#'   dplyr::filter(case_ == TRUE) %>%
#'   dplyr::pull("cos_ta_")
#'
#' tentative_dist <- fit_distr(
#'   tentative_dist_data,
#'   dist_name = "vonmises", na.rm = TRUE
#' )
#'
#' updated_params <- update_dist(model,
#'   dist_name = "vonmises",
#'   interaction_var_name = "elevation",
#'   beta_cos_ta = "cos_ta_",
#'   tentative_dist = tentative_dist
#' )
#'
#' model <- glmmTMB(
#'   case_ ~ cos_ta_ + cos_ta_:elevation + (0 + cos_ta_ | id),
#'   data = mixedssa_fisher_data
#' )
#'
#' updated_params <- update_dist(model,
#'   dist_name = "vonmises",
#'   interaction_var_name = "elevation",
#'   random_effects_var_name = "id",
#'   beta_cos_ta = "cos_ta_",
#'   tentative_dist = tentative_dist
#' )
#'
#' updated_params@updated_parameters
update_dist <- function(model,
                        dist_name,
                        beta_sl = NULL,
                        beta_log_sl = NULL,
                        beta_sl_sq = NULL,
                        beta_log_sl_sq = NULL,
                        beta_cos_ta = NULL,
                        random_effects_var_name = NULL,
                        interaction_var_name = NULL,
                        tentative_dist = NULL,
                        quantiles = NULL) {
  formal_args <- list()

  formal_args$model <- model
  formal_args$dist_name <- dist_name
  formal_args$beta_sl <- beta_sl
  formal_args$beta_log_sl <- beta_log_sl
  formal_args$beta_sl_sq <- beta_sl_sq
  formal_args$beta_log_sl_sq <- beta_log_sl_sq
  formal_args$beta_cos_ta <- beta_cos_ta
  formal_args$random_effects_var_name <- random_effects_var_name
  formal_args$interaction_var_name <- interaction_var_name
  formal_args$tentative_dist <- tentative_dist
  formal_args$quantiles <- quantiles

  validate_base_args(formal_args)

  update_dist_fn <- get_update_dist_fn(model, interaction_var_name)
  update_dist_args <- get_update_dist_args(formal_args)

  updated_dist_parameters <- do.call(update_dist_fn, args = update_dist_args)
  return(updated_dist_parameters)
}
