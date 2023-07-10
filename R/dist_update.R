# TODO:
# - support non-interaction variable
# - only allow quantiles to be passed if cont variable?

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
    # TODO: add support for non-interaction
    return(NULL)
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
    interaction_var_name = args$interaction_var_name
  )

  if (!is.null(args$interaction_var_name)) {
    if (is_continuous(args$model, args$interaction_var_name)) {
      update_dist_args$quantiles <- args$quantiles
    }
  }

  return(update_dist_args)
}


#' Update movement distributions based on fitted models
#'
#' @export
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
update_dist <- function(model,
                        dist_name,
                        beta_sl = NULL,
                        beta_log_sl = NULL,
                        beta_sl_sq = NULL,
                        beta_log_sl_sq = NULL,
                        beta_cos_ta = NULL,
                        random_effects_var_name = NULL,
                        interaction_var_name = NULL,
                        quantiles = DEFAULT_QUANTILES) {
  args <- DesignLibrary::match.call.defaults()
  args$model <- model
  validate_base_args(args)

  update_dist_fn <- get_update_dist_fn(model, interaction_var_name)
  update_dist_args <- get_update_dist_args(args)

  updated_dist_parameters <- do.call(update_dist_fn, args = update_dist_args)
  return(updated_dist_parameters)
}
