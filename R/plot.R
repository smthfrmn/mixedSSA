# TODO: plotting

get_stat_function_args <- function(dist_name, params_row) {
  if (dist_name == GAMMA) {
    args <- list(fun = stats::dgamma,
                 args = list(
                   shape = params_row[["shape"]],
                   scale = params_row[["scale"]]
                 ))
  } else if (dist_name == EXP) {
    args <- list(fun = stats::dexp,
                 args = list(
                   rate = params_row[["rate"]]
                   ))
  } else if (dist_name == HNORM) {
    args <- list(fun = extraDistr:dhnorm,
                 args = list(
                   sigma = params_row[["sd"]]
                 ))
  } else if (dist_name == LNORM) {
    args <- list(fun = stats::dlnorm,
                 args = list(
                   meanlog = params_row[["meanlog"]],
                   sdlog = params_row[["sdlog"]]
                 ))
  } else if (dist_name == VONMISES) {
    args <- list(fun = circular::dvonmises,
                 args = list(
                   mu = params_row[["mu"]],
                   kappa = params_row[["kappa"]]
                 ))

  } else {
    stop(stringr::str_interp("argument dist_name with value '${dist_name}' is not supported by plotting currently."))
  }

  args$args <- as.numeric(args$args)
  return(args)
}


get_stat_functions <- function(updated_distribution_parameters) {
  dist_name <- updated_distribution_parameters@distribution_name

  updated_parameters <- updated_distribution_parameters@updated_parameters
  stat_functions <- apply(updated_parameters, MARGIN = 1, function(row) {
    args <- get_stat_function_args(dist_name, row)
    stat_function_ <- ggplot2::stat_function(fun = args$fun, args = args$args)
  })

  return(stat_functions)
}


#' @export
plot_movement_distributions <- function(updated_distribution_parameters) {
  if (class(updated_distribution_parameters) != UPDATED_DISTRIBUTION_PARAMETERS) {
    stop(stringr::str_interp(
      "argument 'updated_distribution_parameters' must be of class '${UPDATED_DISTRIBUTION_PARAMETERS}'. Pass your fitted iSSF model to 'update_distributions_by_categorical_var' or 'update_distributions_by_continuous_var' to get the updated distribution parameters."))
  }


  stat_functions <- get_stat_functions(updated_distribution_parameters)

  plot <- ggplot2::ggplot() + stat_functions

  return(plot)
}
