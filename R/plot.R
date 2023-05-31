#' # TODO: plotting
#'
#' plot_movement_distributions <- function(updated_distribution_parameters) {
#'   if (class(updated_distribution_parameters) != UPDATED_DISTRIBUTION_PARAMETERS) {
#'     stop(stringr::str_interp(
#'       "argument 'updated_distribution_parameters' must be of class '${UPDATED_DISTRIBUTION_PARAMETERS}'. Pass your fitted iSSF model to 'update_distributions_by_categorical_var' or 'update_distributions_by_continuous_var' to get the updated distribution parameters."))
#'   }
#'
#'   density_fun <- stringr::str_interp("d${updated_distributions_parameters$distribution_name}")
#'
#'   stat_functions <- apply(updated_distribution_parameters, MARGIN = 1, function(row) {
#'     browser()
#'     args <- NULL
#'     stat_function_ <- stat_function(fun = density_fun, args = list())
#'   })
#' }
