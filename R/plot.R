get_row_density_data <- function(dist_name, params_row, grouping, xs) {
  if (dist_name == GAMMA) {
    ys <- stats::dgamma(
      x = xs,
      shape = as.numeric(params_row[["shape"]]),
      scale = as.numeric(params_row[["scale"]])
    )
  } else if (dist_name == EXP) {
    ys <- stats::dexp(
      x = xs,
      rate = as.numeric(params_row[["rate"]])
    )
  } else if (dist_name == HNORM) {
    ys <- extraDistr::dhnorm(
      x = xs,
      sigma = as.numeric(params_row[["sd"]])
    )
  } else if (dist_name == LNORM) {
    ys <- stats::dlnorm(
      x = xs,
      meanlog = as.numeric(params_row[["meanlog"]]),
      sdlog = as.numeric(params_row[["sdlog"]])
    )
  } else if (dist_name == VONMISES) {
    ys <- circular::dvonmises(
      x = xs,
      mu = as.numeric(params_row[["mu"]]),
      kappa = as.numeric(params_row[["kappa"]])
    )
  } else {
    stop(
      stringr::str_interp(
        "argument dist_name with value '${dist_name}' is not supported by plotting currently."
      )
    )
  }

  density_data <- cbind(xs, ys, params_row[[grouping]])
  colnames(density_data) <- c("x", "y", "grouping")

  return(density_data)
}


get_xs <- function(dist_name, movement_data) {
  if (dist_name == VONMISES) {
    xs <- seq(from = -pi, to = pi, length.out = 200)
  } else {
    xs <- seq(from = min(movement_data), to = max(movement_data), length.out = 200)
  }
  return(xs)
}


get_density_data <- function(updated_distribution_parameters, xs, include_observed) {
  dist_name <- updated_distribution_parameters@distribution_name
  grouping <- updated_distribution_parameters@grouping
  updated_parameters <- updated_distribution_parameters@updated_parameters

  if (!include_observed) {
    updated_parameters <- updated_parameters %>%
      filter(.data[[grouping]] != "observed")
  }

  density_data <- data.frame()
  for (i in 1:nrow(updated_parameters)) {
    row_density_data <- get_row_density_data(
      dist_name,
      updated_parameters[i, ],
      grouping, xs
    )
    density_data <- rbind(density_data, row_density_data)
  }

  return(density_data)
}


get_plot_data <- function(updated_distribution_parameters, include_observed) {
  xs <- get_xs(
    updated_distribution_parameters@distribution_name,
    updated_distribution_parameters@movement_data
  )

  plot_data <- get_density_data(updated_distribution_parameters, xs, include_observed)
  plot_data <- plot_data %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y)
    )

  return(plot_data)
}


#' @export
#'
#' @import ggplot2
plot_movement_distributions <- function(updated_distribution_parameters,
                                        include_observed = FALSE) {
  if (class(updated_distribution_parameters) != UPDATED_DISTRIBUTION_PARAMETERS) {
    stop(stringr::str_interp(
      "argument 'updated_distribution_parameters' must be of class '${UPDATED_DISTRIBUTION_PARAMETERS}'. Pass your fitted iSSF model to 'update_distributions_by_categorical_var' or 'update_distributions_by_continuous_var' to get the updated distribution parameters."
    ))
  }

  movement_characteristic <- ifelse(
    updated_distribution_parameters@distribution_name %in% STEP_LENGTH_DISTRIBUTIONS,
    "Step Length",
    "Turn Angle"
  )

  plot_data <- get_plot_data(updated_distribution_parameters, include_observed)
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_data,
      aes(x = x, y = y, color = grouping),
      linewidth = 1
    ) +
    labs(x = movement_characteristic, y = "Density") +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = c(0.8, 0.8),
      legend.title = element_blank()
    )

  return(plot)
}
