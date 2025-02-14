get_plot_data <- function(updated_dist_params_obj, vonmises_mu){

  is_vonmises <- updated_dist_params_obj@distribution_name == VONMISES
  updated_params <- updated_dist_params_obj@updated_parameters
  plots_data <- list()

  from <- ifelse(is_vonmises, -pi, 0)
  to <- ifelse(is_vonmises, pi, ceiling(max(updated_dist_params_obj@movement_data)))

  for (i in 1:nrow(updated_params)) {

    plots_data[[i]] <- data.frame(x = rep(NA, 200))
    plots_data[[i]]$x <- seq(
      from = from,
      to = to,
      length.out = 200
    )

    plots_data[[i]][["grouping"]] <- updated_params[i, "grouping"]

    if (!is.null(updated_dist_params_obj@random_effect)) {
      plots_data[[i]][["random_effect"]] <- updated_params[i, "random_effect"]
    }

    fn_name <- stringr::str_interp("d${updated_dist_params_obj@distribution_name}")
    formal_args <- formalArgs(fn_name)

    params <- updated_params |>
      dplyr::select(any_of(formal_args))

    params_args <- abs(params[i,]) |>
      as.list()


    args <- c(list(
      x = plots_data[[i]]$x),
      params_args
    )

    if (!is.null(vonmises_mu)) {
      args$mu <- circular(vonmises_mu)
    }

    plots_data[[i]]$y <- do.call(
      fn_name,
      args = args
    )

  }

  plots_data_all <- do.call(rbind, plots_data)

  if(!is.null(updated_dist_params_obj@random_effect)) {

    random_effect_str <- str_interp("random effect: ${updated_dist_params_obj@random_effect}")
    plots_data_all <- plots_data_all |>
      mutate(
        random_effect_type = ifelse(random_effect == "typical", "typical", random_effect_str)
      )

  }

  return(plots_data_all)
}


validate_plot_args <- function(args) {
  # validate vonmises_mu
  # validate include_random_effect for models params with random effects
}


#' Plot updated distributions
#'
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
#' @import circular
#' @import ggplot2
#'
#' @export
#'
#' @param updated_dist_params_obj An updatedDistributionParameters instance created from calling update_dist on an ISSA model
#' @param vonmises_mu pi or 0, default NULL. Tells the function where to center the distribution when plotting a von Mises
#' @param include_random_effect TRUE or FALSE, default F. Indicates whether or not to plot the curves per random effect variable
#' @param include_tentative_dist TRUE or FALSE, default T. Indicates whether or not to plot the tentative distribution
#'
#' @return ggplot object
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
#' plot_updated_dist(updated_params)
#'
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
#'
#' plot_updated_dist(updated_params,
#'   vonmises_mu = 0,
#'   include_random_effect = T,
#'   include_tentative = F)
#'
plot_updated_dist <- function(updated_dist_params_obj,
                              vonmises_mu = NULL,
                              include_random_effect = FALSE,
                              include_tentative = TRUE) {


  validate_plot_args()

  plot_data <- get_plot_data(
    updated_dist_params_obj = updated_dist_params_obj,
    vonmises_mu = vonmises_mu)

  if(isFALSE(include_tentative)) {
    plot_data <- plot_data |>
      filter(
        grouping != "tentative"
      )
  } else {
    plot_data <- plot_data |>
      mutate(
        random_effect = ifelse(grouping == "tentative", "tentative", random_effect)
      )
  }


  if(isFALSE(include_random_effect)) {
    plot_data <- plot_data |>
      filter(
        random_effect == "typical" | grouping == "tentative"
      )
  }


  line_args <- list(
    x = "x",
    y = "y"
  )

  line_args$col <- "grouping"


  if (include_random_effect) {
    line_args$alpha <- "random_effect_type"
    line_args$group <- "random_effect"
  }

  if (include_random_effect) {
    plot_data <- plot_data |>
      mutate(
        random_effect_grouping = paste(random_effect, grouping, sep = "_")
      )

    line_args$group <- "random_effect_grouping"
  }


  lines <- geom_line(do.call(aes_string, line_args), linewidth = 1)


  plot <- ggplot(data = plot_data) +
    lines +
    theme_bw() +
    scale_color_discrete(name = updated_dist_params_obj@interaction_var) +
    scale_alpha_discrete(guide = "none")

  if (updated_dist_params_obj@distribution_name == VONMISES) {
    plot <- plot +
      scale_x_continuous(
        breaks = c(-pi, -pi / 2, 0, pi / 2, pi),
        labels = c(expression(-pi, -pi / 2, 0, pi / 2, pi))
      ) +
      labs(x = "Relative turn angle (radians)", y = "Probability Density")
  } else {
    plot <- plot + labs(x = "Step length (m)", y = "Probability Density")
  }

  return(plot)
}
