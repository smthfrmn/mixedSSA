get_plot_data <- function(updated_dist_params_obj, vonmises_mu,
                          include_tentative, include_random_effect) {
  is_vonmises <- updated_dist_params_obj@distribution_name == VONMISES
  updated_params <- updated_dist_params_obj@updated_parameters
  plots_data <- list()

  from <- ifelse(is_vonmises, -pi, 0)
  to <- ifelse(
    is_vonmises,
    pi,
    ceiling(max(updated_dist_params_obj@movement_data))
  )


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

    formal_args <- methods::formalArgs(fn_name)

    params <- updated_params |>
      dplyr::select(any_of(formal_args))

    params_args <- abs(params[i, ]) |>
      as.list()


    args <- c(
      list(
        x = plots_data[[i]]$x
      ),
      params_args
    )

    if (!is.null(vonmises_mu)) {
      args$mu <- circular(vonmises_mu)
    }

    suppressWarnings({
      plots_data[[i]]$y <- do.call(
        fn_name,
        args = args
      )
    })
  }

  plots_data_all <- do.call(rbind, plots_data)

  if (isFALSE(include_tentative)) {
    plots_data_all <- plots_data_all |>
      filter(
        grouping != "tentative"
      )
  }

  has_random_effect <- !is.null(updated_dist_params_obj@random_effect)

  if (has_random_effect) {
    # if(isTRUE(include_tentative)) {
    #   plots_data_all <- plots_data_all |>
    #     mutate(
    #       random_effect = ifelse(grouping == "tentative", "tentative", random_effect)
    #     )
    # }

    if (isFALSE(include_random_effect)) {
      plots_data_all <- plots_data_all |>
        filter(
          random_effect == "typical" | grouping == "tentative"
        )
    } else {
      plots_data_all <- plots_data_all |>
        mutate(
          random_effect_grouping = paste(random_effect, grouping, sep = "_")
        )
    }

    plots_data_all <- plots_data_all |>
      mutate(
        random_effect_type = ifelse(random_effect == "typical" | is.na(random_effect),
          random_effect, updated_dist_params_obj@random_effect
        ),
      )
  }

  plots_data_all <- plots_data_all |>
    filter(!is.infinite(y)) |>
    mutate(
      is_tentative = ifelse(grouping == "tentative", "yes", "no")
    )

  return(plots_data_all)
}


validate_plot_args <- function(args) {
  if (isTRUE(args$include_random_effect) & is.null(args$updated_dist_params_obj@random_effect)) {
    stop("argument 'include_random_effect' = TRUE is not valid for an updated_dist_params_obj with no random effects.")
  }

  if (!is.null(args$vonmises_mu) & args$updated_dist_params_obj@distribution_name != VONMISES) {
    stop("argument 'vonmises_mu' is only valid for updated_dist_params_obj with a von Mises distribution.")
  }


  if (is.null(args$vonmises_mu) & args$updated_dist_params_obj@distribution_name == VONMISES) {
    stop("argument 'vonmises_mu' needs to be provided if the distribution is von Mises")
  }

  if (!is.null(args$vonmises_mu)) {
    if (!args$vonmises_mu %in% c(0, pi)) {
      stop("argument 'vonmises_mu' must be 0 or pi.")
    }
  }
}


#' Plot updated distributions
#'
#'
#' @import dplyr
#' @import amt
#' @import glmmTMB
#' @import circular
#' @import ggplot2
#' @import extraDistr
#'
#' @export
#'
#' @param updated_dist_params_obj `[updatedDistributionParameters]` The output from calling update_dist on an ISSA model
#' @param vonmises_mu `[numeric(1)]{NULL}`, pi or 0, optional parameter. Tells the function where to center the distribution when plotting a von Mises
#' @param include_random_effect `[logical(1)]{FALSE}`. Indicates whether or not to plot the curves per random effect variable
#' @param include_tentative `[logical(1)]{TRUE}`. Indicates whether or not to plot the tentative distribution
#' @param print_plot `[logical(1)]{TRUE}`. Indicates whether or not to plot the plot before returning it
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
#'   random_effects_var_name = "id",
#'   beta_cos_ta = "cos_ta_",
#'   tentative_dist = tentative_dist
#' )
#'
#'
#' plot_updated_dist(updated_params,
#'   vonmises_mu = 0,
#'   include_random_effect = TRUE,
#'   include_tentative = FALSE
#' )
#'
plot_updated_dist <- function(updated_dist_params_obj,
                              vonmises_mu = NULL,
                              include_random_effect = FALSE,
                              include_tentative = TRUE,
                              print_plot = TRUE) {
  args <- list(
    updated_dist_params_obj = updated_dist_params_obj,
    vonmises_mu = vonmises_mu,
    include_random_effect = include_random_effect,
    include_tentative = include_tentative
  )

  validate_plot_args(args = args)

  plot_data <- get_plot_data(
    updated_dist_params_obj = updated_dist_params_obj,
    vonmises_mu = vonmises_mu,
    include_tentative = include_tentative,
    include_random_effect = include_random_effect
  )

  line_args <- list(
    x = "x",
    y = "y",
    col = "grouping",
    linetype = "is_tentative"
  )

  if (include_random_effect) {
    line_args$alpha <- "random_effect_type"
    line_args$group <- "random_effect_grouping"
  }


  suppressWarnings({
    lines <- geom_line(do.call(aes_string, line_args), linewidth = 1)


    plot <- ggplot(data = plot_data) +
      lines +
      theme_bw() +
      scale_color_discrete(name = paste(updated_dist_params_obj@interaction_var,
        updated_dist_params_obj@grouping_type,
        sep = " "
      )) +
      scale_alpha_discrete(na.translate = FALSE, range = c(0.2, 1), name = "random effect") +
      scale_linetype_manual(values = c("yes" = "dotted", "no" = "solid"), guide = "none") +
      ylim(0, max(plot_data$y) + 0.005)



    if (updated_dist_params_obj@distribution_name == VONMISES) {
      plot <- plot +
        scale_x_continuous(
          breaks = c(-pi, -pi / 2, 0, pi / 2, pi),
          labels = c(expression(-pi, -pi / 2, 0, pi / 2, pi))
        ) +
        labs(x = "Relative turn angle (radians)", y = "Probability Density")
    } else {
      plot <- plot +
        labs(x = "Step length (m)", y = "Probability Density")
    }
  })


  if (isTRUE(print_plot)) {
    print(plot)
  }

  return(plot)
}
