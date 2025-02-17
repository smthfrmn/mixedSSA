test_that("validate_plot_args fails random effects", {
  distributions <- SUPPORTED_DISTRIBUTIONS
  grouping_types <- c("quantile", "category", "no_interaction")
  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    for (j in 1:length(grouping_types)) {
      updated_params <- get_sample_updated_params_obj(
        dist_name = dist_name,
        grouping_type = grouping_types[j],
        mixed = F
      )

      vonmises_mu <- NULL

      if (dist_name == VONMISES) {
        vonmises_mu <- 0
      }

      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = T,
        include_tentative = F
      )

      expected_error_msg <- "argument 'include_random_effect' = TRUE is not valid for an updated_dist_params_obj with no random effects."
      error <- expect_error(validate_plot_args(args))
      expect_equal(error$message, expected_error_msg)
    }
  }
})


test_that("validate_plot_args fails vonmises_mu null", {
  updated_params <- get_sample_updated_params_obj(
    dist_name = VONMISES,
    mixed = T,
    grouping_type = "quantile"
  )

  args <- list(
    updated_dist_params_obj = updated_params,
    vonmises_mu = NULL,
    include_random_effect = T,
    include_tentative = F
  )

  expected_error_msg <- "argument 'vonmises_mu' needs to be provided if the distribution is von Mises"
  error <- expect_error(validate_plot_args(args))
  expect_equal(error$message, expected_error_msg)
})


test_that("validate_plot_args fails vonmises_mu invalid number", {
  updated_params <- get_sample_updated_params_obj(
    dist_name = VONMISES,
    mixed = T,
    grouping_type = "quantile"
  )

  args <- list(
    updated_dist_params_obj = updated_params,
    vonmises_mu = 1.2,
    include_random_effect = T,
    include_tentative = F
  )

  expected_error_msg <- "argument 'vonmises_mu' must be 0 or pi."
  error <- expect_error(validate_plot_args(args))
  expect_equal(error$message, expected_error_msg)
})


test_that("validate_plot_args fails vonmises_mu when dist not vm", {
  updated_params <- get_sample_updated_params_obj(
    dist_name = GAMMA,
    mixed = T,
    grouping_type = "quantile"
  )

  args <- list(
    updated_dist_params_obj = updated_params,
    vonmises_mu = 1.2,
    include_random_effect = T,
    include_tentative = F
  )

  expected_error_msg <- "argument 'vonmises_mu' is only valid for updated_dist_params_obj with a von Mises distribution."
  error <- expect_error(validate_plot_args(args))
  expect_equal(error$message, expected_error_msg)
})



test_that("validate_plot_args passes", {
  distributions <- SUPPORTED_DISTRIBUTIONS
  grouping_types <- c("quantile", "category", "no_interaction")
  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    for (j in 1:length(grouping_types)) {
      updated_params <- get_sample_updated_params_obj(
        dist_name = dist_name,
        grouping_type = grouping_types[j],
        mixed = T
      )

      vonmises_mu <- NULL

      if (dist_name == VONMISES) {
        vonmises_mu <- 0
      }

      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = T,
        include_tentative = T
      )

      expect_no_error(validate_plot_args(args))
    }
  }
})




test_that("get_plot_data", {


})


test_that("plot_updated_dist runs for all types random effects", {
  distributions <- SUPPORTED_DISTRIBUTIONS
  grouping_types <- c("quantile", "category", "no_interaction")
  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    for (j in 1:length(grouping_types)) {
      updated_params <- get_sample_updated_params_obj(
        dist_name = dist_name,
        grouping_type = grouping_types[j],
        mixed = T
      )

      vonmises_mu <- NULL

      if (dist_name == VONMISES) {
        vonmises_mu <- 0
      }

      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = T,
        include_tentative = T,
        plot = T
      )

      expect_no_error(do.call(plot_updated_dist, args = args))

      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = F,
        include_tentative = T,
        plot = T
      )

      expect_no_error(do.call(plot_updated_dist, args = args))


      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = T,
        include_tentative = F,
        plot = T
      )

      expect_no_error(do.call(plot_updated_dist, args = args))

      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = F,
        include_tentative = F,
        plot = T
      )

      expect_no_error(do.call(plot_updated_dist, args = args))

    }
  }
})



test_that("plot_updated_dist runs for all types no random effects", {
  distributions <- SUPPORTED_DISTRIBUTIONS
  distributions <- c(LNORM)

  grouping_types <- c("quantile", "category", "no_interaction")
  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    for (j in 1:length(grouping_types)) {
      updated_params <- get_sample_updated_params_obj(
        dist_name = dist_name,
        grouping_type = grouping_types[j],
        mixed = F
      )

      vonmises_mu <- NULL

      if (dist_name == VONMISES) {
        vonmises_mu <- 0
      }

      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = F,
        include_tentative = T,
        plot = T
      )

      expect_no_error(do.call(plot_updated_dist, args = args))


      args <- list(
        updated_dist_params_obj = updated_params,
        vonmises_mu = vonmises_mu,
        include_random_effect = F,
        include_tentative = F,
        plot = T
      )

      expect_no_error(do.call(plot_updated_dist, args = args))

    }
  }
})
