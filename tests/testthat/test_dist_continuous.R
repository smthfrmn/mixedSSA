test_that("validate_con_args fails non-numeric quantiles", {
  expected_error_msg <- "argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)"

  error <- expect_error(
    validate_continuous_args(
      quantiles = c("dog", "cat")
    )
  )

  expect_equal(error$message, expected_error_msg)
})


test_that("validate_con_args fails non-valid quantiles", {
  expected_error_msg <- "argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)"

  error <- expect_error(
    validate_continuous_args(
      quantiles = c(-2, 1000, 3)
    )
  )

  expect_equal(error$message, expected_error_msg)
})



test_that("update_dist_by_continuous_var with custom quantiles", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models(
      data = data, interaction_var_name = "elevation"
    )[[dist_name]]


    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column))

    results <- update_dist_by_continuous_var(
      model = model,
      dist_name = dist_name,
      random_effects_var = NULL,
      interaction_var_name = "elevation",
      quantiles = TEST_QUANTILES,
      coef_names = get_sample_coef_names_by_dist(
        dist_name = dist_name
      ),
      tentative_dist = NULL
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path) |>
      rename(
        grouping = interaction_var
      )

    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping_type = "quantile",
      interaction_var = "elevation",
      random_effect = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})


test_that("update_dist_by_continuous_var with custom quantiles and random effects", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_mixed_models(
      data = data, interaction_var_name = "elevation"
    )[[dist_name]]


    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column))

    results <- update_dist_by_continuous_var(
      model = model,
      dist_name = dist_name,
      random_effects_var_name = "id",
      interaction_var_name = "elevation",
      quantiles = TEST_QUANTILES,
      coef_names = get_sample_coef_names_by_dist(
        dist_name = dist_name
      ),
      tentative_dist = NULL
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/mixed/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)

    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping_type = "quantile",
      interaction_var = "elevation",
      random_effect = "id",
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})
