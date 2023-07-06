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


test_that("get_quantiles_coef_values", {
  mock_data <- data.frame(elevation = c(
    2, 4, 6
  ))

  target_coefs <- data.frame(
    "sl_" = c(2),
    "sl_:elevation" = c(2)
  )

  result <- get_quantiles_coef_values(
    interaction_data = mock_data$elevation,
    quantiles = c(0.25, 0.5, 0.75),
    target_coefs = target_coefs[1, ]
  )

  expect_equal(result, c(8, 10, 12))
})


test_that("get_quantile_coefs", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- as.data.frame(t(unlist(get_mock_coefs(
      dist_name = dist,
      interaction_var_name = "elevation"
    ))))
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_tibble <- tibble::tibble(
        quantile = TEST_QUANTILES,
        coef_name = coef_name,
        coef_value = get_expected_coef_sums(
          distribution = dist,
          coef_index = j,
          quantiles = TEST_QUANTILES
        )
      ) %>%
        arrange(coef_value)

      actual_tibble <- get_quantile_coefs(
        interaction_data = data$elevation,
        coefs = mock_coefs,
        coef_name = coef_name,
        interaction_var_name = "elevation",
        quantiles = TEST_QUANTILES
      ) %>%
        arrange(coef_value)

      expect_equal(
        actual_tibble,
        expected_tibble
      )
    }
  }
})


test_that("get_quantile_coefs_all", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- as.data.frame(t(unlist(get_mock_coefs(
      dist_name = dist,
      interaction_var_name = "elevation"
    ))))
    coef_names <- get_default_coef_names(dists[i])
    expected_tibble <- tibble::tibble()

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      current_tibble <- tibble::tibble(
        quantile = TEST_QUANTILES,
        coef_name = coef_name,
        coef_value = get_expected_coef_sums(
          distribution = dist,
          coef_index = j,
          quantiles = TEST_QUANTILES
        )
      )

      expected_tibble <- rbind(
        expected_tibble,
        current_tibble
      )
    }

    actual_tibble <- get_quantile_coefs_all(
      interaction_data = data$elevation,
      coefs = mock_coefs,
      coef_names = coef_names,
      interaction_var_name = "elevation",
      quantiles = TEST_QUANTILES
    )

    expect_equal(actual_tibble, expected_tibble)
  }
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
      )
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "quantile",
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})
