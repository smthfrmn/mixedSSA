test_that("validate_con_args fails non-continuous interaction var type", {
  sample_data <- get_sample_fisher_data()
  dist_name <- "gamma"
  model <- get_sample_models(interaction_var_name = "sex")[[dist_name]]
  expected_error_msg <- "argument 'interaction_var_name' with value 'sex' must be a numeric (i.e. continuous) variable."

  error <- expect_error(
    validate_continuous_args(
      data = sample_data$sl_,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "sex",
      coef_names = c("sl_", "log_sl_"),
      quantiles = DEFAULT_QUANTILES
    )
  )

  expect_equal(error$message, expected_error_msg)
})


test_that("validate_con_args fails non-numeric quantiles", {
  sample_data <- get_sample_fisher_data()
  dist_name <- "gamma"
  model <- get_sample_models(interaction_var_name = "elevation")[[dist_name]]
  expected_error_msg <- "argument 'quantiles' must be a numeric vector"

  error <- expect_error(
    validate_continuous_args(
      data = sample_data$sl_,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "elevation",
      coef_names = c("sl_", "log_sl_"),
      quantiles = c("dog", "cat")
    )
  )

  expect_equal(error$message, expected_error_msg)
})


test_that("validate_con_args fails non-valid quantiles", {
  sample_data <- get_sample_fisher_data()
  dist_name <- "gamma"
  model <- get_sample_models(interaction_var_name = "elevation")[[dist_name]]
  expected_error_msg <- "argument 'quantiles' must be a numeric vector containing values between 0 and 1, e.g c(0.2, 0.4, 0.8)"

  error <- expect_error(
    validate_continuous_args(
      data = sample_data$sl_,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "elevation",
      coef_names = c("sl_", "log_sl_"),
      quantiles = c(-2, 1000, 3)
    )
  )

  expect_equal(error$message, expected_error_msg)
})




test_that("get_quantiles_coef_values", {
  mock_data <- data.frame(elevation = c(
    2, 4, 6
  ))
  result <- get_quantiles_coef_values(
    interaction_data = mock_data$elevation,
    quantiles = c(0.25, 0.5, 0.75),
    interaction_coef_values = 1,
    coef_value_vector = c(2, 2, 2)
  )

  expect_equal(result, c(5, 6, 7))
})



test_that("get_quantile_coefs", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist <- dists[i]
    mock_coefs <- get_mock_coefs(
      dist_name = dist,
      interaction_var_name = "elevation"
    )
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

    mock_coefs <- get_mock_coefs(
      dist_name = dist,
      interaction_var_name = "elevation"
    )
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


test_that("update_distributions_by_continuous_var with interaction and default coef names and custom quantiles", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models(
      data = data,
      interaction_var_name = "elevation"
    )[[dist_name]]

    if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
      column_data <- data$cos_ta_
      column_name <- "cos_ta_"
    } else {
      column_data <- data$sl_
      column_name <- "sl_"
    }


    mockr::local_mock(
      fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column_name)
    )

    results <- update_distributions_by_continuous_var(
      model = model,
      dist_name = dist_name,
      interaction_var_name = "elevation",
      quantiles = TEST_QUANTILES
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/${dist_name}.rds"
    ))
    expected_results <- readRDS(file_path)

    expect_equal(results, expected_results)
  }
})


test_that("update_distributions_by_continuous_var with custom coef names", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data(custom_coefs = TRUE)

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models_custom_coefficients(
      data = data, interaction_var_name = "elevation"
    )[[dist_name]]

    if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
      column_data <- data$turn_angle_cos
      column_name <- "turn_angle_cos"
    } else {
      column_data <- data$step_length
      column_name <- "step_length"
    }

    mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column_name))

    results <- update_distributions_by_continuous_var(
      model = model,
      dist_name = dist_name,
      interaction_var_name = "elevation",
      quantiles = TEST_QUANTILES,
      coef_names = get_sample_coef_names_by_dist(
        dist_name = dist_name,
        custom_coefs = TRUE
      )
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/${dist_name}.rds"
    ))
    expected_results <- readRDS(file_path)

    expect_equal(results, expected_results)
  }
})
