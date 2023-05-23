test_that("get_categories_from_coefs", {
  dists <- get_supported_distributions()
  expected_categories <- c("F")

  for (i in 1:length(dists)) {
    coefs <- get_mock_coefs(dists[i])
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      interaction_coefs <- coefs[grepl(str_interp("^${coef_name}:sex"), names(coefs))]
      categories <- get_categories_from_coefs(interaction_coefs, "sex")
      expect_equal(categories, expected_categories)
    }
  }
})


test_that("validate_categorical_args fails non-categorical interaction var type", {
  sample_data <- get_sample_fisher_data()

  # get continuous interaction model
  dist_name <- "gamma"
  model <- get_sample_models(interaction_var_name = "elevation")[[dist_name]]
  expected_error_msg <- "argument 'interaction_var_name' with value 'elevation' must be a factor (i.e. categorical) variable."

  error <- expect_error(
    validate_categorical_args(
      model = model,
      dist_name = dist_name,
      coef_names = c("sl_", "log_sl_"),
      interaction_var_name = "elevation"
    )
  )

  expect_equal(error$message, expected_error_msg)
})


test_that("get_summed_coefs", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]
    mock_coefs <- get_mock_coefs(dist)
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_tibble <- tibble::tibble(
        category = SEXES,
        coef_name = coef_name,
        coef_value = get_expected_coef_sums(
          distribution = dist,
          coef_index = j
        )
      ) %>%
        arrange(coef_value)

      actual_tibble <- get_summed_coefs(
        mock_coefs, coef_name,
        interaction_var_name = "sex",
        reference_category = REFERENCE_CATEGORY
      ) %>%
        arrange(coef_value)

      expect_equal(
        actual_tibble,
        expected_tibble
      )
    }
  }
})


test_that("get_summed_coefs_all", {
  dists <- get_supported_distributions()
  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- get_mock_coefs(dist)
    coef_names <- get_default_coef_names(dists[i])
    expected_tibble <- tibble::tibble()
    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      expected_tibble <- rbind(
        expected_tibble,
        tibble::tibble(
          category = SEXES,
          coef_name = coef_name,
          coef_value = get_expected_coef_sums(
            distribution = dist,
            coef_index = j
          )
        )
      ) %>%
        arrange(coef_value)
    }

    actual_tibble <- get_summed_coefs_all(
      coefs = mock_coefs,
      coef_names = coef_names,
      interaction_var_name = "sex",
      reference_category = REFERENCE_CATEGORY
    ) %>%
      arrange(coef_value)

    expect_equal(actual_tibble, expected_tibble)
  }
})


test_that("update_distributions_by_categorical_var with interaction and default coef names", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models(
      data = data
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

    results <- update_distributions_by_categorical_var(
      data = column_data,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "sex",
      reference_category = REFERENCE_CATEGORY
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/updated_params_interactions/${dist_name}.rds"
    ))
    expected_results <- readRDS(file_path)

    expect_equal(results, expected_results)
  }
})


test_that("update_distributions_by_categorical_var with custom coef names", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data(custom_coefs = TRUE)

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models_custom_coefficients(
      data = data
    )[[dist_name]]

    if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
      column_data <- data$turn_angle_cos
      column_name <- "turn_angle_cos"
    } else {
      column_data <- data$step_length
      column_name <- "step_length"
    }

    mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column_name))

    results <- update_distributions_by_categorical_var(
      data = column_data,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "sex",
      coef_names = get_sample_coef_names_by_dist(
        dist_name = dist_name,
        custom_coefs = TRUE
      ),
      reference_category = REFERENCE_CATEGORY
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/updated_params_interactions/${dist_name}.rds"
    ))
    expected_results <- readRDS(file_path)
    expect_equal(results, expected_results)
  }
})
