# TODO:
# - add other random effects tests


test_that("get_summed_coefs_all", {
  dists <- get_supported_distributions()
  for (i in 1:length(dists)) {
    dist_name <- dists[i]

    mock_coefs <- as.data.frame(t(unlist(get_mock_coefs(dist_name))))
    coef_names <- get_default_coef_names(dist_name)
    expected_tibble <- tibble::tibble()
    model <- get_sample_models(
      interaction_var_name = "sex"
    )[[dist_name]]

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      expected_tibble <- rbind(
        expected_tibble,
        tibble::tibble(
          grouping = SEXES,
          random_effect = rep(NA, length(SEXES)),
          coef_name = coef_name,
          coef_value = get_expected_coef_sums(
            distribution = dist_name,
            coef_index = j
          )
        )
      ) %>%
        arrange(coef_value)
    }

    actual_tibble <- get_summed_coefs_all(
      model = model,
      coefs = mock_coefs,
      coef_names = coef_names,
      random_effects_var_name = NULL,
      interaction_var_name = "sex"
    ) %>%
      arrange(coef_value)

    expect_equal(actual_tibble, expected_tibble)
  }
})


test_that("update_dist_by_categorical_var with interaction", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_models(
    data = data
  )
  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(
      fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column)
    )

    results <- update_dist_by_categorical_var(
      model = model,
      dist_name = dist_name,
      random_effects_var_name = NULL,
      interaction_var_name = "sex",
      coef_names = get_default_coef_names(dist_name),
      tentative_dist = NULL
    )


    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)

    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping_type = "category",
      interaction_var = "sex",
      random_effect = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})


test_that("update_dist_by_categorical_var with interaction with more than two categories", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_models(
    data = data,
    interaction_var_name = "sex_three_factors"
  )
  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(
      fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column)
    )

    results <- update_dist_by_categorical_var(
      model = model,
      dist_name = dist_name,
      random_effects_var_name = NULL,
      interaction_var_name = "sex_three_factors",
      coef_names = get_default_coef_names(dist_name),
      tentative_dist = NULL
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/three_factors/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)

    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping_type = "category",
      interaction_var = "sex_three_factors",
      random_effect = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})


test_that("update_dist_by_categorical_var with interaction with more than 2 categories and random effects ", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_mixed_models(
    data = data,
    interaction_var_name = "elevation_fact"
  )

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(
      fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column)
    )

    results <- update_dist_by_categorical_var(
      model = model,
      dist_name = dist_name,
      random_effects_var_name = "id",
      interaction_var_name = "elevation_fact",
      coef_names = get_default_coef_names(dist_name),
      tentative_dist = NULL
    )


    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/mixed/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)

    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping_type = "category",
      interaction_var = "elevation_fact",
      random_effect = "id",
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})


test_that("update_dist_by_categorical_var with interaction with dummy variable", {
  dummy_model <- get_sample_dummy_model()


  results <- update_dist_by_categorical_var(
    model = dummy_model,
    dist_name = "gamma",
    random_effects_var_name = "id",
    interaction_var_name = "sex",
    coef_names = get_default_coef_names("gamma"),
    tentative_dist = NULL
  )


  file_path <- here(str_interp(
    "${get_data_path_root()}/expected/categorical/legacy_mixed/gamma.rds"
  ))

  expected_movement_data <- abs(subset(data, case_ == TRUE)[["sl_"]])
  expected_results_tibble <- readRDS(file_path)

  dummy_model$frame$sex <- as.factor(dummy_model$frame$sex)
  expected_results <- updatedDistributionParameters(
    updated_parameters = expected_results_tibble,
    distribution_name = "gamma",
    grouping_type = "category",
    interaction_var = "sex",
    random_effect = "id",
    movement_data = expected_movement_data,
    model = dummy_model
  )

  expect_equal(results, expected_results)
})
