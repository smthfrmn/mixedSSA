test_that("get_non_interaction_coefs", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- as.data.frame(t(unlist(get_mock_coefs(
      dist_name = dist,
      interaction_var_name = NULL
    ))))

    coef_names <- get_default_coef_names(dists[i])

    expected_tibble <- tibble::tibble(
      grouping = NA,
      coef_value = unlist(
        mock_coefs[1, 2:length(mock_coefs)],
        use.names = F),
      coef_name = coef_names
    )

    actual_tibble <- get_non_interaction_coefs(
      coefs = mock_coefs,
      coef_name = coef_names,
      random_effects_var_name = NULL
    )

    expect_equal(
      actual_tibble,
      expected_tibble
    )
  }
})


test_that("get_non_interaction_coefs with random_effects", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- get_simple_mock_mixed_coefs(dist)
    coef_names <- get_default_coef_names(dists[i])

    browser()

    expected_tibble <- tibble::tibble(
      grouping = NA,
      coef_value = rep(1:nrow(mock_coefs), length(coef_names)),
      coef_name = rep(coef_names, nrow(mock_coefs)),
      id = rep(c("F1", "F2", "M1", "M4"), length(coef_names))
    )

    actual_tibble <- get_non_interaction_coefs(
      coefs = mock_coefs,
      coef_name = coef_names,
      random_effects_var_name = "id"
    )


    expect_equal(
      actual_tibble,
      expected_tibble
    )
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
      ),
      tentative_dist = NULL
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
      grouping = "quantile",
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})
