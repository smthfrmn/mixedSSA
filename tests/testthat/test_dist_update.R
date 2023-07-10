test_that("update_dist continuous random effects", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_mixed_models(
    data = data, interaction_var_name = "elevation"
  )
  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column))

    args <- list(model = model,
                 dist_name = dist_name,
                 random_effects_var_name = "id",
                 interaction_var_name = "elevation",
                 quantiles = TEST_QUANTILES)

    update_args <- switch(as.character(dist_name),
                          "gamma" = list(beta_sl = "sl_", beta_log_sl = "log_sl_"),
                          "exp" = list(beta_sl = "sl_"),
                          "hnorm" = list(beta_sl_sq = "sl_sq_"),
                          "lnorm" = list(beta_log_sl = "log_sl_", beta_log_sl_sq = "log_sl_sq_"),
                          "vonmises" = list(beta_cos_ta = "cos_ta_")
    )

    results <- do.call(update_dist, args = c(args, update_args))

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


test_that("update_dist continuous no random effects", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_models(
    data = data, interaction_var_name = "elevation"
  )
  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column))

    args <- list(model = model,
                 dist_name = dist_name,
                 interaction_var_name = "elevation",
                 quantiles = TEST_QUANTILES)

    update_args <- switch(as.character(dist_name),
                          "gamma" = list(beta_sl = "sl_", beta_log_sl = "log_sl_"),
                          "exp" = list(beta_sl = "sl_"),
                          "hnorm" = list(beta_sl_sq = "sl_sq_"),
                          "lnorm" = list(beta_log_sl = "log_sl_", beta_log_sl_sq = "log_sl_sq_"),
                          "vonmises" = list(beta_cos_ta = "cos_ta_")
    )

    results <- do.call(update_dist, args = c(args, update_args))


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


test_that("update_dist categorical random effects", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_mixed_models(
    data = data
  )
  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(
      fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column)
    )

    args <- list(model = model,
                 dist_name = dist_name,
                 random_effects_var_name = "id",
                 interaction_var_name = "sex")

    update_args <- switch(as.character(dist_name),
                          "gamma" = list(beta_sl = "sl_", beta_log_sl = "log_sl_"),
                          "exp" = list(beta_sl = "sl_"),
                          "hnorm" = list(beta_sl_sq = "sl_sq_"),
                          "lnorm" = list(beta_log_sl = "log_sl_", beta_log_sl_sq = "log_sl_sq_"),
                          "vonmises" = list(beta_cos_ta = "cos_ta_")
    )
    results <- do.call(update_dist, args = c(args, update_args))


    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/mixed/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "category",
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})


test_that("update_dist categorical no random effects", {
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

    args <- list(model = model,
                 dist_name = dist_name,
                 interaction_var_name = "sex")

    update_args <- switch(as.character(dist_name),
                          "gamma" = list(beta_sl = "sl_", beta_log_sl = "log_sl_"),
                          "exp" = list(beta_sl = "sl_"),
                          "hnorm" = list(beta_sl_sq = "sl_sq_"),
                          "lnorm" = list(beta_log_sl = "log_sl_", beta_log_sl_sq = "log_sl_sq_"),
                          "vonmises" = list(beta_cos_ta = "cos_ta_")
    )
    results <- do.call(update_dist, args = c(args, update_args))


    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "category",
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})
