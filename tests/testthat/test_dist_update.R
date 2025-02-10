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

    if(dist_name %in% NEED_TENTATIVE_DIST) {
      args$tentative_dist <- get_sample_tentative_distribution(
        dist_name = dist_name, column = column)
      expected_movement_data <- NULL
    } else {
      expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])
    }

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

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "quantile",
      interaction_var = "elevation",
      random_effect = "id",
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

    if(dist_name %in% NEED_TENTATIVE_DIST) {
      args$tentative_dist <- get_sample_tentative_distribution(
        dist_name = dist_name, column = column)
      expected_movement_data <- NULL
    } else {
      expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])
    }

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

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "quantile",
      interaction_var = "elevation",
      random_effect = NULL,
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

    args <- list(model = model,
                 dist_name = dist_name,
                 random_effects_var_name = "id",
                 interaction_var_name = "elevation_fact")

    if(dist_name %in% NEED_TENTATIVE_DIST) {
      args$tentative_dist <- get_sample_tentative_distribution(
        dist_name = dist_name, column = column)
      expected_movement_data <- NULL
    } else {
      expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])
    }

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

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "category",
      interaction_var = "elevation_fact",
      random_effect = "id",
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

    if(dist_name %in% NEED_TENTATIVE_DIST) {
      args$tentative_dist <- get_sample_tentative_distribution(
        dist_name = dist_name, column = column)
      expected_movement_data <- NULL
    } else {
      expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])
    }

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

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "category",
      interaction_var = "sex",
      random_effect = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})


test_that("update_dist no_interaction random effects", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_simple_mixed_models()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(
      fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column)
    )

    args <- list(model = model,
                 dist_name = dist_name,
                 random_effects_var_name = "id")

    if(dist_name %in% NEED_TENTATIVE_DIST) {
      args$tentative_dist <- get_sample_tentative_distribution(
        dist_name = dist_name, column = column)
      expected_movement_data <- NULL
    } else {
      expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])
    }

    update_args <- switch(as.character(dist_name),
                          "gamma" = list(beta_sl = "sl_", beta_log_sl = "log_sl_"),
                          "exp" = list(beta_sl = "sl_"),
                          "hnorm" = list(beta_sl_sq = "sl_sq_"),
                          "lnorm" = list(beta_log_sl = "log_sl_", beta_log_sl_sq = "log_sl_sq_"),
                          "vonmises" = list(beta_cos_ta = "cos_ta_")
    )
    results <- do.call(update_dist, args = c(args, update_args))
    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/no_interaction/mixed/${dist_name}.rds"
    ))

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "no_interaction",
      random_effect = NULL,
      interaction_var = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    browser()
    expect_equal(results, expected_results)
  }
})


test_that("update_dist no_interaction no random effects", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()
  models <- get_sample_simple_models()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- models[[dist_name]]

    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")

    mockr::local_mock(
      fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column)
    )

    args <- list(model = model,
                 dist_name = dist_name)

    if(dist_name %in% NEED_TENTATIVE_DIST) {
      args$tentative_dist <- get_sample_tentative_distribution(
        dist_name = dist_name, column = column)
      expected_movement_data <- NULL
    } else {
      expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])
    }

    update_args <- switch(as.character(dist_name),
                          "gamma" = list(beta_sl = "sl_", beta_log_sl = "log_sl_"),
                          "exp" = list(beta_sl = "sl_"),
                          "hnorm" = list(beta_sl_sq = "sl_sq_"),
                          "lnorm" = list(beta_log_sl = "log_sl_", beta_log_sl_sq = "log_sl_sq_"),
                          "vonmises" = list(beta_cos_ta = "cos_ta_")
    )
    results <- do.call(update_dist, args = c(args, update_args))
    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/no_interaction/${dist_name}.rds"
    ))

    expected_results_tibble <- readRDS(file_path)
    expected_results <- updatedDistributionParameters(
      updated_parameters = expected_results_tibble,
      distribution_name = dist_name,
      grouping = "no_interaction",
      random_effect = NULL,
      interaction_var = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(results, expected_results)
  }
})
