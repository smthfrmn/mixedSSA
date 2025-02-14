test_that("update_unif succeeds", {

})

test_that("validate_unif", {

})

test_that("validate_ta_dist", {

})


test_that("get_movement_data", {

})


test_that("validate_tentative_distribution validates not null", {
  distributions <- c(HNORM, LNORM, VONMISES)
  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    args <- list(
      dist_name = dist_name,
      tentative_dist = NULL
    )

    expected_error_msg <- stringr::str_interp(
      "arg 'tentative_dist' must not be null for distribution ${dist_name}. See amt::fit_distr."
    )
    error <- expect_error(validate_tentative_distribution(args))
    expect_equal(error$message, expected_error_msg)
  }
})


test_that("validate_tentative_distribution validates null", {
  distributions <- c(GAMMA, EXP)
  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    args <- list(
      dist_name = dist_name,
      tentative_dist = NULL
    )

    expect_no_error(validate_tentative_distribution(args))
  }
})



test_that("validate_tentative_distribution validates tentative_dist is of correct type", {
  args <- list(
    dist_name = VONMISES,
    tentative_dist = get_sample_tentative_distribution(
      dist_name = VONMISES,
      column = "cos_ta_"
    )
  )

  expect_no_error(validate_tentative_distribution(args))
})



test_that("validate_tentative_distribution validates tentative_dist is of incorrect type", {
  args <- list(
    dist_name = VONMISES,
    tentative_dist = 123
  )

  expect_error(validate_tentative_distribution(args), "arg 'tentative_dist' must be of clas 'amt_distr'")
})




test_that("transform_movement_data supports all distributions and transforms data correctly", {
  distributions <- get_supported_distributions()
  colnames <- hash(
    GAMMA = "sl_",
    EXP = "sl_",
    HNORM = "sl_sq_",
    LNORM = "log_sl_",
    VONMISES = "cos_ta_",
    UNIF = "cos_ta_"
  )

  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]

    test_movement_data <- c(0.25, 0.5, 0.75)
    results <- transform_movement_data(test_movement_data, dist_name)

    expected_results <- NULL
    if (dist_name %in% c(GAMMA, EXP)) {
      expected_results <- test_movement_data
    } else if (dist_name == HNORM) {
      expected_results <- sqrt(test_movement_data)
    } else if (dist_name == LNORM) {
      expected_results <- exp(test_movement_data)
    } else if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
      expected_results <- acos(test_movement_data)
    }

    expect_equal(results, expected_results)
  }
})


test_that("get_update_distribution_function_and_args returns correct values", {
  distributions <- get_supported_distributions()
  expected_fns <- list(
    amt::update_gamma,
    amt::update_exp,
    amt::update_hnorm,
    amt::update_lnorm,
    amt::update_vonmises,
    update_unif
  )

  expected_args <- list(
    c("dist", "beta_sl", "beta_log_sl"),
    c("dist", "beta_sl"),
    c("dist", "beta_sl_sq"),
    c("dist", "beta_log_sl", "beta_log_sl_sq"),
    c("dist", "beta_cos_ta"),
    c("dist", "beta_cos_ta")
  )

  for (i in 1:length(distributions)) {
    fn_and_args <- get_update_distribution_function_and_args(distributions[i])
    expect_equal(fn_and_args$fn, expected_fns[[i]])
    expect_equal(fn_and_args$args, expected_args[[i]])
  }
})


test_that("update_parameters for all distributions", {
  distributions <- get_supported_distributions()
  update_fns <- hash(
    "gamma" = amt::update_gamma,
    "exp" = amt::update_exp,
    "hnorm" = amt::update_hnorm,
    "lnorm" = amt::update_lnorm,
    "vonmises" = amt::update_vonmises,
    "unif" = update_unif
  )

  args_tibble_rows <- hash(
    "gamma" = tibble::tibble(
      beta_sl = 0.003,
      beta_log_sl = 0.002
    ),
    "exp" = tibble::tibble(
      beta_sl = 0.003
    ),
    "hnorm" = tibble::tibble(
      beta_sl_sq = 0.0003
    ),
    "lnorm" = tibble::tibble(
      beta_log_sl = 0.002,
      beta_log_sl_sq = 0.004
    ),
    "vonmises" = tibble::tibble(
      beta_cos_ta = 0.002
    ),
    "unif" = tibble::tibble(
      beta_cos_ta = 0 # TODO
    )
  )

  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]
    column <- ifelse(dist_name == "vonmises", "cos_ta_", "sl_")

    if (dist_name == HNORM) {
      # TODO: MAKE WORK...
      next
    }
    dist <- get_sample_tentative_distribution(dist_name = dist_name, column = column)
    update_fn <- update_fns[[dist_name]]

    args_tibble_row <- args_tibble_rows[[dist_name]]
    actual_params <- update_parameters(args_tibble_row, dist, update_fn)

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/utils/${dist_name}.rds"
    ))

    expected_params <- readRDS(file_path)
    expect_equal(actual_params, expected_params)
  }
})


test_that("get_update_fn_nvars", {
  distributions <- get_supported_distributions()

  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_"),
    c("cos_ta_")
  )

  for (i in 1:length(distributions)) {
    fn_nvars <- get_update_fn_nvars(distributions[i])
    expect_equal(fn_nvars, length(expected_params_list[[i]]))
  }
})



test_that("validate_coef_names fails missing parameters", {
  expected_error_msg <- "beta_sl not present, must pass for dist gamma."
  args <- list(model = NULL, dist_name = "gamma")
  expect_error(
    validate_coef_names(args),
    expected_error_msg
  )
})


test_that("validate_coef_names fails unmatching coefs", {
  model <- get_sample_models()[["gamma"]]

  expected_error_msg <- "distribution parameter arguments (e.g. beta_sl, beta_log_sl) must be coefficient names that exist in the model with coefficients (Intercept), sl_, log_sl_, sl_:sexF, log_sl_:sexF."
  args <- list(model = model, dist_name = "gamma", beta_sl = "foo", beta_log_sl = "foo2")
  error <- expect_error(
    validate_coef_names(args)
  )
  expect_equal(error$message, expected_error_msg)
})


test_that("validate_coef_names fails non-numeric coef data", {
  model <- get_sample_models()[["gamma"]]

  # artificially make non-numeric for check
  model$frame$sl_ <- as.character(model$frame$sl_)
  args <- list(
    model = model,
    dist_name = "gamma",
    beta_sl = "sl_",
    beta_log_sl = "log_sl_"
  )

  error_msg <- "distribution parameter arguments (e.g. beta_sl, beta_log_sl) must be coefficient names that map to numeric data. Make sure you are passing either the name of the step length (e.g. sl_, log_sl_, sl_sq_) or turn angle (e.g. cos_ta_) movement coefficients in the parameter arguments."
  error <- expect_error(validate_coef_names(args))
  expect_equal(error$message, error_msg)
})


test_that("validate_coef_names succeeds", {
})


test_that("validate_base_args fails tentative_dist NULL", {
  model <- get_sample_models()[["vonmises"]]
  dist_name <- VONMISES

  args <- list(
    model = model,
    dist_name = dist_name,
    cos_ta_ = "cos_ta_",
    tentative_dist = NULL
  )

  expect_error(
    validate_base_args(args),
    "arg 'tentative_dist' must not be null for distribution vonmises. See amt::fit_distr."
  )
})



test_that("validate_base_args fails non-glmmTMB model", {
  sample_data <- get_sample_fisher_data()
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")
  args <- list(
    model = "i am not a model",
    dist_name = "gamma",
    beta_sl = "sl_",
    beta_log_sl = "log_sl_"
  )
  error <- expect_error(
    validate_base_args(args),
    "argument 'model' must be of class 'glmmTMB'"
  )
})


test_that("validate_base_args fails non-string interaction_var_name", {
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"

  args <- list(
    model = model,
    dist_name = dist_name,
    beta_sl = "sl_",
    beta_log_sl = "log_sl_",
    interaction_var_name = 123
  )
  error <- expect_error(
    validate_base_args(args),
    "argument 'interaction_var_name' must be a string"
  )
})


test_that("validate_base_args fails interaction_var_name not in model", {
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"

  args <- list(
    model = model,
    dist_name = dist_name,
    beta_sl = "sl_",
    beta_log_sl = "log_sl_",
    interaction_var_name = "foo-sex"
  )

  error <- expect_error(
    validate_base_args(args),
    "argument 'interaction_var_name' with value 'foo-sex' does not appear to be part of an interaction coefficient in the provided model."
  )
})



test_that("validate_base_args succeeds", {
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"

  args <- list(
    model = model,
    dist_name = dist_name,
    beta_sl = "sl_",
    beta_log_sl = "log_sl_",
    interaction_var_name = "sex"
  )

  expect_no_error(validate_base_args(args))
})



test_that("validate_interaction_coefficients fails when not a string", {
  model <- get_sample_models()[["gamma"]]
  expect_error(validate_interaction_coefficients(
    model = model,
    interaction_var_name = 3
  ), "argument 'interaction_var_name' must be a string.")
})



test_that("validate_interaction_coefficients fails when not present in model", {
  model <- get_sample_models()[["gamma"]]
  expect_error(validate_interaction_coefficients(
    model = model,
    interaction_var_name = "foo-sex"
  ), "argument 'interaction_var_name' with value 'foo-sex' does not appear to be part of an interaction coefficient in the provided model.")
})




test_that("validate_interaction_coefficients succeeds", {
  model <- get_sample_models()[["gamma"]]
  expect_no_error(validate_interaction_coefficients(
    model = model,
    interaction_var_name = "sex"
  ))
})



test_that("validate_gamma fails", {
  model <- get_sample_models()[["lnorm"]]
  data <- model$frame
  expect_error(
    validate_gamma(data = data, coef_names = c("log_sl_", "log_sl_sq_"))
  )
})


test_that("validate_gamma succeeds", {
  model <- get_sample_models()[["gamma"]]
  data <- model$frame
  expect_no_error(
    validate_gamma(data = data, coef_names = c("sl_", "log_sl_"))
  )
})


test_that("validate_exp fails", {
  model <- get_sample_models()[["hnorm"]]
  data <- model$frame
  data$sl_sq_ <- data$sl_sq_ * -1
  expect_error(
    validate_exp(data = data, coef_names = c("sl_sq_"))
  )
})


test_that("validate_exp succeeds", {
  model <- get_sample_models()[["exp"]]
  data <- model$frame
  expect_no_error(
    validate_exp(data = data, coef_names = c("sl_"))
  )
})


test_that("validate_hnorm fails", {
  model <- get_sample_models()[["exp"]]
  data <- model$frame
  data$sl_ <- data$sl_ * -1
  expect_error(
    validate_exp(data = data, coef_names = c("sl_"))
  )
})


test_that("validate_hnorm succeeds", {
  model <- get_sample_models()[["exp"]]
  data <- model$frame
  expect_no_error(
    validate_exp(data = data, coef_names = c("sl_sq_"))
  )
})


test_that("validate_lnorm fails", {
  model <- get_sample_models()[["gamma"]]
  data <- model$frame
  expect_error(
    validate_lnorm(data = data, coef_names = c("sl_", "log_sl_"))
  )
})


test_that("validate_lnorm succeeds", {
  model <- get_sample_models()[["lnorm"]]
  data <- model$frame
  expect_no_error(
    validate_lnorm(data = data, coef_names = c("log_sl_", "log_sl_sq_"))
  )
})


test_that("validate_vonmises fails", {
  model <- get_sample_models()[["exp"]]
  data <- model$frame
  expect_error(
    validate_vonmises(data = data, coef_names = c("sl_"))
  )
})


test_that("validate_vonmises succeeds", {
  model <- get_sample_models()[["vonmises"]]
  data <- model$frame
  expect_no_error(
    validate_vonmises(data = data, coef_names = c("cos_ta_"))
  )
})



test_that("validate_movement_data fails", {
  model <- get_sample_models()[["exp"]]
  expect_error(
    validate_movement_data(
      model = model,
      dist_name = "vonmises",
      coef_names = c("sl_")
    )
  )
})


test_that("validate_movement_var_data succeeds", {
  model <- get_sample_models()[["gamma"]]
  expect_no_error(
    validate_movement_data(
      model = model,
      dist_name = "gamma",
      coef_names = c("sl_", "log_sl_")
    )
  )
})


test_that("get_updated_parameters with categorical interactions", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")
    model <- get_sample_models()[[dist_name]]

    coefs <- get_sample_coefs(
      dist_name = dist_name
    )
    coef_names <- get_default_coef_names(dist_name = dist_name)

    summed_coef_tibble <- get_summed_coefs_all(
      model = model,
      coefs = coefs,
      coef_names = coef_names,
      interaction_var_name = "sex",
      random_effects_var_name = NULL
    )

    mockr::local_mock(fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column))

    actual_updated_parameters <- get_updated_parameters(
      model = model,
      movement_coef_name = coef_names[1],
      dist_name = dist_name,
      coefs_tibble = summed_coef_tibble,
      tentative_dist = NULL,
      grouping_type = "category",
      interaction_var_name = "sex",
      random_effect_var_name = NULL
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/${dist_name}.rds"
    ))

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_updated_parameters_tibble <- readRDS(file_path) |>
      rename(
        grouping = interaction_var
      )

    expected_updated_parameters <- updatedDistributionParameters(
      updated_parameters = expected_updated_parameters_tibble,
      distribution_name = dist_name,
      grouping_type = "category",
      interaction_var = "sex",
      random_effect = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(actual_updated_parameters, expected_updated_parameters)
  }
})


test_that("get_updated_parameters with continuous interactions", {
  dists <- get_supported_distributions()

  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    column <- ifelse(dist_name %in% TURN_ANGLE_DISTRIBUTIONS, "ta_", "sl_")
    model <- get_sample_models(interaction_var_name = "elevation")[[dist_name]]

    coefs <- get_sample_coefs(
      dist_name = dist_name,
      interaction_var_name = "elevation"
    )

    coef_names <- get_default_coef_names(dist_name = dist_name)

    interaction_data <- model$frame$elevation

    quantile_coef_tibble <- get_quantile_coefs_all(
      interaction_data = interaction_data,
      coefs = coefs,
      coef_names = coef_names,
      interaction_var_name = "elevation",
      random_effects_var_name = NULL,
      quantiles = TEST_QUANTILES
    )

    mockr::local_mock(fit_distribution = function(movement_data, dist_name, na_rm) get_sample_tentative_distribution(dist_name = dist_name, column = column))

    actual_updated_parameters <- get_updated_parameters(
      model = model,
      movement_coef_name = coef_names[1],
      dist_name = dist_name,
      coefs_tibble = quantile_coef_tibble,
      grouping_type = "quantile",
      interaction_var_name = "elevation",
      random_effect = NULL,
      tentative_dist = NULL
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/${dist_name}.rds"
    ))

    expected_updated_parameters_tibble <- readRDS(file_path) |>
      rename(
        grouping = interaction_var
      )

    expected_movement_data <- abs(subset(data, case_ == TRUE)[[column]])

    expected_updated_parameters <- updatedDistributionParameters(
      updated_parameters = expected_updated_parameters_tibble,
      distribution_name = dist_name,
      grouping_type = "quantile",
      interaction_var = "elevation",
      random_effect = NULL,
      movement_data = expected_movement_data,
      model = model
    )

    expect_equal(actual_updated_parameters, expected_updated_parameters)
  }
})
