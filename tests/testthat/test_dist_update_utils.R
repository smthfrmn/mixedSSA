# TODO: test special characters in coef names doesn't mess everything up

test_that("get_update_distribution_function_and_args returns correct values", {
  distributions <- get_supported_distributions()
  expected_fns <- list(
    amt::update_gamma,
    amt::update_exp,
    amt::update_hnorm,
    amt::update_lnorm,
    amt::update_vonmises
  )
  expected_args <- list(
    c("dist", "beta_sl", "beta_log_sl"),
    c("dist", "beta_sl"),
    c("dist", "beta_sl_sq"),
    c("dist", "beta_log_sl", "beta_log_sl_sq"),
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
    "vonmises" = amt::update_vonmises
  )


  args_tibble_rows <- hash(
    "gamma" = tibble::tibble(
      category = "placeholder",
      beta_sl = 0.003,
      beta_log_sl = 0.002
    ),
    "exp" = tibble::tibble(
      category = "placeholder",
      beta_sl = 0.003
    ),
    "hnorm" = tibble::tibble(
      category = "placeholder",
      beta_sl_sq = 0.0003
    ),
    "lnorm" = tibble::tibble(
      category = "placeholder",
      beta_log_sl = 0.002,
      beta_log_sl_sq = 0.004
    ),
    "vonmises" = tibble::tibble(
      category = "placeholder",
      beta_cos_ta = 0.002
    )
  )

  expected_params <- hash(
    "gamma" = list(
      shape = 0.62691709,
      scale = -5483.4104
    ),
    "exp" = list(
      rate = 0.001508273
    ),
    "hnorm" = list(
      sd = 0 # TODO: not working
    ),
    "lnorm" = list(
      meanlog = 4.5511495,
      sdlog = 1.9637997
    ),
    "vonmises" = list(
      kappa = 2.4857535,
      mu = 0
    )
  )

  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]
    column <- ifelse(dist_name == "vonmises", "cos_ta_", "sl_")

    if (dist_name == "hnorm") {
      # TODO: ACTUALLY FIX THIS...
      next
    }

    dist <- get_sample_observed_distribution(dist_name = dist_name, column = column)
    update_fn <- update_fns[[dist_name]]

    current_expected_params <- expected_params[[dist_name]]

    args_tibble_row <- args_tibble_rows[[dist_name]]
    actual_params <- update_parameters(args_tibble_row, dist, update_fn)

    expect_equal(actual_params, current_expected_params)
  }
})


test_that("get_default_coef_names", {
  distributions <- get_supported_distributions()

  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  for (i in 1:length(distributions)) {
    default_coefs <- get_default_coef_names(distributions[i])
    expect_equal(default_coefs, expected_params_list[[i]])
  }
})



test_that("validate_coef_names fails non-character coef_names", {

})



test_that("validate_coef_names fails number of args", {
  distributions <- get_supported_distributions()
  expected_number_params <- c(2, 1, 1, 2, 1)

  for (i in 1:length(distributions)) {
    expected_num <- expected_number_params[i]
    wrong_num_coef_names <- rep("coef_name", expected_num + 1)
    model <- NULL # don't need this for this test
    dist_name <- distributions[i]

    expected_param_str <- ifelse(expected_num == 1, "parameter", "parameters")
    expected_error_msg <- stringr::str_interp(
      "distribution ${dist_name} expects ${expected_num} ${expected_param_str} to be passed, not ${expected_num + 1}"
    )
    expect_error(
      validate_coef_names(
        model = model,
        dist_name = dist_name,
        coef_names = wrong_num_coef_names
      ),
      expected_error_msg
    )
  }
})


test_that("validate_coef_names fails unmatching coef names", {
  distributions <- get_supported_distributions()

  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  actual_params_list <- sapply(expected_params_list, function(x) {
    return(unname(sapply(x, function(y) {
      stringr::str_interp("${y}foo_")
    })))
  })

  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]
    model <- get_sample_models()[[dist_name]]

    wrong_coef_names <- actual_params_list[[i]]
    expect_error(validate_coef_names(
      model = model,
      dist_name = dist_name,
      coef_names = wrong_coef_names
    ))
  }
})


test_that("validate_coef_names fails non-numeric coef data", {
  model <- get_sample_models()[["gamma"]]

  # artificially make non-numeric for check
  model$frame$sl_ <- as.character(model$frame$sl_)

  expect_error(validate_coef_names(
    model = model,
    dist_name = "gamma",
    coef_names = c("sl_", "log_sl_")
  ))
})


test_that("validate_coef_names succeeds", {
  distributions <- get_supported_distributions()
  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  for (i in 1:length(distributions)) {
    dist_name <- distributions[i]
    model <- get_sample_models()[[dist_name]]

    right_coef_names <- expected_params_list[[i]]

    expect_no_error(validate_coef_names(
      model = model,
      dist_name = dist_name,
      coef_names = right_coef_names
    ))
  }
})


test_that("validate_base_args fails non-glmmTMB model", {
  sample_data <- get_sample_fisher_data()
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_base_args(
    model = "I am not a model",
    dist_name = dist_name,
    coef_names = coef_names,
    interaction_var_name = "sex"
  ), "argument 'model' must be of class 'glmmTMB'")
})


test_that("validate_base_args fails non-string interaction_var_name", {
  sample_data <- get_sample_fisher_data()
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_base_args(
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    interaction_var_name = 123
  ), "argument 'interaction_var_name' must be a string")
})


test_that("validate_base_args fails interaction_var_name not in model", {
  sample_data <- get_sample_fisher_data()
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_base_args(
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    interaction_var_name = "foo-sex"
  ), "argument 'interaction_var_name' with value foo-sex does not appear to be part of an interaction coefficient in the provided model.")
})



test_that("validate_base_args succeeds with user-passed coef names", {
  sample_data <- get_sample_fisher_data()
  model <- get_sample_models_custom_coefficients()[["gamma"]]
  dist_name <- "gamma"
  coef_names <- c("step_length", "step_length_log")

  expect_no_error(validate_base_args(
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    interaction_var_name = "sex"
  ))
})



test_that("validate_interaction_coefficients fails when not present in model", {
  model <- get_sample_models_custom_coefficients()[["gamma"]]
  expect_error(validate_interaction_coefficients(
    model = model,
    interaction_var_name = 3
  ), "argument 'interaction_var_name' must be a string.")
})



test_that("validate_interaction_coefficients fails when not a string", {
  model <- get_sample_models_custom_coefficients()[["gamma"]]
  expect_error(validate_interaction_coefficients(
    model = model,
    interaction_var_name = "foo-sex"
  ), "argument 'interaction_var_name' with value foo-sex does not appear to be part of an interaction coefficient in the provided model.")
})




test_that("validate_interaction_coefficients succeeds", {
  model <- get_sample_models_custom_coefficients()[["gamma"]]
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
      coef_names = c("sl_")
    )
  )
})


test_that("get_updated_parameters with categorical interactions", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    column <- ifelse(dist_name == "vonmises", "cos_ta_", "sl_")

    coefs <- get_sample_coefs(
      dist_name = dist_name
    )
    coef_names <- get_default_coef_names(dist_name = dist_name)

    summed_coef_tibble <- get_summed_coefs_all(
      model = get_sample_models()[[dist_name]],
      coefs = coefs,
      coef_names = coef_names,
      interaction_var_name = "sex"
    )

    mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column))

    actual_updated_parameters_tibble <- get_updated_parameters(
      data = data[[column]],
      dist_name = dist_name,
      coefs_tibble = summed_coef_tibble
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/categorical/${dist_name}.rds"
    ))
    expected_updated_parameters_tibble <- readRDS(file_path)
    expect_equal(actual_updated_parameters_tibble, expected_updated_parameters_tibble)
  }
})


test_that("get_updated_parameters with continuous interactions", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    column <- ifelse(dist_name == "vonmises", "cos_ta_", "sl_")

    coefs <- get_sample_coefs(
      dist_name = dist_name,
      interaction_var_name = "elevation"
    )
    coef_names <- get_default_coef_names(dist_name = dist_name)

    quantile_coef_tibble <- get_quantile_coefs_all(
      interaction_data = data$elevation,
      coefs = coefs,
      coef_names = coef_names,
      interaction_var_name = "elevation",
      quantiles = TEST_QUANTILES
    )

    mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column))

    actual_updated_parameters_tibble <- get_updated_parameters(
      data = data[[column]],
      dist_name = dist_name,
      coefs_tibble = quantile_coef_tibble,
      grouping = "quantile"
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/continuous/${dist_name}.rds"
    ))
    expected_updated_parameters_tibble <- readRDS(file_path)
    expect_equal(actual_updated_parameters_tibble, expected_updated_parameters_tibble)
  }
})
