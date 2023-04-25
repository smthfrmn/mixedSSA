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
    "gamma" = tibble(
      category = "placeholder",
      beta_sl = 0.003,
      beta_log_sl = 0.002
    ),
    "exp" = tibble(
      category = "placeholder",
      beta_sl = 0.003
    ),
    "hnorm" = tibble(
      category = "placeholder",
      beta_sl_sq = 0.0003
    ),
    "lnorm" = tibble(
      category = "placeholder",
      beta_log_sl = 0.002,
      beta_log_sl_sq = 0.004
    ),
    "vonmises" = tibble(
      category = "placeholder",
      beta_cos_ta = 0.002
    )
  )

  expected_params <- hash(
    "gamma" = list(
      shape = 0.77641408,
      scale = -1157.7153
    ),
    "exp" = list(
      rate = -0.00022153185
    ),
    "hnorm" = list(
      sd = 0 # TODO: not working
    ),
    "lnorm" = list(
      meanlog = 5.2112948,
      sdlog = 1.56059986
    ),
    "vonmises" = list(
      kappa = 2.5005742,
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


test_that("validate_coef_names fails number of args", {
  distributions <- get_supported_distributions()
  expected_number_params <- c(2, 1, 1, 2, 1)

  for (i in 1:length(distributions)) {
    expected_num <- expected_number_params[i]
    wrong_num_coef_names <- rep("coef_name", expected_num + 1)
    model <- NULL # don't need this for this test
    dist_name <- distributions[i]

    expected_param_str <- ifelse(expected_num == 1, "parameter", "parameters")
    expected_error_msg <- str_interp(
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
      str_interp("${y}foo_")
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


test_that("validate_args fails non-numeric data", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")
  error_msg <- "argument 'data' must be a vector of type numeric. Make sure you are passing either the step lengths column (e.g. sl_) or turn angles (e.g. cos_ta_)."

  error <- expect_error(validate_args(
    data = as.character(sample_data$sl_),
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    reference_category = "reference_category"
  ))

  expect_equal(error$message, error_msg)
})


test_that("validate_args fails non-glmmTMB model", {
  sample_data <- get_sample_deer_data()
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_args(
    data = sample_data$sl_,
    model = "I am not a model",
    dist_name = dist_name,
    coef_names = coef_names,
    reference_category = "reference_category"
  ), "argument 'model' must be of class 'glmmTMB'")
})


test_that("validate_args fails non-string reference_category", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_args(
    data = sample_data$sl_,
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    reference_category = 123
  ), "argument 'reference_category' must be a string")
})



test_that("validate_args succeeds with user-passed coef names", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"
  coef_names <- c("sl_", "log_sl_")

  expect_no_error(validate_args(
    data = sample_data$sl_,
    model = model,
    dist_name = dist_name,
    coef_names = coef_names,
    reference_category = "reference_category"
  ))
})


test_that("validate_args succeeds with null coef names", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  dist_name <- "gamma"

  expect_no_error(validate_args(
    data = sample_data$sl_,
    model = model,
    dist_name = dist_name,
    coef_names = NULL,
    reference_category = "reference_category"
  ))
})


test_that("get_categories_from_coefs", {
  dists <- get_supported_distributions()
  expected_categories <- c("habitatdesert", "habitatlake", "habitatmountain")

  for (i in 1:length(dists)) {
    coefs <- get_mock_coefs(dists[i])
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      interaction_coefs <- names(coefs) %>%
        str_detect(pattern = str_interp("^${coef_name}:")) %>%
        purrr::keep(coefs, .)
      categories <- get_categories_from_coefs(interaction_coefs)
      expect_equal(categories, expected_categories)
    }
  }
})


test_that("get_summed_coefs with_interactions TRUE", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]
    mock_coefs <- get_mock_coefs(dist)
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_tibble <- tibble::tibble(
        category = unname(sapply(HABITATS, function(habitat) {
          habitat_string <- ifelse(
            habitat == "forest", habitat, str_interp("habitat${habitat}")
          )
        })),
        coef_name = coef_name,
        coef_value_sum = get_expected_coef_sums(
          distribution = dist,
          coef_index = j
        )
      ) %>%
        arrange(coef_value_sum)

      actual_tibble <- get_summed_coefs(
        mock_coefs, coef_name,
        reference_category = REFERENCE_CATEGORY
      ) %>%
        arrange(coef_value_sum)

      expect_equal(
        actual_tibble,
        expected_tibble
      )
    }
  }
})


test_that("get_summed_coefs with_interactions FALSE", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]
    mock_coefs <- get_mock_coefs(dist, with_interaction = FALSE)
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_tibble <- tibble::tibble(
        category = c("reference_category"),
        coef_name = c(coef_name),
        coef_value_sum = c(j + 1)
      )

      actual_tibble <- get_summed_coefs(
        mock_coefs, coef_name,
        reference_category = "reference_category"
      )

      expect_equal(
        actual_tibble,
        expected_tibble
      )
    }
  }
})


test_that("get_summed_coefs_all with_interaction TRUE", {
  dists <- get_supported_distributions()
  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- get_mock_coefs(dist, with_interaction = TRUE)
    coef_names <- get_default_coef_names(dists[i])
    expected_tibble <- tibble::tibble()
    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      expected_tibble <- rbind(
        expected_tibble,
        tibble::tibble(
          category = unname(sapply(HABITATS, function(habitat) {
            habitat_string <- ifelse(
              habitat == "forest", habitat, str_interp("habitat${habitat}")
            )
          })),
          coef_name = coef_name,
          coef_value_sum = get_expected_coef_sums(
            distribution = dist,
            coef_index = j
          )
        )
      ) %>%
        arrange(coef_value_sum)
    }

    actual_tibble <- get_summed_coefs_all(
      coefs = mock_coefs,
      coef_names = coef_names,
      reference_category = REFERENCE_CATEGORY
    ) %>%
      arrange(coef_value_sum)

    expect_equal(actual_tibble, expected_tibble)
  }
})


test_that("get_summed_coefs_all with_interaction FALSE", {
  dists <- get_supported_distributions()
  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- get_mock_coefs(dist, with_interaction = FALSE)
    coef_names <- get_default_coef_names(dists[i])
    expected_tibble <- tibble::tibble()
    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_tibble <- rbind(
        expected_tibble,
        tibble::tibble(
          category = c("reference_category"),
          coef_name = c(coef_name),
          coef_value_sum = c(j + 1)
        )
      ) %>%
        arrange(coef_value_sum)
    }

    actual_tibble <- get_summed_coefs_all(
      coefs = mock_coefs,
      coef_names = coef_names,
      reference_category = "reference_category"
    ) %>%
      arrange(coef_value_sum)

    expect_equal(actual_tibble, expected_tibble)
  }
})



test_that("get_updated_parameters with interactions", {
  dists <- get_supported_distributions()
  data <- get_sample_deer_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    column <- ifelse(dist_name == "vonmises", "cos_ta_", "sl_")

    coefs <- get_sample_coefs(
      dist_name = dist_name,
      with_interaction = TRUE
    )
    coef_names <- get_default_coef_names(dist_name = dist_name)

    summed_coef_tibble <- get_summed_coefs_all(
      coefs = coefs,
      coef_names = coef_names,
      reference_category = REFERENCE_CATEGORY
    )

    mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column))
    actual_updated_parameters_tibble <- get_updated_parameters(
      data = data[[column]],
      dist_name = dist_name,
      summed_coefs_tibble = summed_coef_tibble
    )

    file_path <- here(str_interp("tests/testthat/data/expected_updated_parameters_tibbles/${dist_name}.rds"))
    expected_updated_parameters_tibble <- readRDS(file_path)
    expect_equal(actual_updated_parameters_tibble, expected_updated_parameters_tibble)
  }
})


test_that("update_distributions_by_categorical_var no interaction", {


})


test_that("update_distributions_by_categorical_var with interaction", {


})


test_that("update_distributions_by_categorical_var with custom coef names", {


})
