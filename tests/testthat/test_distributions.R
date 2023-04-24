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



update_parameters <- function(args_df_row, dist, update_fn) {
  args <- c(list(dist = dist), sapply(args_df_row[2:length(args_df_row)], as.numeric))
  updated_parameters <- do.call(update_fn, args)$params
  return(updated_parameters)
}


test_that("update_parameters for all distributions", {
  # distributions <- get_supported_distributions()
  # update_fns <- list(
  #   amt::update_gamma,
  #   amt::update_exp,
  #   amt::update_hnorm,
  #   amt::update_lnorm,
  #   amt::update_vonmises
  # )
  #
  # for (i in 1:length(distributions)) {
  #   dist <- get_sample_observed_distribution(distribution = distributions[i])
  #   update_fn <- update_fns[[i]]
  #   params <- update_parameters(args_df_row, dist, update_fn)
  #   expect_equal(0, 1)
  # }
})


test_that("get_default_coefficient_names", {
  distributions <- get_supported_distributions()

  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  for (i in 1:length(distributions)) {
    default_coefs <- get_default_coefficient_names(distributions[i])
    expect_equal(default_coefs, expected_params_list[[i]])
  }
})


test_that("validate_coefficient_names fails number of args", {
  distributions <- get_supported_distributions()
  expected_number_params <- c(2, 1, 1, 2, 1)

  for (i in 1:length(distributions)) {
    expected_num <- expected_number_params[i]
    wrong_num_coefficient_names <- rep("coef_name", expected_num + 1)
    model <- NULL # don't need this for this test
    distribution <- distributions[i]

    expected_param_str <- ifelse(expected_num == 1, "parameter", "parameters")
    expected_error_msg <- str_interp(
      "distribution ${distribution} expects ${expected_num} ${expected_param_str} to be passed, not ${expected_num + 1}"
    )
    expect_error(
      validate_coefficient_names(
        model = model,
        distribution = distribution,
        coefficient_names = wrong_num_coefficient_names
      ),
      expected_error_msg
    )
  }
})


test_that("validate_coefficient_names fails unmatching coefficient names", {
  distributions <- get_supported_distributions()

  # coefficient names from MODELS
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
    distribution <- distributions[i]
    model <- MODELS[[distribution]] # from helper_test_distributions.R

    wrong_coefficient_names <- actual_params_list[[i]]
    expect_error(validate_coefficient_names(
      model = model,
      distribution = distribution,
      coefficient_names = wrong_coefficient_names
    ))
  }
})


test_that("validate_coefficient_names succeeds", {
  distributions <- get_supported_distributions()
  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  for (i in 1:length(distributions)) {
    distribution <- distributions[i]
    model <- MODELS[[distribution]] # from helper_test_distributions.R

    right_coefficient_names <- expected_params_list[[i]]

    expect_no_error(validate_coefficient_names(
      model = model,
      distribution = distribution,
      coefficient_names = right_coefficient_names
    ))
  }
})


test_that("validate_args fails non-numeric data", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  distribution <- "gamma"
  coefficient_names <- c("sl_", "log_sl_")
  error_msg <- "argument 'data' must be a vector of type numeric. Make sure you are passing either the step lengths column (e.g. sl_) or turn angles (e.g. cos_ta_)."

  error <- expect_error(validate_args(
    data = as.character(sample_data$sl_),
    model = model,
    distribution = distribution,
    coefficient_names = coefficient_names,
    reference_category = "reference_category"
  ))

  expect_equal(error$message, error_msg)
})


test_that("validate_args fails non-glmmTMB model", {
  sample_data <- get_sample_deer_data()
  distribution <- "gamma"
  coefficient_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_args(
    data = sample_data$sl_,
    model = "I am not a model",
    distribution = distribution,
    coefficient_names = coefficient_names,
    reference_category = "reference_category"
  ), "argument 'model' must be of class 'glmmTMB'")
})


test_that("validate_args fails non-string reference_category", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  distribution <- "gamma"
  coefficient_names <- c("sl_", "log_sl_")

  error <- expect_error(validate_args(
    data = sample_data$sl_,
    model = model,
    distribution = distribution,
    coefficient_names = coefficient_names,
    reference_category = 123
  ), "argument 'reference_category' must be a string")
})



test_that("validate_args succeeds with user-passed coefficient names", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  distribution <- "gamma"
  coefficient_names <- c("sl_", "log_sl_")

  expect_no_error(validate_args(
    data = sample_data$sl_,
    model = model,
    distribution = distribution,
    coefficient_names = coefficient_names,
    reference_category = "reference_category"
  ))
})


test_that("validate_args succeeds with null coefficient names", {
  sample_data <- get_sample_deer_data()
  model <- get_sample_models()[["gamma"]]
  distribution <- "gamma"

  expect_no_error(validate_args(
    data = sample_data$sl_,
    model = model,
    distribution = distribution,
    coefficient_names = NULL,
    reference_category = "reference_category"
  ))
})


test_that("get_categories_from_coefficients", {
  dists <- get_supported_distributions()
  expected_categories <- c("habitatforest", "habitatlake", "habitatmountain")

  for (i in 1:length(dists)) {

    coefs <- get_mock_coefs(dists[i])
    coef_names <- get_default_coefficient_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      interaction_coefficients <- names(coefs) %>%
        str_detect(pattern = str_interp("^${coef_name}:")) %>%
        purrr::keep(coefs, .)
      categories <- get_categories_from_coefficients(interaction_coefficients)
      expect_equal(categories, expected_categories)
    }
  }
})


test_that("get_summed_coefficients", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]
    mock_coefs <- get_mock_coefs(dist)
    coef_names <- get_default_coefficient_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_df <- data.frame(cbind(
        category = sapply(HABITATS, function(habitat) {
          habitat_string <- ifelse(
            habitat == "forest", habitat, str_interp("habitat${habitat}"))
        }),
        coefficient_name = coef_name,
        coefficient_value_sum = get_expected_coefficient_sums(distribution = dist,
                                                              coef_index = j)
      )) %>%
        arrange(coefficient_value_sum)

      summed_coefficients_df <- get_summed_coefficients(
        mock_coefs, coef_name, reference_category = REFERENCE_CATEGORY) %>%
        arrange(coefficient_value_sum)

      expect_equal(tibble::as_tibble(summed_coefficients_df), tibble::as_tibble(expected_df))
    }
  }
})


test_that("get_summed_coefficients_all", {


})


test_that("get_updated_parameters", {


})


test_that("update_distributions_by_categorical_var no interaction", {


})


test_that("update_distributions_by_categorical_var with interaction", {


})


test_that("update_distributions_by_categorical_var with custom coefficient names", {


})
