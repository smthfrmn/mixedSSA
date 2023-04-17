test_that("get_update_distribution_function_and_args returns correct values", {
  distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")
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
  for(i in 1:length(distributions)) {
    fn_and_args <- get_update_distribution_function_and_args(distributions[i])
    expect_equal(fn_and_args$fn, expected_fns[[i]])
    expect_equal(fn_and_args$args, expected_args[[i]])
  }
})


test_that("get_updated_parameters", {

})


test_that("get_default_coefficient_names", {

})


test_that("validate_coefficient_names fails number of args", {
  distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")
  expected_number_params <- c(2, 1, 1, 2, 1)
  for (i in 1:length(distributions)) {
    expected_num <- expected_number_params[i]
    wrong_num_coefficient_names <- rep("coef_name", expected_num + 1)
    model <- NULL  # don't need this for this test
    distribution <- distributions[i]

    expected_param_str <- ifelse(expected_num == 1, "parameter", "parameters")
    expected_error_msg <- str_interp(
      "distribution ${distribution} expects ${expected_num} ${expected_param_str} to be passed, not ${expected_num + 1}"
    )
    expect_error(validate_coefficient_names(model = model,
                               distribution = distribution,
                               coefficient_names = wrong_num_coefficient_names),
                 expected_error_msg)
  }
})


test_that("validate_coefficient_names fails unmatching coefficient names", {

  distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")

  # coefficient names from MODELS
  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  actual_params_list <- sapply(expected_params_list, function(x) {
    return(unname(sapply(x, function(y) {str_interp("${y}foo_")})))
  })

  for (i in 1:length(distributions)) {
    distribution <- distributions[i]
    model <- MODELS[[distribution]]  # from helper_test_distributions.R

    wrong_coefficient_names <- actual_params_list[[i]]
    expect_error(validate_coefficient_names(model = model,
                                            distribution = distribution,
                                            coefficient_names = wrong_coefficient_names))
  }
})


test_that("validate_coefficient_names succeeds", {
  distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")
  expected_params_list <- list(
    c("sl_", "log_sl_"),
    c("sl_"),
    c("sl_sq_"),
    c("log_sl_", "log_sl_sq_"),
    c("cos_ta_")
  )

  for (i in 1:length(distributions)) {
    distribution <- distributions[i]
    model <- MODELS[[distribution]]  # from helper_test_distributions.R

    right_coefficient_names <- expected_params_list[[i]]

    expect_no_error(validate_coefficient_names(model = model,
                                            distribution = distribution,
                                            coefficient_names = right_coefficient_names))
  }
})


test_that("validate_args", {

})


test_that("update_distributions_by_categorical_var", {

})
