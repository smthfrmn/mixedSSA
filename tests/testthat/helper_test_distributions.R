library(hash)
library(amt)
library(here)
library(glmmTMB)
library(stringr)

REFERENCE_CATEGORY <- "M"
SEXES <- c(REFERENCE_CATEGORY, "F")
TEST_QUANTILES <- c(0.25, 0.5, 0.75)

# A hack to make local tests and github actions work :(
get_data_path_root <- function() {
  # current_path <- testthat::test_path()
  # if (current_path == ".") {
  #   current_path <- "tests/testthat"
  # }
  #
  # return(stringr::str_interp("${current_path}/helper_data/dist"))
  root_path <- R.utils::getAbsolutePath("tests/testthat/helper_data/dist")

  # hack
  root_path <- gsub("tests/testthat/tests/testthat", "tests/testthat", root_path)
  return(root_path)
}

get_sample_fisher_data <- function(custom_coefs = FALSE) {
  # data("amt_fisher")
  #
  # track <- amt_fisher %>%
  #   make_track(x_, y_, t_ = t_, id = id, sex = sex, name = name) %>%
  #   nest(data = !any_of(c("id", "sex", "name"))) %>%
  #   mutate(
  #     steps = map(data, function(x) {
  #       steps_final <- x %>%
  #         track_resample(rate = minutes(30), tolerance = minutes(5)) %>%
  #         steps_by_burst()
  #       return(steps_final)
  #     })
  #   ) %>%
  #   unnest(cols = steps) %>%
  #   select(-data)
  #
  #
  # m1 <- track %>%
  #   filter(id == "M1") %>%
  #   amt::random_steps()
  #
  # m4 <- track %>%
  #   filter(id == "M4") %>%
  #   amt::random_steps()
  #
  # f2 <- track %>%
  #   filter(id == "F2") %>%
  #   amt::random_steps()
  #
  # f1 <- track %>%
  #   filter(id == "F1") %>%
  #   amt::random_steps()
  #
  # data <- rbind(
  #     m1, m4, f2, f1
  #   ) %>%
  #   within(sex <- relevel(as.factor(sex), ref = REFERENCE_CATEGORY)) %>%
  #   mutate(
  #     sl_sq_ = sl_ * sl_,
  #     log_sl_ = log(sl_),
  #     log_sl_sq_ = log_sl_ * log_sl_,
  #     cos_ta_ = cos(ta_)
  #   ) %>%
  #   extract_covariates(terra::unwrap(amt_fisher_covar$elevation))

  file_path <- str_interp("${get_data_path_root()}/fisher_data.rds")
  data <- readRDS(file_path)
  if (custom_coefs) {
    # If the user isn't using the amt nomenclature
    data <- data %>%
      rename(
        step_length = sl_,
        turn_angle = ta_,
        step_length_sq = sl_sq_,
        step_length_log = log_sl_,
        step_length_log_sq = log_sl_sq_,
        turn_angle_cos = cos_ta_
      )
  }

  return(data)
}


get_fisher_data_quantiles <- function() {
  data <- get_sample_fisher_data()
  return(quantile(data$elevation, probs = TEST_QUANTILES, na.rm = TRUE))
}


get_sample_simple_models <- function(data = get_sample_fisher_data()) {
  # models <- hash(
  #   "gamma" = glmmTMB(case_ ~ sl_ + log_sl_, data = data),
  #   "exp" = glmmTMB(case_ ~ sl_, data = data),
  #   "hnorm" = glmmTMB(case_ ~ sl_sq_, data = data),
  #   "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_, data = data),
  #   "vonmises" = glmmTMB(case_ ~ cos_ta_, data = data),
  #   "unif" = glmmTMB(case_ ~ cos_ta_, data = data)
  # )

  file_path <- str_interp("${get_data_path_root()}/models/simple_models.rds")
  # saveRDS(models, file = file_path)
  models <- readRDS(file_path)

  return(models)
}


get_sample_simple_mixed_models <- function(data = get_sample_fisher_data()) {
  # models <- hash(
  #   "gamma" = glmmTMB(case_ ~ sl_ + log_sl_ + (0 + sl_ + log_sl_ | id), data = data),
  #   "exp" = glmmTMB(case_ ~ sl_ + (0 + sl_ | id), data = data),
  #   "hnorm" = glmmTMB(case_ ~ sl_sq_ + (0 + sl_sq_ | id), data = data),
  #   "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_ + (0 + log_sl_ + log_sl_sq_ | id), data = data),
  #   "vonmises" = glmmTMB(case_ ~ cos_ta_ + (0 + cos_ta_ | id), data = data),
  #   "unif" = glmmTMB(case_ ~ cos_ta_ + (0 + cos_ta_ | id), data = data)
  # )

  file_path <- str_interp("${get_data_path_root()}/models/mixed/simple_models.rds")
  # saveRDS(models, file = file_path)
  models <- readRDS(file_path)

  return(models)
}


get_sample_models <- function(data = get_sample_fisher_data(), interaction_var_name = "sex") {
  # suppress model convergence warnings :/
  suppressWarnings({
    if (interaction_var_name == "sex") {
      # models <- hash(
      #   "gamma" = glmmTMB(case_ ~ sl_ + log_sl_ + sl_:sex + log_sl_:sex, data = data),
      #   "exp" = glmmTMB(case_ ~ sl_ + sl_:sex, data = data),
      #   "hnorm" = glmmTMB(case_ ~ sl_sq_ + sl_sq_:sex, data = data),
      #   "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_ + log_sl_:sex + log_sl_sq_:sex, data = data),
      #   "vonmises" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:sex, data = data),
      #   "unif" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:sex, data = data)
      # )
      #
      file_path <- str_interp("${get_data_path_root()}/models/sex_models.rds")
      # saveRDS(models, file_path)
    }

    if (interaction_var_name == "elevation") {
      # models <- hash(
      #   "gamma" = glmmTMB(case_ ~ sl_ + log_sl_ + sl_:elevation + log_sl_:elevation, data = data),
      #   "exp" = glmmTMB(case_ ~ sl_ + sl_:elevation, data = data),
      #   "hnorm" = glmmTMB(case_ ~ sl_sq_ + sl_sq_:elevation, data = data), # convergence issues
      #   "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_ + log_sl_:elevation + log_sl_sq_:elevation, data = data),
      #   "vonmises" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:elevation, data = data),
      #   "unif" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:elevation, data = data)
      # )
      file_path <- str_interp("${get_data_path_root()}/models/elevation_models.rds")
      # saveRDS(models, file_path)
    }
  })

  models <- readRDS(file_path)
  return(models)
}


get_sample_mixed_models <- function(data = get_sample_fisher_data(), interaction_var_name = "sex") {
  # suppress model convergence warnings :/
  suppressWarnings({
    if (interaction_var_name == "sex") {
      # models <- hash(
      #   "gamma" = glmmTMB(case_ ~ sl_ + log_sl_ + sl_:sex + log_sl_:sex + (0 + sl_ + log_sl_ | id), data = data),
      #   "exp" = glmmTMB(case_ ~ sl_ + sl_:sex + (0 + sl_ | id), data = data),
      #   "hnorm" = glmmTMB(case_ ~ sl_sq_ + sl_sq_:sex + (0 + sl_sq_ | id), data = data),
      #   "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_ + log_sl_:sex + log_sl_sq_:sex + (0 + log_sl_ + log_sl_sq_ | id), data = data),
      #   "vonmises" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:sex + (0 + cos_ta_ | id), data = data),
      #   "unif" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:sex + (0 + cos_ta_ | id), data = data)
      # )
      file_path <- str_interp("${get_data_path_root()}/models/mixed/sex_models.rds")
      # saveRDS(models, file_path)
    }

    if (interaction_var_name == "elevation") {
      # models <- hash(
      #   "gamma" = glmmTMB(case_ ~ sl_ + log_sl_ + sl_:elevation + log_sl_:elevation + (0 + sl_ + log_sl_ | id), data = data),
      #   "exp" = glmmTMB(case_ ~ sl_ + sl_:elevation + (0 + sl_ | id), data = data),
      #   "hnorm" = glmmTMB(case_ ~ sl_sq_ + sl_sq_:elevation + (0 + sl_sq_ | id), data = data), # convergence issues
      #   "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_ + log_sl_:elevation + log_sl_sq_:elevation + (0 + log_sl_ + log_sl_sq_ | id), data = data),
      #   "vonmises" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:elevation + (0 + cos_ta_ | id), data = data),
      #   "unif" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:elevation + (0 + cos_ta_ | id), data = data)
      # )
      file_path <- str_interp("${get_data_path_root()}/models/mixed/elevation_models.rds")
      # saveRDS(models, file_path)
    }
  })

  models <- readRDS(file_path)
  return(models)
}


get_sample_coefs <- function(dist_name, data = get_sample_fisher_data(),
                             interaction_var_name = "sex", custom_coefs = FALSE) {
  if (!custom_coefs) {
    model <- get_sample_models(interaction_var_name = interaction_var_name)[[dist_name]]
  } else {
    model <- get_sample_models_custom_coefficients(interaction_var_name = interaction_var_name)[[dist_name]]
  }
  return(as.data.frame(t(unlist(fixef(model)$cond))))
}


get_supported_distributions <- function() {
  return(SUPPORTED_DISTRIBUTIONS)
}


get_default_coef_names <- function(dist_name) {
  update_fn_args <- get_update_distribution_function_and_args(dist_name)$args
  variable_args <- update_fn_args[2:length(update_fn_args)]
  return(sapply(variable_args, function(variable_arg) {
    variable <- stringr::str_interp(
      '${stringr::str_extract(variable_arg, stringr::regex("(?<=beta_).*"))}_'
    )
    return(variable)
  }, USE.NAMES = FALSE))
}


get_sample_coef_names_by_dist <- function(dist_name, custom_coefs = FALSE) {
  if (!custom_coefs) {
    coef_names <- hash(
      "gamma" = c("sl_", "log_sl_"),
      "exp" = c("sl_"),
      "hnorm" = c("sl_sq_"),
      "lnorm" = c("log_sl_", "log_sl_sq_"),
      "vonmises" = c("cos_ta_"),
      "unif" = c("cos_ta_")
    )[[dist_name]]
  } else {
    coef_names <- hash(
      "gamma" = c("step_length", "step_length_log"),
      "exp" = c("step_length"),
      "hnorm" = c("step_length_sq"),
      "lnorm" = c("step_length_log", "step_length_log_sq"),
      "vonmises" = c("turn_angle_cos"),
      "unif" = c("turn_angle_cos")
    )[[dist_name]]
  }

  return(coef_names)
}


get_cached_distribution <- function(dist_name) {
  file_path <- here::here(stringr::str_interp("${get_data_path_root()}/fitted/${dist_name}.rds"))
  if (file.exists(file_path)) {
    return(readRDS(file_path))
  }
  return(NULL)
}


get_sample_tentative_distribution <- function(dist_name = "gamma", column = "sl_") {
  distribution <- get_cached_distribution(dist_name)
  if (is.null(distribution)) {
    data <- get_sample_fisher_data() %>%
      dplyr::filter(case_ == TRUE) %>%
      dplyr::pull(column)
    distribution <- amt::fit_distr(data, dist_name = dist_name, na.rm = TRUE)
    file_path <- here(str_interp("${get_data_path_root()}/fitted/${dist_name}.rds"))
    saveRDS(distribution, file = file_path)
  }

  return(distribution)
}


get_mock_coefs <- function(dist_name, interaction_var_name = "sex") {
  if(is.null(interaction_var_name)) {
    model <- get_sample_simple_models()[[dist_name]]
  } else {
    model <- get_sample_models(
      interaction_var_name = interaction_var_name)[[dist_name]]
  }

  coefs <- glmmTMB::fixef(model)$cond
  mock_coefs <- coefs * 0 + c(1:length(coefs))
  mock_coefs <- as.data.frame(t(unlist(mock_coefs)))
  return(mock_coefs)
}


get_simple_mock_mixed_coefs <- function(dist_name) {
  model <- get_sample_simple_mixed_models()[[dist_name]]
  coefs <- get_coefs_from_model(model, "id")

  mock_coefs <- coefs * 0 + c(1:nrow(coefs))
  return(mock_coefs)
}


get_expected_coef_sums <- function(distribution, coef_index, quantiles = NULL) {
  expected_coef_sums <- hash(
    "gamma" = hash(
      "1" = c(2, 6), # sl_
      "2" = c(3, 8) # log_sl_
    ),
    "exp" = hash(
      "1" = c(2, 5), # sl_
      "2" = NULL # hack for hash indexing :/
    ),
    "hnorm" = hash(
      "1" = c(2, 5), # sl_sq_
      "2" = NULL
    ),
    "lnorm" = hash(
      "1" = c(2, 6), # log_sl_
      "2" = c(3, 8) # log_sl_sq_
    ),
    "vonmises" = hash(
      "1" = c(2, 5), # cos_ta_
      "2" = NULL
    ),
    # TODO
    "unif" = hash(
      "1" = c(2, 5),
      "2" = NULL
    )
  )

  expected_values <- as.vector(expected_coef_sums[[distribution]][[as.character(coef_index)]])

  if (!is.null(quantiles) & !is.null(expected_values)) {
    quantile_data <- get_fisher_data_quantiles()
    interaction_coef_value <- expected_values[2] - expected_values[1]
    expected_values <- expected_values[1] + (quantile_data * interaction_coef_value)
  }

  names(expected_values) <- NULL

  return(expected_values)
}
