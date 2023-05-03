library(hash)
library(amt)
library(here)

REFERENCE_CATEGORY <- "forest"
HABITATS <- c(REFERENCE_CATEGORY, "desert", "lake", "mountain")

get_sample_deer_data <- function() {
  # data("deer")
  # deer_amt_data <- deer |>
  #   steps_by_burst() |>
  #   random_steps() |>
  #   mutate(
  #     sl_sq_ = sl_ * sl_,
  #     log_sl_ = log(sl_),
  #     log_sl_sq_ = log_sl_ * log_sl_,
  #     cos_ta_ = cos(ta_)
  #   )
  # deer_data <- set_sample_habitat(deer_amt_data)
  file_path = here(str_interp("${testthat::test_path()}/helper_data/deer_data.rds"))
  return(readRDS(file_path))
}


set_sample_habitat <- function(data, reference_category = REFERENCE_CATEGORY){
  nrows <- nrow(data)
  quarter <- round(nrows/4)
  habitat <- c(rep("forest", quarter), rep("desert", quarter), rep("lake", quarter), rep("mountain", quarter), "mountain")
  data$habitat <- habitat

  data <- within(data, habitat <- relevel(as.factor(habitat), ref = reference_category))
  return(data)
}


get_sample_models <- function(data = get_sample_deer_data(), with_interaction = FALSE) {
  if(!with_interaction) {
    models <- hash(
      "gamma" = glmmTMB(case_ ~ sl_ + log_sl_, data = data),
      "exp" = glmmTMB(case_ ~ sl_, data = data),
      "hnorm" = glmmTMB(case_ ~ sl_sq_, data = data),
      "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_, data = data),
      "vonmises" = glmmTMB(case_ ~ cos_ta_, data = data)
    )
  } else {
    models <- hash(
      "gamma" = glmmTMB(case_ ~ sl_ + log_sl_ + sl_:habitat + log_sl_:habitat, data = data),
      "exp" = glmmTMB(case_ ~ sl_ + sl_:habitat, data = data),
      "hnorm" = glmmTMB(case_ ~ sl_sq_ + sl_sq_:habitat, data = data),
      "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_ + log_sl_:habitat + log_sl_sq_:habitat, data = data),
      "vonmises" = glmmTMB(case_ ~ cos_ta_ + cos_ta_:habitat, data = data)
    )
  }

  return(models)
}


get_sample_coefs <- function(dist_name, data = get_sample_deer_data(),
                             with_interaction = FALSE) {
  model <- get_sample_models(with_interaction = with_interaction)[[dist_name]]
  return(fixef(model)$cond)
}


get_supported_distributions <- function() {
  return(SUPPORTED_DISTRIBUTIONS)
}


get_default_coef_names_by_dist <- function(distribution) {
  return(hash(
    "gamma" = c("sl_", "log_sl_"),
    "exp" = c("sl_"),
    "hnorm" = c("sl_sq_"),
    "lnorm" = c("log_sl_", "log_sl_sq_"),
    "cos_ta_" = c("cos_ta_")
  )[[distribution]])
}


get_cached_distribution <- function(dist_name) {
  file_path <- here(str_interp("${testthat::test_path()}/helper_data/distributions/${dist_name}.rds"))
  if(file.exists(file_path)) {
    return(readRDS(file_path))
  }
  return(NULL)
}

get_sample_observed_distribution <- function(dist_name = "gamma", column = "sl_") {
  distribution <- get_cached_distribution(dist_name)
  if(is.null(distribution)) {
    print("NOT GETTING CACHED")
    data <- get_sample_deer_data()[[column]]
    distribution <- amt::fit_distr(data, dist_name = dist_name, na.rm = TRUE)
    file_path <- here(str_interp("${testthat::test_path()}/helper_data/distributions/${dist_name}.rds"))
    saveRDS(distribution, file = file_path)
  }

  return(distribution)
}


get_mock_coefs <- function(distribution, with_interaction = TRUE) {
  model <- get_sample_models(with_interaction = with_interaction)[[distribution]]
  coefs <- glmmTMB::fixef(model)$cond
  mock_coefs <- coefs * 0 + c(1:length(coefs))
  return(mock_coefs)
}


get_expected_coef_sums <- function(distribution, coef_index){
  expected_coef_sums = hash(
    "gamma" = hash(
      "1" = c(2, 6, 7, 8),  # sl_
      "2" = c(3, 10, 11, 12)  # log_sl_
    ),
    "exp" = hash(
      "1" = c(2, 5, 6, 7), # sl_
      "2" = NULL   # hack for hash indexing :/
    ),
    "hnorm" = hash(
      "1" = c(2, 5, 6, 7),  # sl_sq_
      "2" = NULL
    ),
    "lnorm" = hash(
      "1" = c(2, 6, 7, 8),  # log_sl_
      "2" = c(3, 10, 11, 12)  # log_sl_sq_
    ),
    "vonmises" = hash(
      "1" = c(2, 5, 6, 7),  # cos_ta_
      "2" = NULL
    )
  )

  return(as.vector(expected_coef_sums[[distribution]][[as.character(coef_index)]]))
}
