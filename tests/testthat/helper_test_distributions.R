library(hash)
library(amt)


REFERENCE_CATEGORY <- "forest"
HABITATS <- c(REFERENCE_CATEGORY, "desert", "lake", "mountain")

get_sample_deer_data <- function() {
  data("deer")
  deer_amt_data <- deer |>
    steps_by_burst() |>
    random_steps() |>
    mutate(
      sl_sq_ = sl_ * sl_,
      log_sl_ = log(sl_),
      log_sl_sq_ = log_sl_ * log_sl_,
      cos_ta_ = cos(ta_)
    )

  return(set_sample_habitat(deer_amt_data))
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

get_supported_distributions <- function() {
  distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")
  return(distributions)
}

get_default_coefficient_names_by_dist <- function(distribution) {
  return(hash(
    "gamma" = c("sl_", "log_sl_"),
    "exp" = c("sl_"),
    "hnorm" = c("sl_sq_"),
    "lnorm" = c("log_sl_", "log_sl_sq_"),
    "cos_ta_" = c("cos_ta_")
  )[[distribution]])
}


get_sample_observed_distribution <- function(distribution = "gamma") {
  data <- get_sample_deer_data()
  return(amt::fit_distr(data, distribution, na.rm = TRUE))
}


get_sample_args_df_row <- function(distribution = "gamma") {
  # args_df_row <- c("reference_category", "1.416323e-06", "-0.0009192258")
  # names(args_df_row) <- c("category", "beta_sl", "beta_log_sl")
}


get_mock_coefs <- function(distribution, with_interaction = TRUE) {
  model <- get_sample_models(with_interaction = with_interaction)[[distribution]]
  coefs <- glmmTMB::fixef(model)$cond
  mock_coefs <- coefs * 0 + c(1:length(coefs))
  return(mock_coefs)
}


get_expected_coefficient_sums <- function(distribution, coef_index){
  expected_coefficient_sums = hash(
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

  return(as.vector(expected_coefficient_sums[[distribution]][[as.character(coef_index)]]))
}
