library(hash)

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

  return(deer_amt_data)
}


get_sample_models <- function(data = get_sample_deer_data()) {
  models <- hash(
    "gamma" = glmmTMB(case_ ~ sl_ + log_sl_, data = data),
    "exp" = glmmTMB(case_ ~ sl_, data = data),
    "hnorm" = glmmTMB(case_ ~ sl_sq_, data = data),
    "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_, data = data),
    "vonmises" = glmmTMB(case_ ~ cos_ta_, data = data)
  )

  return(models)
}

get_supported_distributions <- function() {
  distributions <- c("gamma", "exp", "hnorm", "lnorm", "vonmises")
  return(distributions)
}


get_sample_observed_distribution <- function(distribution = "gamma") {
  data <- get_sample_deer_data()
  return(amt::fit_distr(data, distribution, na.rm = TRUE))
}


get_sample_args_df_row <- function(distribution = "gamma") {
  # args_df_row <- c("reference_category", "1.416323e-06", "-0.0009192258")
  # names(args_df_row) <- c("category", "beta_sl", "beta_log_sl")
}
