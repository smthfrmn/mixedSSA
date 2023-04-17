library(hash)
data("deer")
deer_amt <- deer |>
  steps_by_burst() |>
  random_steps() |>
  mutate(
    sl_sq_ = sl_ * sl_,
    log_sl_ = log(sl_),
    log_sl_sq_ = log_sl_ * log_sl_,
    cos_ta_ = cos(ta_)
  )


MODELS <- hash(
  "gamma" = glmmTMB(case_ ~ sl_ + log_sl_, data = deer_amt),
  "exp" = glmmTMB(case_ ~ sl_, data = deer_amt),
  "hnorm" = glmmTMB(case_ ~ sl_sq_ , data = deer_amt),
  "lnorm" = glmmTMB(case_ ~ log_sl_ + log_sl_sq_, data = deer_amt),
  "vonmises" = glmmTMB(case_ ~ cos_ta_, data = deer_amt)
)
