#' Fisher Movement and Covariate Data
#'
#' A data set containing fisher movement and covariate data.
#' Data set was generated from amt::amt_fisher using the following code
#'
#'
#' data("amt_fisher")

#' track <- amt_fisher %>%  make_track(x_, y_, t_ = t_, id = id, sex = sex, name = name) %>%
#'  nest(data = !any_of(c("id", "sex", "name"))) %>%
#'  mutate(
#'    steps = map(data, function(x) {
#'      steps_final <- x %>%
#'        track_resample(rate = minutes(30), tolerance = minutes(5)) %>%
#'        steps_by_burst()
#'      return(steps_final)
#'    })
#'  ) %>%
#'  unnest(cols = steps) %>%
#'  select(-data)
#'
#'
#'m1 <- track %>%
#'  filter(id == "M1") %>%
#'  amt::random_steps()
#'
#' m4 <- track %>%
#'  filter(id == "M4") %>%
#'  amt::random_steps()
#'
#'f2 <- track %>%
#'  filter(id == "F2") %>%
#'  amt::random_steps()
#'
#'f1 <- track %>%
#'  filter(id == "F1") %>%
#'  amt::random_steps()
#'
#'
#'
#'
#'data <- rbind(
#'  m1, m4, f2, f1
#') %>%
#'  within(sex <- relevel(as.factor(sex), ref = REFERENCE_CATEGORY)) %>%
#'  mutate(
#'    sl_sq_ = sl_ * sl_,
#'    log_sl_ = log(sl_),
#'    log_sl_sq_ = log_sl_ * log_sl_,
#'    cos_ta_ = cos(ta_),
#'    sex_three_factors = factor(ifelse(id == "F2", as.character("UNK"), as.character(sex)))
#'  ) %>%
#'  extract_covariates(terra::unwrap(amt_fisher_covar$elevation)) |>
#'  extract_covariates(terra::unwrap(amt_fisher_covar$elevation), where = "both") |>
#'  mutate(
#'    elevation_fact = case_when(
#'      elevation < 45 + 35 ~ "low",
#'      elevation < 45 + 70 ~ "medium",
#'      elevation < 45 + 100 ~ "high"
#'    ) |> as.factor(),
#'    elevation_fact_start = case_when(
#'      elevation_start < 45 + 35 ~ "low",
#'      elevation_start < 45 + 70 ~ "medium",
#'      elevation_start < 45 + 100 ~ "high"
#'    ) |> as.factor(),
#'    elevation_fact_end = case_when(
#'      elevation_start < 45 + 35 ~ "low",
#'      elevation_start < 45 + 70 ~ "medium",
#'      elevation_start < 45 + 100 ~ "high"
#'    ) |> as.factor()
#'  )
#'
#' @format ## `mixedssa_fisher_data`
#' A data frame with 18,095 rows and 21 columns:
#' \describe{
#'   \item{id}{Fisher ID}
#'   \item{sex}{Fisher Sex}
#'   \item{name}{Fisher Name}
#'   \item{burst_}{Burst ID}
#'   \item{x1_}{Latitude of start of step}
#'   \item{x2_}{Latitude of end of step}
#'   \item{y1_}{Longitude of start of step}
#'   \item{y2_}{Longitude of end of step}
#'   \item{sl_}{Step length}
#'   \item{ta_}{Turn angle}
#'   \item{case_}{Whether or not the row is observed or generated}
#'   \item{step_id_}{Step ID}
#'   \item{sl_sq_}{Step length squared}
#'   \item{log_sl_}{Log of step length}
#'   \item{log_sl_sq_}{Log of step length squared}
#'   \item{cos_ta_}{Cosine of turn angle}
#'   \item{elevation}{Elevation end of step (legacy column)}
#'   \item{elevation_start}{Elevation start of step}
#'   \item{elevation_end}{Elevation end of step (same as elevation)}
#'   \item{elevation_fact}{Elevation factor end of step (i.e., low, medium, high; legacy column)}
#'   \item{elevation_fact_start}{Elevation factor start of step}
#'   \item{elevation_fact_end}{Elevation factor end of step (same as elevation_fact)}
#'
#' }
#' @source <https://datarepository.movebank.org/entities/datapackage/8d40ab81-1f34-4280-b6be-91e6b9fc3fcd>
"mixedssa_fisher_data"
