library(here)

model <- readRDS(here("R/Snapper.ssf_sum_6min_all.rds"))

model_data <- model$frame

populate_reef <- function(model_data) {
  reef <- apply(model_data, 1, function(row) {
    if (row[["reefS2"]] == "1") {
      return("S2")
    } else if (row[["reefS3"]] == "1") {
      return("S3")
    } else if (row[["reefS4"]] == "1") {
      return("S4")
    } else {
      return("S1")
    }
  })
}

new_model_data <- model_data %>%
  mutate(
    reef = as.factor(populate_reef(model_data))
  )

model$frame <- new_model_data


# steps
updated_steps_dist <- update_distributions_by_categorical_var(
  model = model,
  dist_name = "gamma",
  interaction_var_name = "reef",
  coef_names = c("sl_", "log_sl_")
)

plot_movement_distributions(updated_steps_dist)


# tas
updated_tas_dist <- update_distributions_by_categorical_var(
  model = model,
  dist_name = "vonmises",
  interaction_var_name = "reef",
  coef_names = c("cos(ta_)")
)

plot_movement_distributions(updated_tas_dist)
