library(hash)
library(amt)
library(here)
library(glmmTMB)
library(stringr)


create_issa_test_data <- function() {
  # Create animal ID and step ID
  n_animals <- 5
  steps_per_animal <- 10
  animal_ids <- rep(1:n_animals, each = steps_per_animal)
  step_ids <- rep(1:steps_per_animal, times = n_animals)

  # Create test data frame with various variable types
  set.seed(123)  # For reproducibility
  test_data <- data.frame(
    animal_id = factor(animal_ids),
    step_id = step_ids,
    case = rep(c(1, 0), length.out = n_animals * steps_per_animal),  # Case-control (1=used, 0=available)

    # Habitat variables
    forest_cover = factor(sample(c("High", "Medium", "Low"),
                                 n_animals * steps_per_animal, replace = TRUE)),
    habitat_type = factor(sample(c("Forest", "Grassland", "Wetland", "Urban"),
                                 n_animals * steps_per_animal, replace = TRUE)),

    # Continuous variables
    elevation = runif(n_animals * steps_per_animal, 100, 1000),
    distance_to_water = rexp(n_animals * steps_per_animal, rate = 0.01),
    slope = runif(n_animals * steps_per_animal, 0, 45),

    # Binary variables - important to have EXACTLY 0 and 1 values and nothing else
    road_present = sample(c(0, 1), n_animals * steps_per_animal, replace = TRUE),
    protected_area = sample(c(0, 1), n_animals * steps_per_animal, replace = TRUE),

    # Movement variables
    step_length = rexp(n_animals * steps_per_animal, rate = 0.1),
    log_step_length = NA,  # Will be filled
    turning_angle = runif(n_animals * steps_per_animal, -pi, pi),
    cos_ta = NA  # Will be filled
  )

  # Create log step length and cosine of turning angle
  test_data$log_step_length <- log(test_data$step_length)
  test_data$cos_ta <- cos(test_data$turning_angle)

  # Add a special binary variable with exactly 0 and 1 (and no other values)
  # This is important for testing the updated dummy check
  test_data$exact_binary <- NA
  test_data$exact_binary[seq(1, nrow(test_data), 2)] <- 0
  test_data$exact_binary[seq(2, nrow(test_data), 2)] <- 1

  return(test_data)
}



# Function to fit a mixed ISSA model using glmmTMB
fit_mixed_issa_model <- function(test_data) {
  # Fit basic ISSA model with random effects
  model <- glmmTMB(
    case ~ forest_cover + road_present +
      log_st
      (1 | animal_id),
    family = binomial(),
    data = test_data
  )

  # Add the data frame to the model object for testing
  # Since glmmTMB doesn't store the original data in $frame by default
  model$frame <- test_data

  return(model)
}
