test_that("get_categories_from_coefs", {
  dists <- get_supported_distributions()
  expected_categories <- c("desert", "lake", "mountain")

  for (i in 1:length(dists)) {
    coefs <- get_mock_coefs(dists[i])
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      interaction_coefs <- coefs[grepl(str_interp("^${coef_name}:habitat"), names(coefs))]
      categories <- get_categories_from_coefs(interaction_coefs, "habitat")
      expect_equal(categories, expected_categories)
    }
  }
})


test_that("validate_categorical_args fails non-categorical interaction var type") {

}


test_that("get_summed_coefs", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]
    mock_coefs <- get_mock_coefs(dist)
    coef_names <- get_default_coef_names(dists[i])

    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]

      expected_tibble <- tibble::tibble(
        category = HABITATS,
        coef_name = coef_name,
        coef_value = get_expected_coef_sums(
          distribution = dist,
          coef_index = j
        )
      ) %>%
        arrange(coef_value)

      actual_tibble <- get_summed_coefs(
        mock_coefs, coef_name,
        interaction_var_name = "habitat",
        reference_category = REFERENCE_CATEGORY
      ) %>%
        arrange(coef_value)

      expect_equal(
        actual_tibble,
        expected_tibble
      )
    }
  }
})


# test_that("get_summed_coefs with_interactions FALSE", {
#   dists <- get_supported_distributions()
#
#   for (i in 1:length(dists)) {
#     dist <- dists[i]
#     mock_coefs <- get_mock_coefs(dist, with_interaction = FALSE)
#     coef_names <- get_default_coef_names(dists[i])
#
#     for (j in 1:length(coef_names)) {
#       coef_name <- coef_names[j]
#
#       expected_tibble <- tibble::tibble(
#         category = c("reference_category"),
#         coef_name = c(coef_name),
#         coef_value_sum = c(j + 1)
#       )
#
#       actual_tibble <- get_summed_coefs(
#         mock_coefs, coef_name,
#         reference_category = "reference_category"
#       )
#
#       expect_equal(
#         actual_tibble,
#         expected_tibble
#       )
#     }
#   }
# })


test_that("get_summed_coefs_all", {
  dists <- get_supported_distributions()
  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- get_mock_coefs(dist, with_interaction = TRUE)
    coef_names <- get_default_coef_names(dists[i])
    expected_tibble <- tibble::tibble()
    for (j in 1:length(coef_names)) {
      coef_name <- coef_names[j]
      expected_tibble <- rbind(
        expected_tibble,
        tibble::tibble(
          category = HABITATS,
          coef_name = coef_name,
          coef_value = get_expected_coef_sums(
            distribution = dist,
            coef_index = j
          )
        )
      ) %>%
        arrange(coef_value)
    }

    actual_tibble <- get_summed_coefs_all(
      coefs = mock_coefs,
      coef_names = coef_names,
      interaction_var_name = "habitat",
      reference_category = REFERENCE_CATEGORY
    ) %>%
      arrange(coef_value)

    expect_equal(actual_tibble, expected_tibble)
  }
})


# test_that("get_summed_coefs_all with_interaction FALSE", {
#   dists <- get_supported_distributions()
#   for (i in 1:length(dists)) {
#     dist <- dists[i]
#
#     mock_coefs <- get_mock_coefs(dist, with_interaction = FALSE)
#     coef_names <- get_default_coef_names(dists[i])
#     expected_tibble <- tibble::tibble()
#     for (j in 1:length(coef_names)) {
#       coef_name <- coef_names[j]
#
#       expected_tibble <- rbind(
#         expected_tibble,
#         tibble::tibble(
#           category = c("reference_category"),
#           coef_name = c(coef_name),
#           coef_value_sum = c(j + 1)
#         )
#       ) %>%
#         arrange(coef_value_sum)
#     }
#
#     actual_tibble <- get_summed_coefs_all(
#       coefs = mock_coefs,
#       coef_names = coef_names,
#       reference_category = "reference_category"
#     ) %>%
#       arrange(coef_value_sum)
#
#     expect_equal(actual_tibble, expected_tibble)
#   }
# })



# test_that("update_distributions_by_categorical_var no interaction and default coef names", {
#   dists <- get_supported_distributions()
#   data <- get_sample_deer_data()
#
#   for (i in 1:length(dists)) {
#     dist_name <- dists[i]
#     model <- get_sample_models(
#       data = data,
#       with_interaction = FALSE
#     )[[dist_name]]
#
#     if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
#       column_data <- data$cos_ta_
#     } else {
#       column_data <- data$sl_
#     }
#
#     mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column))
#
#     results <- update_distributions_by_categorical_var(
#       data = column_data,
#       model = model,
#       dist_name = dist_name,
#       reference_category = "updated"
#     )
#
#
#     file_path <- here(str_interp(
#       "${get_data_path_root()}/helper_data/expected/updated_params_no_interactions/${dist_name}.rds"
#     ))
#
#     expected_results <- readRDS(file_path)
#     expect_equal(results, expected_results)
#   }
# })


# test_that("update_distributions_by_categorical_var with default reference category and default coef names", {
#   dists <- get_supported_distributions()
#   data <- get_sample_deer_data()
#
#   for (i in 1:length(dists)) {
#     dist_name <- dists[i]
#     model <- get_sample_models(
#       data = data,
#       with_interaction = FALSE
#     )[[dist_name]]
#
#     if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
#       column_data <- data$cos_ta_
#     } else {
#       column_data <- data$sl_
#     }
#
#     mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column))
#
#     results <- update_distributions_by_categorical_var(
#       data = column_data,
#       model = model,
#       dist_name = dist_name
#     )
#
#     file_path <- here(str_interp(
#       "${get_data_path_root()}/helper_data/expected/updated_params_no_interactions/${dist_name}.rds"
#     ))
#
#     expected_results <- readRDS(file_path)
#
#     expected_results$category <- c("observed", "reference_category")
#     expect_equal(results, expected_results)
#   }
# })


test_that("update_distributions_by_categorical_var with interaction and default coef names", {
  dists <- get_supported_distributions()
  data <- get_sample_deer_data()

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models(
      data = data,
      with_interaction = TRUE
    )[[dist_name]]

    if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
      column_data <- data$cos_ta_
      column_name <- "cos_ta_"
    } else {
      column_data <- data$sl_
      column_name <- "sl_"
    }


    mockr::local_mock(
      fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column_name)
    )

    results <- update_distributions_by_categorical_var(
      data = column_data,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "habitat",
      reference_category = REFERENCE_CATEGORY
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/updated_params_interactions/${dist_name}.rds"
    ))
    expected_results <- readRDS(file_path)

    expect_equal(results, expected_results)
  }
})


test_that("update_distributions_by_categorical_var with custom coef names", {
  dists <- get_supported_distributions()
  data <- get_sample_deer_data(custom_coefs = TRUE)

  for (i in 1:length(dists)) {
    dist_name <- dists[i]
    model <- get_sample_models_custom_coefficients(
      data = data,
      with_interaction = TRUE
    )[[dist_name]]

    if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
      column_data <- data$turn_angle_cos
      column_name <- "turn_angle_cos"
    } else {
      column_data <- data$step_length
      column_name <- "step_length"
    }

    mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column_name))

    results <- update_distributions_by_categorical_var(
      data = column_data,
      model = model,
      dist_name = dist_name,
      interaction_var_name = "habitat",
      coef_names = get_sample_coef_names_by_dist(
        dist_name = dist_name,
        custom_coefs = TRUE
      ),
      reference_category = REFERENCE_CATEGORY
    )

    file_path <- here(str_interp(
      "${get_data_path_root()}/expected/updated_params_interactions/${dist_name}.rds"
    ))
    expected_results <- readRDS(file_path)
    expect_equal(results, expected_results)
  }
})


# test_that("update_distributions_by_categorical_var with no interaction and custom coef names", {
#   dists <- get_supported_distributions()
#   data <- get_sample_deer_data(custom_coefs = TRUE)
#
#   for (i in 1:length(dists)) {
#     dist_name <- dists[i]
#     model <- get_sample_models_custom_coefficients(
#       data = data,
#       with_interaction = FALSE
#     )[[dist_name]]
#
#     if (dist_name %in% TURN_ANGLE_DISTRIBUTIONS) {
#       column_data <- data$turn_angle_cos
#       column_name <- "turn_angle_cos"
#     } else {
#       column_data <- data$step_length
#       column_name <- "step_length"
#     }
#
#     mockr::local_mock(fit_distribution = function(data, dist_name, na_rm) get_sample_observed_distribution(dist_name = dist_name, column = column))
#
#     results <- update_distributions_by_categorical_var(
#       data = column_data,
#       model = model,
#       dist_name = dist_name,
#       coef_names = get_sample_coef_names_by_dist(
#         dist_name = dist_name,
#         custom_coefs = TRUE
#       ),
#       reference_category = "updated"
#     )
#
#     file_path <- here(str_interp(
#       "${get_data_path_root()}/helper_data/expected/updated_params_no_interactions/${dist_name}.rds"
#     ))
#     expected_results <- readRDS(file_path)
#     expect_equal(results, expected_results)
#   }
# })
