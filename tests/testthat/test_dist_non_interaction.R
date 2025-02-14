test_that("get_no_interaction_coefs", {
  dists <- get_supported_distributions()
  data <- get_sample_fisher_data()

  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- as.data.frame(t(unlist(get_mock_coefs(
      dist_name = dist,
      interaction_var_name = NULL
    ))))

    coef_names <- get_default_coef_names(dists[i])

    expected_tibble <- tibble::tibble(
      grouping = NA,
      random_effect = NA,
      coef_name = coef_names,
      coef_value = unlist(
        mock_coefs[1, 2:length(mock_coefs)],
        use.names = F
      )
    )

    actual_tibble <- get_no_interaction_coefs(
      coefs = mock_coefs,
      coef_name = coef_names,
      random_effects_var_name = NULL
    )

    expect_equal(
      actual_tibble,
      expected_tibble
    )
  }
})


test_that("get_no_interaction_coefs with random_effects", {
  dists <- get_supported_distributions()

  for (i in 1:length(dists)) {
    dist <- dists[i]

    mock_coefs <- get_simple_mock_mixed_coefs(dist)
    coef_names <- get_default_coef_names(dists[i])

    expected_tibble <- tibble::tibble(
      grouping = NA,
      random_effect = rep(c("typical", "F1", "F2", "M1", "M4"), length(coef_names)),
      coef_name = rep(coef_names, each = nrow(mock_coefs)),
      coef_value = rep(1:nrow(mock_coefs), length(coef_names)),
    ) |>
      arrange(grouping, random_effect)

    actual_tibble <- get_no_interaction_coefs(
      coefs = mock_coefs,
      coef_name = coef_names,
      random_effects_var_name = "id"
    )

    expect_equal(
      actual_tibble,
      expected_tibble
    )
  }
})
