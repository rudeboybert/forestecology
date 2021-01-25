context("utilities")

test_that("readme code works", {
  library(dplyr)
  library(ggplot2)
  library(stringr)

  library(sf)
  library(sfheaders)
  library(blockCV)

  library(yardstick)
  library(snakecase)

  census_2_ex_no_r <- census_2_ex %>%
    filter(!str_detect(codes, "R"))

  id <- "ID"

  growth_ex <-
    compute_growth(census_1_ex, census_2_ex_no_r, id) %>%
    mutate(
      sp = to_any_case(sp),
      sp = as.factor(sp)
    )

  max_dist <- 1

  growth_ex <- growth_ex %>%
    add_buffer_variable(direction = "in", size = max_dist, region = study_region_ex)

  expect_true(check_inherits(growth_ex, "data.frame"))

  fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
  fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
  blocks <- bind_rows(
    sf_polygon(fold1),
    sf_polygon(fold2)
  ) %>%
    mutate(foldID = c(1, 2))

  cv_grid_ex <- spatialBlock(
    speciesData = growth_ex,
    verbose = FALSE,
    k = 2,
    selection = "systematic",
    blocks = blocks,
    showBlocks = FALSE
  )

  growth_ex <- growth_ex %>%
    mutate(foldID = cv_grid_ex$foldID %>% as.factor())

  cv_grid_sf_ex <- cv_grid_ex$blocks %>%
    st_as_sf()

  focal_vs_comp_ex <- growth_ex %>%
    create_focal_vs_comp(max_dist, cv_grid_sf = cv_grid_sf_ex, id = "ID")

  # Checks each column in focal_vs_comp is of appropriate type
  expect_true(check_inherits(focal_vs_comp_ex, "data.frame"))
  expect_true(
    check_focal_vs_comp(focal_vs_comp_ex) %>%
      unlist() %>%
      all()
  )

  comp_bayes_lm_ex <- focal_vs_comp_ex %>%
    comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)


  expect_true(check_comp_bayes_lm(comp_bayes_lm_ex))

  predictions <- focal_vs_comp_ex %>%
    mutate(growth_hat = predict(comp_bayes_lm_ex, focal_vs_comp_ex))

  expect_true(
    check_inherits(
      predictions,
      "data.frame"
    )
  )

  expect_true(
    check_inherits(
      comp_bayes_lm_ex %>% autoplot(type = "intercepts"),
      "ggplot"
    )
  )

  expect_true(
    check_inherits(
      focal_vs_comp_ex %>%
        run_cv(max_dist = max_dist, cv_grid = cv_grid_sf_ex) %>%
        right_join(growth_ex, by = c("focal_ID" = "ID")),
    "data.frame"
    )
  )
})
