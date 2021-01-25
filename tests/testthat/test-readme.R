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

  census_df2_ex_no_r <- census_df2_ex %>%
    filter(!str_detect(codes, "R"))

  id <- "ID"

  ex_growth_df <-
    compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
    mutate(
      sp = to_any_case(sp),
      sp = as.factor(sp)
    )

  max_dist <- 1

  ex_growth_df <- ex_growth_df %>%
    add_buffer_variable(direction = "in", size = max_dist, region = ex_study_region)

  expect_true(check_inherits(ex_growth_df, "data.frame"))

  fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
  fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
  blocks <- bind_rows(
    sf_polygon(fold1),
    sf_polygon(fold2)
  ) %>%
    mutate(foldID = c(1, 2))

  ex_cv_grid <- spatialBlock(
    speciesData = ex_growth_df,
    verbose = FALSE,
    k = 2,
    selection = "systematic",
    blocks = blocks,
    showBlocks = FALSE
  )

  ex_growth_df <- ex_growth_df %>%
    mutate(foldID = ex_cv_grid$foldID %>% as.factor())

  ex_cv_grid_sf <- ex_cv_grid$blocks %>%
    st_as_sf()

  focal_vs_comp_ex <- ex_growth_df %>%
    create_focal_vs_comp(max_dist, cv_grid_sf = ex_cv_grid_sf, id = "ID")

  expect_true(check_inherits(focal_vs_comp_ex, "data.frame"))
  expect_true(
    check_focal_vs_comp(focal_vs_comp_ex) %>%
      unlist() %>%
      all()
  )

  comp_bayes_lm_ex <- focal_vs_comp_ex %>%
    comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)

  expect_true(check_comp_bayes_lm(comp_bayes_lm_ex))

  predictions <- comp_bayes_lm_ex %>%
    predict(focal_vs_comp = focal_vs_comp_ex) %>%
    right_join(ex_growth_df, by = c("focal_ID" = "ID"))

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
        run_cv(max_dist = max_dist, cv_grid = ex_cv_grid_sf) %>%
        right_join(ex_growth_df, by = c("focal_ID" = "ID")),
    "data.frame"
    )
  )
})
