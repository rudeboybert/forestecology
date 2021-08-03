#' Example input census data for package use
#'
#' This is example forest census data to be analyzed with this package.
#'
#' @format A \code{tibble}
#'
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Species of the individual}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Date the stem was measured}
#'   \item{codes}{Code for additional information on the stem: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh}{Diameter at breast height of the tree in cm}
#' }
#'
#' @family example data objects
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#'
#' growth_ex <-
#'   compute_growth(
#'     census_1 = census_1_ex %>%
#'       mutate(sp = to_any_case(sp) %>% factor()),
#'     census_2 = census_2_ex %>%
#'       filter(!str_detect(codes, "R")) %>%
#'       mutate(sp = to_any_case(sp) %>% factor()),
#'     id = "ID"
#'   )
"census_1_ex"





#' Example input census data for package use
#'
#' This is an example second census to be analyzed with the package.
#'
#' @format A \code{tibble}
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Species of the individual}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Date the stem was measured}
#'   \item{codes}{Code for additional information on the stem: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh}{Diameter at breast height of the tree in cm}
#' }
#'
#' @family example data objects
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#'
#' growth_ex <-
#'   compute_growth(
#'     census_1 = census_1_ex %>%
#'       mutate(sp = to_any_case(sp) %>% factor()),
#'     census_2 = census_2_ex %>%
#'       filter(!str_detect(codes, "R")) %>%
#'       mutate(sp = to_any_case(sp) %>% factor()),
#'     id = "ID"
#'   )
"census_2_ex"





#' Study region for example data
#'
#' Boundary region for small example data set defined in terms of (x,y) vertices of a polygon.
#'
#' @format A \code{sf} spatial features polygon
#'
#' @family example data objects
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Convert stems to sf object
#' census_1_ex_sf <- census_1_ex %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = study_region_ex) +
#'   geom_sf(data = study_region_bw, color = "red", fill = "transparent")
"study_region_ex"





#' Example growth data frame for small example
#'
#' This is an example growth data frame formed from two census data frames. In this case it is
#' made by combining [census_1_ex] and [census_2_ex]. The
#' individuals alive in both censuses were linked by their tree ID.
#'
#' @format A \code{sf} spatial tibble
#'
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Species of the individual}
#'   \item{codes1}{Code for additional information on the stem during the first census: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh1}{Diameter at breast height of the tree in cm at the first census}
#'   \item{dbh2}{Diameter at breast height of the tree in cm at the second census}
#'   \item{growth}{Average annual growth between the two censuses in cm per year}
#'   \item{codes2}{Codes at the second census}
#'   \item{geometry}{Point location of the individual}
#' }
#'
#' @family example data objects
#' @seealso [compute_growth()]
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(sf)
#' library(sfheaders)
#' library(blockCV)
#'
#' growth_ex %>%
#'   ggplot() +
#'   geom_sf()
#'
#' growth_ex %>%
#'   group_by(sp) %>%
#'   summarize(mean(growth))
#'
#' # Add buffer
#' growth_spatial_ex <- growth_ex %>%
#'   add_buffer_variable(direction = "in", size = 1, region = study_region_ex)
#'
#' # Add cross-validation folds
#' fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
#' fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
#' blocks <- bind_rows(
#'   sf_polygon(fold1),
#'   sf_polygon(fold2)
#' ) %>%
#'   mutate(foldID = c(1, 2))
#'
#' SpatialBlock_ex <- spatialBlock(
#'   speciesData = growth_ex,
#'   verbose = FALSE,
#'   k = 2,
#'   selection = "systematic",
#'   blocks = blocks
#' )
#'
#' # Add foldID to data
#' growth_spatial_ex <- growth_spatial_ex %>%
#'   mutate(foldID = SpatialBlock_ex$foldID %>% as.factor())
"growth_ex"





#' Example growth data frame with spatial data for small example
#'
#' This is an example growth data frame formed from two census data frames which
#' has been updated with spatial data. It starts from [growth_ex].
#'
#' @format A \code{sf} spatial tibble
#'
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Species of the individual}
#'   \item{codes1}{Code for additional information on the stem during the first census: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh1}{Diameter at breast height of the tree in cm at the first census}
#'   \item{dbh2}{Diameter at breast height of the tree in cm at the second census}
#'   \item{growth}{Average annual growth between the two censuses in cm per year}
#'   \item{codes2}{Codes at the second census}
#'   \item{geometry}{Point location of the individual}
#'   \item{buffer}{A boolean variable for whether the individual is in the buffer region or not}
#'   \item{foldID}{Which cross-validation fold the individual is in}
#' }
#'
#' @family example data objects
#' @seealso [compute_growth()]
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(sf)
#'
#' comp_dist <- 1
#'
#' ggplot() +
#'   geom_sf(data = growth_spatial_ex, aes(col = buffer), size = 2)
#'
#' ggplot() +
#'   geom_sf(data = growth_spatial_ex, aes(col = foldID), size = 2)
#'
#' # Create the focal versus comp data frame
#' focal_vs_comp_ex <- growth_spatial_ex %>%
#'   create_focal_vs_comp(comp_dist, blocks = blocks_ex, id = "ID")
"growth_spatial_ex"





#' Example cross validation grid
#'
#' This is an example cross validation grid. This is needed to create the focal versus
#' comp data frame and run cross-validated models
#'
#' @format A \code{sf} polygons
#'
#' \describe{
#'   \item{foldID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{geometry}{Point location of the individual}
#' }
#'
#' @family example data objects
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' comp_dist <- 1
#'
#' ggplot(blocks_ex) +
#'   geom_sf() +
#'   geom_sf(data = growth_spatial_ex)
#'
#' focal_vs_comp_ex <- growth_spatial_ex %>%
#'   create_focal_vs_comp(comp_dist, blocks = blocks_ex, id = "ID")
"blocks_ex"





#' Example focal versus comp data frame
#'
#' This is an example focal versus comp data frame. The rows are focal
#' trees which are repeated for all competitor trees within a specified
#' distance from them. In this case that distance is 1. This is the focal
#' versus comp for [growth_spatial_ex].
#'
#' @format
#'
#' A [tibble::tbl_df]:
#' \describe{
#'   \item{focal_ID}{Tree identification number for the focal tree}
#'   \item{focal_sp}{Species of the focal tree}
#'   \item{dbh}{Diameter at breast height of the focal tree at the first census}
#'   \item{foldID}{The CV-fold that the focal tree is in}
#'   \item{geometry}{The point location of the focal tree}
#'   \item{growth}{The average annual growth of the focal tree between censuses}
#'   \item{comp}{A list-column: characteristics of the relevant competitor trees}
#' }
#'
#' The `comp` list-column contains [tibble::tbl_df]s with columns:
#' \describe{
#'   \item{comp_ID}{Tree identification number for the competitor tree}
#'   \item{dist}{The distance between the focal and comp tree, this will be less than the max distance specified.}
#'   \item{comp_sp}{Species of the comp tree}
#'   \item{comp_x_var}{Numerical variable associated with comp tree}
#' }
#'
#' @family example data objects
#' @seealso [create_focal_vs_comp()]
#'
#' @importFrom forcats fct_rev
#'
#' @examples
#' comp_bayes_lm_ex <- focal_vs_comp_ex %>%
#'   comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)
"focal_vs_comp_ex"





#' Example bayesian competition model fit
#'
#' @description This object contains an example fitted Bayesian competition
#' model outputted by [comp_bayes_lm()].
#'
#' @format A list subclass containing the following elements:
#'
#' \describe{
#'   \item{prior_params}{Prior parameters supplied to [comp_bayes_lm()]}
#'   \item{post_params}{Posterior parameters outputted by [comp_bayes_lm()]}
#'   \item{terms}{The formula object used in model fitting}
#' }
#'
#' @importFrom forcats fct_rev
#'
#' @import ggplot2
#'
#' @family example data objects
#' @seealso [comp_bayes_lm()]
#'
#' @examples
#' library(dplyr)
#' library(yardstick)
#'
#' # Compare model predictions to observation
#' predictions <- focal_vs_comp_ex %>%
#'   mutate(growth_hat = predict(comp_bayes_lm_ex, focal_vs_comp_ex))
#'
#' predictions %>%
#'   rmse(truth = growth, estimate = growth_hat) %>%
#'   pull(.estimate)
#'
#' # Plot posterior parameters
#' comp_bayes_lm_ex %>%
#'   autoplot()
"comp_bayes_lm_ex"





#' Example input data for [create_focal_vs_comp()]
#'
#' An example `sf` of type generated by [compute_growth()]
#'
#' @format A \code{sf} spatial features polygon
#'
#' @family example data objects
"growth_toy"
