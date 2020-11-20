#' Michigan Big Woods research plot data
#'
#' The \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Big Woods}
#' data come from three censuses of a 23 ha forest research plots. All
#' free-standing vegetation greater than 1 cm diameter at 1.3 m height (diameter
#' at breast height; DBH) were tagged, identified, spatially mapped and had
#' their DBH measured. The original census took place in 2003 and covered only
#' 12 ha. A second census took place from 2008-2010 and expanded the plot to its
#' current 23 ha. In the first and second censuses trees larger than 3.1 cm DBH
#' were included. Finally a third census took place in 2014. In this census
#' trees larger than 1 cm DBH were included. In the second and third censuses
#' the original trees were found, recorded for survival, remeasured, and new
#' individuals were tagged. This data frame has data from the second census
#' (2008-2010).
#'
#' @details This plot is part of the Smithsonian Institution's Forest Global Earth
#' Observatory \href{https://forestgeo.si.edu/}{(ForestGEO)} global network of
#' forest research sites. For complete details on this dataset see its
#' \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Deep Blue Data repository page}.
#' @format A data frame with 27193 rows and 8 variables:
#' \describe{
#'   \item{treeID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{stemID}{Stem number for a multi-stemmed individual. For all trees this starts
#'   at 1 and continues up from there. To uniquely identify a stem across the plot this
#'   value must be combined with \code{treeID}.}
#'   \item{dbh}{Diameter at breast}
#'   \item{sp}{Code for the species. See \code{bw_species} for scientific name.}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Approximate date the stem was measured}
#'   \item{codes}{Code for additional informaiton on the stem: M means the main stem
#'   of the individual tree; AL means the stem is alive but leaning or completely fallen
#'   over; B means the stem is broken and over half the canopy is assumed to be missing;
#'   and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#' }
#' @seealso \code{\link{bw_census_2014}}, \code{\link{bw_species}}, \code{\link{compute_growth}}
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Convert all 2008 plot stems to sf object
#' bw_census_2008_sf <- bw_census_2008 %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = bw_census_2008_sf, size = 0.25)
"bw_census_2008"



#' Michigan Big Woods research plot data
#'
#' The \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Big Woods}
#' data come from three censuses of a 23 ha forest research plots. All
#' free-standing vegetation greater than 1 cm diameter at 1.3 m height (diameter
#' at breast height; DBH) were tagged, identified, spatially mapped and had
#' their DBH measured. The original census took place in 2003 and covered only
#' 12 ha. A second census took place from 2008-2010 and expanded the plot to its
#' current 23 ha. In the first and second censuses trees larger than 3.1 cm DBH
#' were included. Finally a third census took place in 2014. In this census
#' trees larger than 1 cm DBH were included. In the second and third censuses
#' the original trees were found, recorded for survival, remeasured, and new
#' individuals were tagged. This data frame has data from the third census
#' (2014).
#'
#' @details This plot is part of the Smithsonian Institution's Forest Global Earth
#' Observatory \href{https://forestgeo.si.edu/}{(ForestGEO)} global network of
#' forest research sites. For complete details on this dataset see its
#' \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Deep Blue Data repository page}.
#' @format A data frame with 48371 rows and 8 variables:
#' \describe{
#'   \item{treeID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{stemID}{Stem number for a multi-stemmed individual. For all trees this starts
#'   at 1 and continues up from there. To uniquely identify a stem across the plot this
#'   value must be combined with \code{treeID}.}
#'   \item{sp}{Code for the species. See \code{\link{bw_species}} for scientific name.}
#'   \item{dbh}{Diameter at breast}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Approximate date the stem was measured}
#'   \item{codes}{Code for additional informaiton on the stem: M means the main stem
#'   of the individual tree; AL means the stem is alive but leaning or completely fallen
#'   over; B means the stem is broken and over half the canopy is assumed to be missing;
#'   and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#' }
#' @seealso \code{\link{bw_census_2008}}, \code{\link{bw_species}}, \code{\link{compute_growth}}
#' @examples
#' library(ggplot2)
#' library(sf)
#' library(dplyr)
#'
#' # Convert all 2008 plot stems to sf object
#' bw_census_2008_sf <- bw_census_2008 %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = bw_census_2008_sf, size = 0.25)
#'
#' # Species-specific mortality between 2008 and 2014 censuses
#' bw_census_2008 %>%
#'   left_join(bw_census_2014, by = c("treeID", "stemID"), suffix = c("_2008", "_2014")) %>%
#'   mutate(mortality = ifelse(is.na(dbh_2014), 1, 0)) %>%
#'   group_by(sp_2008) %>%
#'   summarize(mortality = mean(mortality), n = n()) %>%
#'   arrange(desc(n))
"bw_census_2014"



#' Phylogenic groupings and trait based clustering of various tree species
#'
#' A date frame mapping the species codes to their common names, scientific
#' names, and families. This also includes a trait-based clustering of the
#' species.
#'
#' @format A data frame with 46 rows and 6 variables:
#' \describe{
#'   \item{sp}{The code for the species. Link to \code{\link{bw_census_2008}} and \code{\link{bw_census_2014}} with \code{sp} variable.}
#'   \item{genus}{Genus}
#'   \item{species}{Species epithet}
#'   \item{latin}{Scientific name}
#'   \item{family}{Family}
#'   \item{trait_group}{Clustering of species based on three traits rather than
#'   their evolutionary relationships. The traits are specific leaf area, maximum
#'   height, and wood density}
#' }
#' @source For more information on trait clustering see Allen and Kim 2020 "A permutation
#' test and spatial cross-validation approach to assess models of interspecific competition
#' between trees." \href{https://doi.org/10.1371/journal.pone.0229930}{Plos One 15: e0229930}.
#' @seealso \code{\link{bw_census_2008}}, \code{\link{bw_census_2014}}
#' @examples
#' library(dplyr)
#'
#' # Original 2008 census data
#' bw_census_2008
#'
#' # 2008 census data with additional species information
#' bw_census_2008 %>%
#'   left_join(bw_species, by = "sp")
"bw_species"



#' Bigwoods forest study region boundary
#'
#' Boundary region for bigwoods defined in terms of (x,y) vertices of a polygon.
#'
#' @format A \code{sf} spatial features polygon
#' @seealso \code{\link{bw_census_2008}} and \code{\link{bw_census_2014}}
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Convert all 2008 plot stems to sf object
#' bw_census_2008_sf <- bw_census_2008 %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = bw_census_2008_sf, size = 0.25) +
#'   geom_sf(data = bw_study_region, color = "red", fill = "transparent")
"bw_study_region"


#' Example input census data for package use
#'
#' This is example forest census data to be analyzed with this package.
#'
#' @format A \code{tibble}
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Speices of the individual}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Date the stem was measured}
#'   \item{codes}{Code for additional information on the stem: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh}{Diameter at breast hieght of the tree in cm}
#' }
#' @seealso \code{\link{census_df2_ex}}
#' @examples
#' data(census_df1_ex, census_df2_ex)
#' library(dplyr)
#' library(stringr)
#' library(snakecase)
#' # Filter out resprouts
#' census_df2_ex_no_r <- census_df2_ex %>%
#'   filter(!str_detect(codes, "R"))
#' id <- "ID"
#' ex_growth_df <-
#'   # Merge both censuses and compute growth:
#'   compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
#'   mutate(
#'     sp = to_any_case(sp),
#'     sp = as.factor(sp)
#'   )
"census_df1_ex"


#' Example input census data for package use
#'
#' This is an example second census to be analyzed with the package.
#'
#' @format A \code{tibble}
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Speices of the individual}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Date the stem was measured}
#'   \item{codes}{Code for additional information on the stem: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh}{Diameter at breast hieght of the tree in cm}
#' }
#' @seealso \code{\link{census_df1_ex}}
#' @examples
#' data(census_df1_ex, census_df2_ex)
#' library(dplyr)
#' library(stringr)
#' library(snakecase)
#' # Filter out resprouts
#' census_df2_ex_no_r <- census_df2_ex %>%
#'   filter(!str_detect(codes, "R"))
#' id <- "ID"
#' ex_growth_df <-
#'   # Merge both censuses and compute growth:
#'   compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
#'   mutate(
#'     sp = to_any_case(sp),
#'     sp = as.factor(sp)
#'   )
"census_df2_ex"


#' Study region for example data
#'
#' Boundary region for small example data set defined in terms of (x,y) vertices of a polygon.
#'
#' @format A \code{sf} spatial features polygon
#' @seealso \code{\link{census_df1_ex}}
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Convert stems to sf object
#' census_df1_ex_sf <- census_df1_ex %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = ex_study_region) +
#'   geom_sf(data = bw_study_region, color = "red", fill = "transparent")
"ex_study_region"

#' Example growth data frame for small example
#'
#' This is an example growth data frame formed from two census data frames. In this case it is
#' made by combining \code{\link{census_df1_ex}} and \code{\link{census_df2_ex}}. The
#' individuals alive in both censuses were linked by their tree ID.
#'
#' @format A \code{sf} spatial tibble
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Speices of the individual}
#'   \item{codes1}{Code for additional information on the stem during the first census: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh1}{Diameter at breast hieght of the tree in cm at the first census}
#'   \item{dbh2}{Diameter at breast hieght of the tree in cm at the second census}
#'   \item{growth}{Average annual growth between the two censuses in cm per year}
#'   \item{codes2}{Codes at the second census}
#'   \item{geometry}{Point location of the individual}
#' }
#' @seealso \code{\link{census_df1_ex}}, \code{\link{census_df2_ex}}, and \code{\link{compute_growth}}
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(sf)
#' library(sfheaders)
#' library(blockCV)
#'
#' ex_growth_df %>%
#'   ggplot() +
#'   geom_sf()
#'
#' ex_growth_df %>%
#'   group_by(sp) %>%
#'   summarize(mean(growth))
#'
#' # Add buffer
#' ex_growth_df_spatial <- ex_growth_df %>%
#'   add_buffer_variable(direction = "in", size = 1, region = ex_study_region)
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
#' ex_cv_grid <- spatialBlock(
#'   speciesData = ex_growth_df,
#'   verbose = FALSE,
#'   k = 2,
#'   selection = "systematic",
#'   blocks = blocks
#' )
#'
#' # Add foldID to data
#' ex_growth_df_spatial <- ex_growth_df_spatial %>%
#'   mutate(foldID = ex_cv_grid$foldID %>% as.factor())
"ex_growth_df"

#' Example growth data frame with spatial data for small example
#'
#' This is an example growth data frame formed from two census data frames which
#' has been updated with spatial data. It starts from \code{\link{ex_growth_df}}.
#'
#' @format A \code{sf} spatial tibble
#' \describe{
#'   \item{ID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{sp}{Speices of the individual}
#'   \item{codes1}{Code for additional information on the stem during the first census: M means the main stem
#'   of the individual tree and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#'   \item{dbh1}{Diameter at breast hieght of the tree in cm at the first census}
#'   \item{dbh2}{Diameter at breast hieght of the tree in cm at the second census}
#'   \item{growth}{Average annual growth between the two censuses in cm per year}
#'   \item{codes2}{Codes at the second census}
#'   \item{geometry}{Point location of the individual}
#'   \item{buffer}{A boolean variable for whether the individual is in the buffer region or not}
#'   \item{foldID}{Which cross-validation fold the individual is in}
#' }
#' @seealso \code{\link{ex_growth_df}}, \code{\link{add_buffer_variable}}, \code{\link{ex_cv_grid_sf}}
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(sf)
#'
#' max_dist <- 1
#'
#' ggplot() +
#'   geom_sf(data = ex_growth_df_spatial, aes(col = buffer), size = 2)
#'
#' ggplot() +
#'   geom_sf(data = ex_growth_df_spatial, aes(col = foldID), size = 2)
#'
#' # Create the focal versus comp data frame
#' focal_vs_comp_ex <- ex_growth_df_spatial %>%
#'   create_focal_vs_comp(max_dist, cv_grid_sf = ex_cv_grid_sf, id = "ID")
"ex_growth_df_spatial"


#' Example cross validation grid
#'
#' This is an example cross validation grid. This is needed to create the focal versus
#' comp data frame and run cross-validated models
#'
#' @format A \code{sf} polygons
#' \describe{
#'   \item{foldID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{geometry}{Point location of the individual}
#' }
#' @seealso \code{\link{ex_growth_df_spatial}}
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' max_dist <- 1
#'
#' ggplot(ex_cv_grid_sf) +
#'   geom_sf() +
#'   geom_sf(data = ex_growth_df_spatial)
#'
#' focal_vs_comp_ex <- ex_growth_df_spatial %>%
#'   create_focal_vs_comp(max_dist, cv_grid_sf = ex_cv_grid_sf, id = "ID")
"ex_cv_grid_sf"

#' Example focal versus comp data frame
#'
#' This is an example focal versus comp data frame. The rows are focal
#' trees which are repeated for all competitor trees within a specified
#' distance from them. In this case that distance is 1. This is the focal
#' versus comp for \code{\link{ex_growth_df_spatial}}.
#'
#' @format A tibble
#' \describe{
#'   \item{focal_ID}{Tree identification number for the focal tree}
#'   \item{focal_sp}{Species of the focal tree}
#'   \item{dbh}{Diameter at breast height of the focal tree at the first census}
#'   \item{foldID}{The CV-fold that the focal tree is in}
#'   \item{geometry}{The point location oif the focal tree}
#'   \item{growth}{The average annual growth of the focal tree between censuses}
#'   \item{comp_ID}{Tree identificaiton number for the competitor tree}
#'   \item{dist}{The distance between the focal and comp tree, this will be less than the max distnace specified.}
#'   \item{comp_so}{Species of the comp tree}
#'   \item{comp_basal_area}{Basal area of the comp tree}
#' }
#' @seealso \code{\link{ex_growth_df_spatial}}
#' @importFrom forcats fct_rev
#' @examples
#' library(dplyr)
#'
#' posterior_param_ex <- focal_vs_comp_ex %>%
#'   fit_bayesian_model(prior_param = NULL, run_shuffle = FALSE)
"focal_vs_comp_ex"

#' Example fit model
#'
#' This has posterior parameters for the fit growth model.
#'
#' @format ??
#' \describe{
#'   \item{??}{Something}
#' }
#' @importFrom forcats fct_rev
#' @import ggplot2
#' @seealso \code{\link{focal_vs_comp_ex}}
#' @examples
#' library(dplyr)
#' library(yardstick)
#'
#' # Compare model predictions to observation
#' predictions <- focal_vs_comp_ex %>%
#'   predict_bayesian_model(posterior_param = posterior_param_ex) %>%
#'   right_join(ex_growth_df, by = c("focal_ID" = "ID"))
#' predictions %>%
#'   rmse(truth = growth, estimate = growth_hat) %>%
#'   pull(.estimate)
#'
#' # Plot posterior parameters
#' posterior_param_ex %>%
#'   plot_posterior_parameters()
"posterior_param_ex"

#' Example input data for \code{\link{create_focal_vs_comp}}
#'
#' An example `sf` of type generated by \code{\link{compute_growth}}
#'
#' @format A \code{sf} spatial features polygon
#' @examples
#' 1 + 1
"growth_df_ex"
