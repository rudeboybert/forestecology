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
#'\describe{
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
#' data(census_df1_ex,census_df2_ex)
#' require(dplyr)
#' # Filter out resprouts
#' census_df2_ex_no_r <- census_df2_ex %>%
#'  filter(!str_detect(codes, 'R'))
#' id <- 'ID'
#' ex_growth_df <-
#'  # Merge both censuses and compute growth:
#'  compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
#'  mutate(
#'    sp = to_any_case(sp),
#'    sp = as.factor(sp))
"census_df1_ex"


#' Example input census data for package use
#'
#' This is an example second census to be analyzed with the package.
#'
#' @format A \code{tibble}
#'\describe{
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
#' data(census_df1_ex,census_df2_ex)
#' require(dplyr)
#' # Filter out resprouts
#' census_df2_ex_no_r <- census_df2_ex %>%
#'  filter(!str_detect(codes, 'R'))
#' id <- 'ID'
#' ex_growth_df <-
#'  # Merge both censuses and compute growth:
#'  compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
#'  mutate(
#'    sp = to_any_case(sp),
#'    sp = as.factor(sp))
"census_df2_ex"


#' Study region for example data
#'
#' Boundary region for bigwoods defined in terms of (x,y) vertices of a polygon.
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


#' Example input data for \code{\link{create_focal_vs_comp}}
#'
#' An example `sf` of type generated by \code{\link{compute_growth}}
#'
#' @format A \code{sf} spatial features polygon
#' @examples
#' 1+1
"growth_df_ex"
