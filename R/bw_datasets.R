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
#'
#' @format A data frame with 27193 rows and 8 variables:
#' \describe{
#'   \item{treeID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{stemID}{Stem number for a multi-stemmed individual. For all trees this starts
#'   at 1 and continues up from there. To uniquely identify a stem across the plot this
#'   value must be combined with \code{treeID}.}
#'   \item{dbh}{Diameter at breast}
#'   \item{sp}{Code for the species. See \code{species_bw} for scientific name.}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Approximate date the stem was measured}
#'   \item{codes}{Code for additional information on the stem: M means the main stem
#'   of the individual tree; AL means the stem is alive but leaning or completely fallen
#'   over; B means the stem is broken and over half the canopy is assumed to be missing;
#'   and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#' }
#'
#' @family Big Woods data
#' @family example data objects
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Convert all 2008 plot stems to sf object
#' census_2008_bw_sf <- census_2008_bw %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = census_2008_bw_sf, size = 0.25)
"census_2008_bw"





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
#'
#' @format A data frame with 48371 rows and 8 variables:
#' \describe{
#'   \item{treeID}{Tree identification number. This identifies an individual tree and
#'   can be used to connect trees between the two censuses.}
#'   \item{stemID}{Stem number for a multi-stemmed individual. For all trees this starts
#'   at 1 and continues up from there. To uniquely identify a stem across the plot this
#'   value must be combined with \code{treeID}.}
#'   \item{sp}{Code for the species. See \code{\link{species_bw}} for scientific name.}
#'   \item{dbh}{Diameter at breast}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Approximate date the stem was measured}
#'   \item{codes}{Code for additional information on the stem: M means the main stem
#'   of the individual tree; AL means the stem is alive but leaning or completely fallen
#'   over; B means the stem is broken and over half the canopy is assumed to be missing;
#'   and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
#'   this stands for resprout.}
#' }
#'
#' @family Big Woods data
#' @family example data objects
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#' library(dplyr)
#'
#' # Convert all 2008 plot stems to sf object
#' census_2008_bw_sf <- census_2008_bw %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = census_2008_bw_sf, size = 0.25)
#'
#' # Species-specific mortality between 2008 and 2014 censuses
#' census_2008_bw %>%
#'   left_join(census_2014_bw, by = c("treeID", "stemID"), suffix = c("_2008", "_2014")) %>%
#'   mutate(mortality = ifelse(is.na(dbh_2014), 1, 0)) %>%
#'   group_by(sp_2008) %>%
#'   summarize(mortality = mean(mortality), n = n()) %>%
#'   arrange(desc(n))
"census_2014_bw"





#' Phylogenic groupings and trait based clustering of various tree species
#'
#' A date frame mapping the species codes to their common names, scientific
#' names, and families. This also includes a trait-based clustering of the
#' species.
#'
#' @format A data frame with 46 rows and 6 variables:
#' \describe{
#'   \item{sp}{The code for the species. Link to \code{\link{census_2008_bw}} and \code{\link{census_2014_bw}} with \code{sp} variable.}
#'   \item{genus}{Genus}
#'   \item{species}{Species epithet}
#'   \item{latin}{Scientific name}
#'   \item{family}{Family}
#'   \item{trait_group}{Clustering of species based on three traits rather than
#'   their evolutionary relationships. The traits are specific leaf area, maximum
#'   height, and wood density}
#' }
#'
#' @source For more information on trait clustering see Allen and Kim 2020 "A permutation
#' test and spatial cross-validation approach to assess models of interspecific competition
#' between trees." \href{https://doi.org/10.1371/journal.pone.0229930}{PLOS One 15: e0229930}.
#'
#' @family Big Woods data
#' @family example data objects
#'
#' @examples
#' library(dplyr)
#'
#' # Original 2008 census data
#' census_2008_bw
#'
#' # 2008 census data with additional species information
#' census_2008_bw %>%
#'   left_join(species_bw, by = "sp")
"species_bw"





#' Bigwoods forest study region boundary
#'
#' Boundary region for Bigwoods defined in terms of (x,y) vertices of a polygon.
#'
#' @format A \code{sf} spatial features polygon
#'
#' @family Big Woods data
#' @family example data objects
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Convert all 2008 plot stems to sf object
#' census_2008_bw_sf <- census_2008_bw %>%
#'   st_as_sf(coords = c("gx", "gy"))
#'
#' # Plot stems with plot boundary
#' ggplot() +
#'   geom_sf(data = census_2008_bw_sf, size = 0.25) +
#'   geom_sf(data = study_region_bw, color = "red", fill = "transparent")
"study_region_bw"
