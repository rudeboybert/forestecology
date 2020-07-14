#' Michigan Big Woods research plot data
#'
#' The \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Big Woods}
#' data come from three censuses of a 23 ha forest research plots. All
#' free-standing vegetation greater than 1 cm diameter at 1.3 m height (diameter
#' at breast height; DBH) were tagged, identified, spatially mapped and had
#' their DBH measured. The original census took place in 2003 and covered only
#' 12 ha. A second census took place from 2008-2010 and expanded the plot to its
#' current 23 ha. In the first and second censuses trees larger than 3.1 cm DBH
#' were included. Finally a third census took place in 2014. In this census trees
#' larger than 1 cm DBH were included. In the second and third censuses the original
#' trees were found, recorded for survival, remeasured, and new individuals were tagged.
#' This data frame has data from the second census (2008-2010).
#'
#' @details This plot is part of the Smithsonian Institution's Forest Global Earth
#' Observatory \href{https://forestgeo.si.edu/}{(ForestGEO)} global network of
#' forest research sites.
#' @format A data frame with 27193 rows and 8 variables:
#' \describe{
#'   \item{treeID}{Tree identification number}
#'   \item{stemID}{Stem number for a multi-stemmed individual}
#'   \item{sp}{Code for the species. See bw_species for scientific name.}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Approximate date the stem was measured}
#'   \item{codes}{Code for additional informaiton on the stem}
#' }
#' @seealso \code{\link{bw_species}} \code{\link{bw_censusdf2}} \code{\link{compute_growth}}
"bw_censusdf1"

#' Michigan Big Woods research plot data
#'
#' The \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Big Woods}
#' data come from three censuses of a 23 ha forest research plots. All
#' free-standing vegetation greater than 1 cm diameter at 1.3 m height (diameter
#' at breast height; DBH) were tagged, identified, spatially mapped and had
#' their DBH measured. The original census took place in 2003 and covered only
#' 12 ha. A second census took place from 2008-2010 and expanded the plot to its
#' current 23 ha. In the first and second censuses trees larger than 3.1 cm DBH
#' were included. Finally a third census took place in 2014. In this census trees
#' larger than 1 cm DBH were included. In the second and third censuses the original
#' trees were found, recorded for survival, remeasured, and new individuals were tagged.
#' This data frame has data from the third census (2014).
#'
#' @details This plot is part of the Smithsonian Institution's Forest Global Earth
#' Observatory \href{https://forestgeo.si.edu/}{(ForestGEO)} global network of
#' forest research sites.
#' @format A data frame with 48371 rows and 8 variables:
#' \describe{
#'   \item{treeID}{Tree identification number}
#'   \item{stemID}{Stem number for a multi-stemmed individual}
#'   \item{sp}{Code for the species. See \code{bw_species} for scientific name.}
#'   \item{gx}{x-coordinate meters from reference point}
#'   \item{gy}{y-coordinate meters from reference point}
#'   \item{date}{Approximate date the stem was measured}
#'   \item{codes}{Code for additional informaiton on the stem}
#' }
#' @seealso \code{\link{bw_species}} \code{\link{bw_censusdf2}} \code{\link{compute_growth}}
"bw_censusdf2"


#' Phylogenic groupings and trait based clustering of various tree species
#'
#' A date frame mapping the species codes to their common names, scientific
#' names, and families. This also includes a trait-based clustering of the
#' species.
#'
#' @format A data frame with 46 rows and 6 variables:
#' \describe{
#'   \item{sp}{The code for the species. Link with sp in \code{bw_censusdf1}.}
#'   \item{genus}{Genus}
#'   \item{species}{Species epithet}
#'   \item{latin}{Scientific name}
#'   \item{family}{Family}
#'   \item{trait_group}{Clustering of species based on three traits rather than
#'   their evolutionary relationships. The traits are specific leaf area, maximum
#'   height, and wood density}
#' }
#' @source For more information on trait clustering see Allen and Kim 2020. A permutation
#' test and spatial cross-validation approach to assess models of interspecific competition
#' between trees. Plos One 15: e0229930. https://doi.org/10.1371/journal.pone.0229930
#' @seealso \code{\link{bw_censusdf2}} \code{\link{bw_censusdf1}}
"bw_species"


#' Bigwoods forest study region boundary
#'
#' Boundary region for bigwoods defined in terms of (x,y) vertices of a polygon.
#'
#' @format A \code{sf} spatial polygon.
#' \describe{
#'   \item{x}{x-coordinate (meters from reference point)}
#'   \item{y}{y-coordinate (meters from reference point)}
#' }
#' @seealso \code{\link{bw_censusdf1}} \code{\link{define_buffer}}
#' @examples
#' library(ggplot2)
#' ggplot(bw_censusdf1, aes(x = gx, y = gy)) +
#'   # Mark study region boundary
#'   geom_path(data = bw_study_region, size = 1) +
#'   coord_fixed(ratio = 1)
"bw_study_region"


