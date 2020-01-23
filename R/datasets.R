#' Michigan Big Woods research plot data
#'
#' The \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Big Woods}
#' data come from three censuses of a 23 ha forest research plots. All
#' free-standing vegetation greater than 1 cm diameter at 1.3 m height (diameter
#' at breast height; DBH) were tagged, identified, spatially mapped and had
#' their DBH measured. The original census took place in 2008 and covered only
#' 12 ha. A second census took place from 2008-2010 and expanded the plot to its
#' current 23 ha. Finally a third census took place in 2014. In the second and
#' third censuses the original trees were found, recorded for survival,
#' remeasured, and new individuals were tagged.
#'
#' @details This plot is part of the Smithsonian Institution's Forest Global Earth
#' Observatory \href{https://forestgeo.si.edu/}{(ForestGEO)} global network of
#' forest research sites.
#' @format A data frame with 50023 rows and 10 variables:
#' \describe{
#'   \item{ID}{Tree ID}
#'   \item{species}{Linnaean species name}
#'   \item{family_phylo}{Corresponding species phylogenetic family}
#'   \item{trait_group}{Corresponding species trait-based cluster}
#'   \item{x}{x-coordinate meters from reference point}
#'   \item{y}{y-coordinate meters from reference point}
#'   \item{growth}{Average annual growth in diameter at breast height from 2008-2014 6 years}
#'   \item{dbh08}{Diameter at breast high in 2008}
#'   \item{dbh14}{Diameter at breast high in 2014}
#'   \item{code14}{Code}
#' }
#' @seealso \code{\link{families}}
"bigwoods"



#' Bigwoods forest study region boundary
#'
#' Boundary region for bigwoods defined in terms of (x,y) vertices of a polygon.
#'
#' @format A data frame with 13 rows and 2 variables:
#' \describe{
#'   \item{x}{x-coordinate (meters from reference point)}
#'   \item{y}{y-coordinate (meters from reference point)}
#' }
#' @seealso \code{\link{families}} \code{\link{define_bigwoods_buffer}}
#' @examples
#' library(ggplot2)
#' ggplot(bigwoods, aes(x = x, y = y)) +
#'   # Mark study region boundary
#'   geom_path(data = bigwoods_study_region, size = 1) +
#'   coord_fixed(ratio = 1)
"bigwoods_study_region"



#' Phylogenic groupings and trait based clustering of various tree species
#'
#' A dataset mapping various tree species to their phylogenetic families and
#' trait-based (rather than evolutionary relationship-based) clusters.
#'
#' @format A data frame with 50 rows and 3 variables:
#' \describe{
#'   \item{spcode}{Linnaean species name}
#'   \item{family}{Taxonomic/phylogenetic grouping at higher level than family.}
#'   \item{trait_group}{Clustering of species based on their traits rather than
#'   their evolutionary relationships.}
#' }
#' @source See \url{https://en.wikipedia.org/wiki/Family_(biology)} for
#'   biological/taxonomical definition of family.
#' @seealso \code{\link{bigwoods}}
"families"


