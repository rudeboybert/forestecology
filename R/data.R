#' Bigwoods forest data
#'
#' Census of trees in bigwoods forest
#'
#' @format A data frame with 50023 rows and 10 variables:
#' \describe{
#'   \item{ID}{Tree ID}
#'   \item{species}{Linnaean species name}
#'   \item{family_phylo}{Corresponding species phylogenetic family}
#'   \item{trait_group}{Corresponding species trait-based cluster}
#'   \item{x}{x-coordinate (meters from reference point)}
#'   \item{y}{y-coordinate (meters from reference point)}
#'   \item{growth}{Average annual growth in diameter at breast height from 2008-2014 (6 years)}
#'   \item{dbh08}{Diameter at breast high in 2008}
#'   \item{dhb14}{Diameter at breast high in 2014}
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
#' @seealso \code{\link{families}}
#' @examples{
#' library(ggplot2)
#' ggplot(data = bigwoods, aes(x,y)) +
#'   geom_point() +
#'   coord_fixed(ratio=1) +
#'   geom_path(data = study_region_vertices, col = "red", size = 1)
#' }
"bigwoods_study_region"



#' Phylogenic groupings and trait based clustering of various tree species
#'
#' A dataset mapping various tree species to their phylogenetic families and
#' trait-based (rather than evolutionary relationship-based) clusters.
#'
#' @format A data frame with 50 rows and 3 variables:
#' \describe{
#'   \item{spcode}{Linnaean species name}
#'   \item{family_phylo}{Taxonomic/phylogenetic grouping at higher level than family.}
#'   \item{trait_group}{Clustering of species based on their traits rather than their evolutionary relationships.}
#' }
#' @source See \url{https://en.wikipedia.org/wiki/Family_(biology)} for biological/taxonomical definition of family.
#' @seealso \code{\link{bigwoods}}
"families"

