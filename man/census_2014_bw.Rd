% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bw_datasets.R
\docType{data}
\name{census_2014_bw}
\alias{census_2014_bw}
\title{Michigan Big Woods research plot data}
\format{
A data frame with 48371 rows and 8 variables:
\describe{
\item{treeID}{Tree identification number. This identifies an individual tree and
can be used to connect trees between the two censuses.}
\item{stemID}{Stem number for a multi-stemmed individual. For all trees this starts
at 1 and continues up from there. To uniquely identify a stem across the plot this
value must be combined with \code{treeID}.}
\item{sp}{Code for the species. See \code{\link{species_bw}} for scientific name.}
\item{dbh}{Diameter at breast}
\item{gx}{x-coordinate meters from reference point}
\item{gy}{y-coordinate meters from reference point}
\item{date}{Approximate date the stem was measured}
\item{codes}{Code for additional information on the stem: M means the main stem
of the individual tree; AL means the stem is alive but leaning or completely fallen
over; B means the stem is broken and over half the canopy is assumed to be missing;
and R means the stem was lost, but the tag was moved to another stem greater than DBH cutoff,
this stands for resprout.}
}
}
\usage{
census_2014_bw
}
\description{
The \href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Big Woods}
data come from three censuses of a 23 ha forest research plots. All
free-standing vegetation greater than 1 cm diameter at 1.3 m height (diameter
at breast height; DBH) were tagged, identified, spatially mapped and had
their DBH measured. The original census took place in 2003 and covered only
12 ha. A second census took place from 2008-2010 and expanded the plot to its
current 23 ha. In the first and second censuses trees larger than 3.1 cm DBH
were included. Finally a third census took place in 2014. In this census
trees larger than 1 cm DBH were included. In the second and third censuses
the original trees were found, recorded for survival, remeasured, and new
individuals were tagged. This data frame has data from the third census
(2014).
}
\details{
This plot is part of the Smithsonian Institution's Forest Global Earth
Observatory \href{https://forestgeo.si.edu/}{(ForestGEO)} global network of
forest research sites. For complete details on this dataset see its
\href{https://deepblue.lib.umich.edu/data/concern/data_sets/ht24wj48w}{Deep Blue Data repository page}.
}
\examples{
library(ggplot2)
library(sf)
library(dplyr)

# Convert all 2008 plot stems to sf object
census_2008_bw_sf <- census_2008_bw \%>\%
  st_as_sf(coords = c("gx", "gy"))

# Plot stems with plot boundary
ggplot() +
  geom_sf(data = census_2008_bw_sf, size = 0.25)

# Species-specific mortality between 2008 and 2014 censuses
census_2008_bw \%>\%
  left_join(census_2014_bw, by = c("treeID", "stemID"), suffix = c("_2008", "_2014")) \%>\%
  mutate(mortality = ifelse(is.na(dbh_2014), 1, 0)) \%>\%
  group_by(sp_2008) \%>\%
  summarize(mortality = mean(mortality), n = n()) \%>\%
  arrange(desc(n))
}
\seealso{
Other Big Woods data: 
\code{\link{census_2008_bw}},
\code{\link{species_bw}},
\code{\link{study_region_bw}}

Other example data objects: 
\code{\link{blocks_ex}},
\code{\link{census_1_ex}},
\code{\link{census_2008_bw}},
\code{\link{census_2_ex}},
\code{\link{comp_bayes_lm_ex}},
\code{\link{focal_vs_comp_ex}},
\code{\link{growth_ex}},
\code{\link{growth_spatial_ex}},
\code{\link{growth_toy}},
\code{\link{species_bw}},
\code{\link{study_region_bw}},
\code{\link{study_region_ex}}
}
\concept{Big Woods data}
\concept{example data objects}
\keyword{datasets}
