globalVariables(c(
  "codes", "date1", "date2"
))

#' Compute growth of trees
#'
#' Based on two tree censuses compute the average annual growth in dbh for all
#' trees that were alive at (TODO: fill this in).
#'
#' @param census_df1 A data frame of the first census.
#' @param census_df2 A data frame of the second (later) census
#' @param id Name of variable common to \code{census_df1} and \code{census_df2}
#' allowing you to join/merge both data frames.
#'
#' @return growth_df A data frame with \code{growth}: average annual growth in dbh.
#' @export
#' @import dplyr
#' @import sf
#' @examples
#' 1+1
compute_growth <- function(census_df1, census_df2, id) {

  # TODO: Write following checks
  # - Both census data frames have variables: id, dbh, date, and codes.
  # - Check variable types: chr, dbl, date/dttm, NA
  # - Check that id uniquely identifies rows
  # - Prompt use with message: "Assuming dbh are in cm"

  # Limit second census data to only those variables that can change
  census_df2 <- census_df2 %>%
    select(id, dbh, date, codes)

  growth_df <- census_df1 %>%
    filter(dbh > 0) %>%
    # TODO: Hey Dave, don't we want inner_join here then?
    # left_join because we want all trees from census 1 (competitors) but
    # only want trees from census 2 that were alive in 1 (to see how much they grew)
    left_join(census_df2, by = id, suffix = c("1", "2")) %>%
    # Compute avg annual growth:
    mutate(
      n_days = difftime(date2, date1),
      n_days = as.numeric(n_days),
      n_years = n_days/365.25,
      growth = (dbh2 - dbh1)/n_years
    ) %>%
    select(-c(n_days, n_years, date1, date2)) %>%


  return(growth_df)
}
