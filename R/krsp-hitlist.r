#' List last year's breeders by parturition date
#'
#' List all female squirrel from August census of previous year, including
#' location, in descending order of parturition date. This is meant to be a
#' to do list at the beginning of the season, i.e. the earliest breeders last
#' year should be caught first this year.
#'
#' @param con Connection to KRSP database
#' @param year integer; year to generate hitlist for, e.g. if year = 2016,
#'    breeders from 2015 will be listed.
#'
#' @return A tbl of all female squirrels from previous August's census,
#'    including location, in descending order of parturition date. Can be
#'    converted to a data.frame.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_hitlist(con, 2016)
krsp_hitlist <- function(con, year) {
  UseMethod("krsp_hitlist")
}

#' @export
krsp_hitlist.krsp <- function(con, year = current_year()) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE))

  year <- as.integer(year)
  aug_start <- paste0(year - 1, "-08-01")
  aug_end <- paste0(year - 1, "-08-31")

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    census <- tbl(con, "census") %>%
      filter_(~ sex == "F",
              ~ census_date >= aug_start,
              ~ census_date <= aug_end) %>%
      select_("squirrel_id", "gr", "taglft", "tagrt", "reflo", "locx", "locy")
    litter <- tbl(con, "litter") %>%
      filter_(~ yr == (year - 1), ~ ln == 1, ~ br != 0) %>%
      mutate_(part_date = ~ coalesce(fieldbdate, date1, tagdt)) %>%
      select_("squirrel_id", "part_date")
  })
  left_join(census, litter, by = "squirrel_id") %>%
    mutate_(non_breeder = ~ is.na(part_date)) %>%
    arrange_("non_breeder", "part_date", "gr", "squirrel_id") %>%
    collect() %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id)) %>%
    select_(~ -non_breeder)
}
