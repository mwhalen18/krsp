#' Lookup up the juveniles in a litter
#'
#' Lookup a litter based on year, squirrel ID, and litter number and return a
#' data frame of the juveniles in that litter.
#'
#' @param con Connection to KRSP database
#' @param year integer; year to search within. Defaults to current year.
#' @param squirrel_id integer; squirrel ID.
#' @param ln integer; litter number.
#'
#' @return A data frame of the juveniles in the given litter.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_litter_lookup(con, year = 2013, squirrel_id = 11059, ln = 1)
krsp_litter_lookup <- function(con, year, squirrel_id, ln) {
  UseMethod("krsp_litter_lookup")
}

#' @export
krsp_litter_lookup.krsp <- function(con, year, squirrel_id, ln) {
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE),
              is_integer(squirrel_id), length(squirrel_id) == 1,
              is_integer(ln), length(ln) == 1, ln < 5)

  # nest lookup query
  nest_query <- sprintf(
  "SELECT
	  j.litter_id, j.squirrel_id, j.sex, j.weight, j.tagWt AS tag_weight,
    l.date1 AS n1_date, l.tagDt AS tag_date
  FROM
    litter 										l
    INNER JOIN juvenile 			j
      ON l.id = j.litter_id
  WHERE
    l.yr = %i
    AND l.squirrel_id = %i
    AND l.ln = %i;", as.integer(year), as.integer(squirrel_id), as.integer(ln))

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    nest <- krsp_sql(con, nest_query)
  })
  # check for valid results
  if (nrow(nest) == 0) {
    stop("No corresponding nest found.")
  }
  if (n_distinct(nest$litter_id) > 1) {
    stop("Multiple matching nests.")
  }
  as.tbl(nest)
}

# look up the last date pregnant before a litter and first date lac
# a helper function for the shiny app part date calculator
lastpreg_firstlac <- function(con, sid, n1_date) {
  n1_date <- lubridate::ymd(n1_date)
  year_arg = lubridate::year(n1_date)
  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # all collar records from trapping
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ squirrel_id == sid,
              ~ year == year_arg,
              ~ rep_con %in% c(3, 4) | nipple == 4) %>%
      select_("date", "rep_con", "nipple") %>%
      collect()
  })
  # subset to within a month of n1
  trapping <- trapping %>%
    filter_(~ date > n1_date - 15,
            ~ date > n1_date + 15)
  last_preg <- trapping %>%
    filter_(~ rep_con %in% c(3, 4))
  last_preg <- if (nrow(last_preg) ==0) NA else max(last_preg$date)
  first_lac <- trapping %>%
    filter_(~ nipple == 4)
  first_lac <- if (nrow(first_lac) ==0) NA else min(first_lac$date)
  c(last_preg = last_preg, first_lac = first_lac)
}
