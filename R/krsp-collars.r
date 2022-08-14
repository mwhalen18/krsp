#' Radio collar status
#'
#' Generate a list of all squirrels that currently have collars on. A squirrel
#' is assumed to have a collar on if there is a record for a new radio collar (
#' `radio = 1`), but no matching radio collar off record (`radio = 4`).
#'
#' @param con Connection to KRSP database
#' @param grid character; one or more grids to search. Defaults to all grids.
#' @param year integer; year to search within. Defaults to current year.
#'
#' @return A data frame of all squirrels with collars on.
#' @export
#' @examples
#' \dontrun{
#' con <- krsp_connect()
#' krsp_collars(con, grid = "JO", year = 2014)
#' }

#' @export
krsp_collars <- function(con, grid, year = current_year()) {
  # assertion on arguments
  assert_that(inherits(con, "MySQLConnection"),
              missing(grid) || valid_grid(grid),
              valid_year(year, single = TRUE))
  year_arg <- as.integer(year)

  # query for most recent trapping record for squirrels with collars
  if (missing(grid)) {
    grid_str <- ""
  } else {
    grid_str <- paste0("s.gr IN ('", paste(grid, collapse = "','"), "') AND ")
  }
  recent_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id, t.date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy
    FROM
      trapping t
    INNER JOIN squirrel s
      ON t.squirrel_id = s.id
    WHERE
      %s
      (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE YEAR(date) = %i
        GROUP BY squirrel_id)
      AND t.squirrel_id IN (
        SELECT DISTINCT squirrel_id
        FROM trapping WHERE radio = 1);", grid_str, year_arg)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # all collar records from trapping
    collars <- tbl(con, "trapping") %>%
      mutate(year =  year(date)) %>%
      filter( !is.na(squirrel_id),
               radio %in% c(1, 2, 3, 4),
               year == year_arg) %>%
      select("id", grid = "gr", "year", observer = "obs",
              "date", "squirrel_id",
              "radio", "collar")
    recent <- krsp_sql(con, recent_query)
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      collars <- filter(collars,  grid == grid_arg)
    } else {
      collars <- filter(collars,  grid %in% grid_arg)
    }
  }
  # collect results
  collars <- collect(collars) %>%
    mutate(date =  lubridate::ymd(date)) %>%
    select("id", "grid", "year", "observer",
            "squirrel_id", "date", "radio", "collar")
  # replace missing frequencies with previous non-NA value
  collars <- collars %>%
    mutate(collar =  if_else(grepl("^[0-9]{6}$", as.character(collar)),
                               collar, NA_integer_)) %>%
    arrange(squirrel_id, date) %>%
    group_by(squirrel_id) %>%
    mutate(collar =  replace_na(collar)) %>%
    ungroup()
  recent <- recent %>%
    mutate(
      color_left =  ifelse(is.na(color_left) | color_left == "",
                            "-", color_left),
      color_right =  ifelse(is.na(color_right) | color_right == "",
                             "-", color_right),
      taglft =  ifelse(is.na(taglft) | taglft == "", "-", taglft),
      tagrt =  ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
      locx =  ifelse(is.na(locx) | locx == "", "-", locx),
      locy =  ifelse(is.na(locy) | locy == "", "-", locy),
      colours =  paste(color_left, color_right, sep = "/"),
      tags =  paste(taglft, tagrt, sep = "/"),
      loc =  paste(locx, locy, sep = "/")) %>%
    select("squirrel_id", last_trapped = "date", "colours", "tags", "loc")

  # create collar timeline
  rc_new <- collars %>%
    filter( radio == 1) %>%
    distinct(squirrel_id, date) %>%
    rename(date_new = date)
  rc_off <- collars %>%
    filter( radio == 4) %>%
    distinct(squirrel_id, date) %>%
    rename(date_off = date)
  # combine rc new and rc off
  timeline <- left_join(rc_new, rc_off, by = "squirrel_id") %>%
    # find rc off closest to rc new
    mutate(date_off =  if_else(date_off < date_new, as.Date(NA), date_off)) %>%
    group_by(squirrel_id, date_new) %>%
    arrange(squirrel_id, date_new, date_off) %>%
    do(utils::head(., 1)) %>%
    # find rc new closest to rc off
    mutate(date_diff =  as.integer(date_off - date_new),
            date_diff =  if_else(date_diff < 0, NA_integer_, date_diff)) %>%
    group_by(squirrel_id, date_off) %>%
    mutate(date_to =  if_else(date_diff == min(date_diff),
                                date_off, as.Date(NA))) %>%
    ungroup() %>%
    mutate(date_off =  date_to) %>%
    select("squirrel_id", "date_new", "date_off")

  # now fill in gaps in timeline assuming date off is day before next rc new
  timeline <- timeline %>%
    group_by(squirrel_id) %>%
    mutate(date_lead =  (lead(date_new, 1, order_by = date_new) - 1),
            date_off =  coalesce(date_off, date_lead)) %>%
    ungroup() %>%
    select("squirrel_id", "date_new", "date_off")

  # list of squirrels with collars on
  results <- timeline %>%
    filter( is.na(date_off)) %>%
    select("squirrel_id", date = "date_new") %>%
    inner_join(collars, by = c("squirrel_id", "date")) %>%
    left_join(recent, by = "squirrel_id") %>%
    select("id", "grid", "year",
            "squirrel_id", "tags", "colours", "loc", "last_trapped",
            "observer", date_new = "date", "collar") %>%
    arrange(grid, year, date_new) %>%
    tibble()

  return(results)
}
