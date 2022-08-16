#' Check radio collars
#'
#' Check that radio collars have been correctly entered into the trapping table.
#'
#' @param con Connection to KRSP database
#' @param grid character; one or more grids to search. Defaults to all grids.
#' @param year integer; year to search within. Defaults to current year.
#'
#' @section Checks:
#'
#' The following checks have been implemented:
#'
#' \enumerate{
#'   \item All trapping records with `radio` = 1 (new collar), 2 (collar
#'     on), 3 (collar change), or 4 (collar removed) should have a radio collar
#'     frequency in the `collar` field.
#'   \item Radio collar frequencies should all be 6 digits with no decimal. If a
#'     decimal is used the frequency will get rounded to the nearest integer,
#'     e.g. 150.231 -> 150.
#'   \item Any squirrel with `radio` = 2 (collar on), 3 (collar change) or
#'     4 (collar removed) should have a previous record with `radio` = 1
#'     (new collar).
#'   \item All squirrels that currently have no collar (`radio = 5`), but
#'     previously had a collar (`radio = 1-3`), should have record for a
#'     collar removal (`radio = 4`).
#' }
#'
#' @return A data frame of records that failed the checks.
#' @export
#' @examples
#' \dontrun{
#' con <- krsp_connect()
#' check_collars(con, grid = "KL", year = 2014)
#' }

#' @export
check_collars <- function(con, grid, year = current_year()) {
  # assertion on arguments
  assert_that(inherits(con, "MySQLConnection"),
              missing(grid) || valid_grid(grid),
              valid_year(year, single = TRUE))
  year_arg <- as.integer(year)

  # query for most recent trapping record
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
      t.locx, t.locy, t.radio
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
        FROM trapping WHERE radio IN (1,2,3,4));", grid_str, year_arg)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    collars <- tbl(con, "trapping") %>%
      mutate(year = year(date)) %>%
      # find missing collar weight
      filter(!is.na(squirrel_id),
              radio %in% c(1, 2, 3, 4),
              year == year_arg) %>%
      select("id", grid = "gr", "year", observer = "obs",
              "squirrel_id", "date",
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
    select("squirrel_id", "date", "colours", "tags", "loc", "radio")

  # check frequency present and valid
  results <- collars %>%
    filter( !grepl("^[0-9]{6}$", collar)) %>%
    mutate(check =  "check_collars_frequency")

  # create collar timeline
  rc_new <- collars %>%
    filter( radio == 1) %>%
    distinct(squirrel_id, date) %>%
    rename(date_new = "date")
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

  # rc removed without rc new
  results <- collars %>%
    filter( radio == 4) %>%
    anti_join(timeline, by = c("squirrel_id", date = "date_off")) %>%
    mutate(check =  "check_collars_rcnew") %>%
    bind_rows(results)

  # rc new without rc off, based on subsequent rc new
  results <- timeline %>%
    group_by(squirrel_id) %>%
    mutate(rank =  dense_rank(date_new)) %>%
    filter( (is.na(date_off) & rank != max(rank))) %>%
    ungroup() %>%
    select("squirrel_id", date = "date_new") %>%
    inner_join(collars %>% filter( radio == 1),
               by = c("squirrel_id", "date")) %>%
    mutate(check =  "check_collars_rcoff") %>%
    bind_rows(results, .)
  # now fill in gaps in timeline assuming date off is day before next rc new
  timeline <- timeline %>%
    group_by(squirrel_id) %>%
    mutate(date_lead =  (lead(date_new, 1, order_by = date_new) - 1),
            date_off =  coalesce(date_off, date_lead,
                                  lubridate::ymd("99991231"))) %>%
    ungroup() %>%
    select("squirrel_id", "date_new", "date_off")

  # collar records missing an rc new entry
  results <- collars %>%
    filter( radio %in% c(2, 3)) %>%
    select("id", "squirrel_id", "date") %>%
    left_join(timeline, by = "squirrel_id") %>%
    mutate(has_new =  !(is.na(date_new) | date < date_new | date > date_off)) %>%
    group_by(id) %>%
    summarize(has_new =  any(has_new)) %>%
    ungroup() %>%
    filter( !has_new) %>%
    distinct(id) %>%
    inner_join(collars, by = "id") %>%
    mutate(check =  "check_collars_rcnew") %>%
    bind_rows(results)

  # check for missing rc off record, signaled by non-collar records
  # find radio collars not removed that have a trapping record showing no collar
  results <- recent %>%
    filter( (is.na(radio) | radio == 5)) %>%
    distinct(squirrel_id, date) %>%
    inner_join(timeline, by = "squirrel_id") %>%
    # cases with no rc removed
    filter( date_off == lubridate::ymd("99991231")) %>%
    # ensure that last trap comes after rc new
    filter( date > date_new) %>%
    # select original collar record
    select("squirrel_id", date = "date_new") %>%
    inner_join(collars %>% filter( radio == 1),
               by = c("squirrel_id", "date")) %>%
    mutate(check =  "check_collars_rcoff") %>%
    bind_rows(results, .)

  # convert radio codes to names
  results <- tibble(radio_code = as.character(1:5),
                    radio = c("new collar", "collar on", "collar change",
                              "collar removed", "no collar")) %>%
    inner_join(results %>% rename(radio_code = "radio"), by = "radio_code") %>%
    left_join(recent %>% select("squirrel_id", "colours", "tags", "loc"),
              by = "squirrel_id") %>%
    select("check", "id", "grid", "year", "observer",
            "squirrel_id", "colours", "tags", "loc",
            "date", "radio", "collar") %>%
    arrange("grid", "year", "check", "date")
  return(results)
}
