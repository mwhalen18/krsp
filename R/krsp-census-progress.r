#' Display a table of progress towards census
#'
#' This function is to meant to be used to monitor progress towards completion
#' of the census. It lists all squirrels caught since the last census and notes
#' whether then have been entered into the current census.
#'
#' The user must identify which census they are completing. If it is an August
#' census, then this function returns all squirrels caught between May 15 and
#' August 15. If it's a May census that is being completed, then this function
#' returns all squirrels seen up to May 15 of the given year.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; year of census you're working on. Must be greater than
#'   2012 when the new census table was implemented.
#' @param census character; are you completing the may or august census?
#'
#' @return A data frame of squirrel seen since the last census.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_census_progress(con, "JO", 2014, "may") %>%
#'   head()
krsp_census_progress <- function(con, grid, year, census) {
  UseMethod("krsp_census_progress")
}

#' @export
krsp_census_progress.krsp <- function(con, grid, year,
                                      census = c("august", "may")) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE), year > 2012,
              valid_grid(grid, single = TRUE))
  census <- match.arg(census)

  year <- as.integer(year)
  grid_choice <- grid
  reverse_grid <- (grid_choice == "AG")

  # search period
  if (census == "may") {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-05-15")
  } else if (census == "august") {
    start_date <- paste0(year, "-05-16")
    end_date <- find_aug_census(con, grid_choice, year)
  } else {
    stop("Invalid census, must be may or august")
  }

  # query for most recent trapping record
  recent_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id, t.date AS trap_date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy,
      s.sex, t.gr AS grid
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
    WHERE
      s.gr = '%s' AND
      (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE date BETWEEN '%s' AND '%s'
        GROUP BY squirrel_id);",
    grid_choice, start_date, end_date)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    census <- tbl(con, "census") %>%
      filter_(~ gr == grid_choice,
              ~ census_date == end_date) %>%
      select_("squirrel_id",
              census_reflo = "reflo",
              census_fate = "sq_fate") %>%
      collect()
    recent <- krsp_sql(con, recent_query)
  })
  # remove possible duplicates in trapping
  recent <- recent %>%
    group_by_("squirrel_id") %>%
    filter_(~ id == max(id)) %>%
    ungroup()
  results <- left_join(recent, census, by = "squirrel_id")

  # clean up
  results <- results %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id),
            locx = ~ loc_to_numeric(locx),
            locy = ~ suppressWarnings(round(as.numeric(locy), 1)),
            trap_date = ~ suppressWarnings(as.Date(lubridate::ymd(trap_date))),
            color_left = ~ ifelse(is.na(color_left) | color_left == "",
                                  "-", color_left),
            color_right = ~ ifelse(is.na(color_right) | color_right == "",
                                   "-", color_right),
            taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
            tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
            colours = ~ paste(color_left, color_right, sep = "/"),
            tags = ~ paste(taglft, tagrt, sep = "/"),
            sex = ~ factor(coalesce(sex, "?"),
                           levels = c("F", "M", "?")),
            in_census = ~ !(is.na(census_fate) & is.na(census_reflo))) %>%
    select_("squirrel_id", "colours", "tags",
            "sex", "grid", "trap_date",
            "locx", "locy", "in_census", "census_reflo", "census_fate") %>%
    arrange_("in_census", "locx", "locy")
  return(results)
}
