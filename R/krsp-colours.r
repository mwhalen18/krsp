#' Colour key
#'
#' Display a colour key based on the most recent trapping records for a given
#' grid and year.
#'
#' @param con Connection to KRSP database
#' @param grid character; grid to display colour key for.
#' @param year integer; year to display colour key for. Defaults to current year.
#'
#' @return A data frame allowing identification of squirrels by colours. In this
#'   data frame there are three flags: \code{valid} indicates whether the
#'   colours are valid (allowing for at most 2 colours per ear),
#'   \code{duplicate} indicates whether that combination of colours is used on
#'   multiple squirrels, and \code{standard} indicates whether a squirrel has
#'   standard colouring, i.e. one colour in each ear and no mixing of wires,
#'   pipes, and discs.
#'
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_colours(con, grid = "KL", year = 2014)
krsp_colours <- function(con, grid, year) {
  UseMethod("krsp_colours")
}

#' @export
krsp_colours.krsp <- function(con, grid, year = current_year()) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_grid(grid, single = TRUE),
              valid_year(year, single = TRUE))
  year_arg <- as.integer(year)

  # query for most recent trapping record
  recent_query <- sprintf(
    "SELECT
      t.id, t.squirrel_id, t.date, t.ft, s.sex,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy,
      (l.id IS NOT NULL) AS juvenile
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
      LEFT JOIN juvenile j
        ON s.id = j.squirrel_id
      LEFT JOIN litter l
        ON j.litter_id = l.id
        AND YEAR(COALESCE(l.fieldBDate, l.date1, l.tagDt)) = %i
    WHERE
      s.gr = '%s'
      AND (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE YEAR(date) = %i
        GROUP BY squirrel_id);", year_arg, grid, year_arg)
  recent <- krsp_sql(con, recent_query)
  # query for juveniles
  juve_query <- sprintf(
    "SELECT
      j.squirrel_id, l.tagDt AS date, j.sex,
      j.taglft AS taglft, j.tagrt,
      j.color_left, j.color_right,
      l.nx2 AS locx, l.ny2 AS locy,
      TRUE AS juvenile
    FROM
      litter                  l
      INNER JOIN juvenile     j
        ON l.id = j.litter_id
    WHERE
      l.grid = '%s'
      AND l.yr = %i
      AND l.tagDt IS NOT NULL
      AND (j.color_left IS NOT NULL OR j.color_right IS NOT NULL);",
    grid, year_arg)
  juveniles <- krsp_sql(con, juve_query)
  # remove duplicate entries for squirrels
  recent <- recent %>%
    arrange_(~ desc(id)) %>%
    # remove dead squirrels
    filter_(~ ft %in% 1:3) %>%
    group_by_("squirrel_id") %>%
    filter_(~ row_number() == 1) %>%
    ungroup() %>%
    select_(~ -id, ~ -ft)
  # combine litter and trapping data
  trap_lit <- bind_rows(juveniles, recent) %>%
    arrange_(~ desc(date)) %>%
    # only keep most recent between trapping and litter
    group_by_("squirrel_id") %>%
    filter_(~ row_number(desc(date)) == 1) %>%
    ungroup()
  if (!is.data.frame(trap_lit) | nrow(trap_lit) == 0) {
    return(as.tbl(data.frame(

    )))
  }
  results <- trap_lit %>%
    mutate_(
      juvenile = ~ (juvenile == 1),
      color_left = ~ toupper(color_left),
      color_left = ~ gsub("BK", "Bk", color_left),
      color_left = ~ gsub("GY", "Gy", color_left),
      color_left = ~ ifelse(is.na(color_left) | color_left == "",
                            "-", color_left),
      color_right = ~ toupper(color_right),
      color_right = ~ gsub("BK", "Bk", color_right),
      color_right = ~ gsub("GY", "Gy", color_right),
      color_right = ~ ifelse(is.na(color_right) | color_right == "",
                             "-", color_right),
      taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
      tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
      locx = ~ ifelse(is.na(locx) | locx == "", "-", locx),
      locy = ~ ifelse(is.na(locy) | locy == "", "-", locy),
      colours = ~ paste(color_left, color_right, sep = "/"),
      tags = ~ paste(taglft, tagrt, sep = "/"),
      loc = ~ paste(locx, locy, sep = "/"),
      reflo = ~ as_reflo(locx, locy)) %>%
    rename_(left = "color_left", right = "color_right")
  # find duplicate coloured squirrels
  results <- results %>%
    group_by_("left", "right") %>%
    mutate_(duplicate = ~ (n() > 1)) %>%
    ungroup()
  # identify non-standard and bad colours
  # replace two letter colours
  results <- results %>%
    # identify invalid colours
    mutate_(valid = ~ grepl("^(([BRGYOWP]|Bk|Gy)[!*]?){1,2}$", left) | left == "-",
            valid = ~ valid & (grepl("^(([BRGYOWP]|Bk|Gy)[!*]?){1,2}$", right) | right == "-")) %>%
    # non-standard colours combos
    mutate_(stdf = ~ grepl("^([BRGYOWP]|Bk|Gy)$", left) | left == "-",
            stdf = ~ stdf & (grepl("^([BRGYOWP]|Bk|Gy)$", right) | right == "-"),
            stdm = ~ grepl("^([BRGYOWP]|Bk|Gy)[!]$", left) | left == "-",
            stdm = ~ stdm & (grepl("^([BRGYOWP]|Bk|Gy)[!]$", right) | right == "-"),
            stdj = ~ grepl("^([BRGYOWP]|Bk|Gy)[*]$", left) | left == "-",
            stdj = ~ stdj & (grepl("^([BRGYOWP]|Bk|Gy)[*]$", right) | right == "-"),
            standard = ~ (sex == "M" & !juvenile & stdm),
            standard = ~ standard | (sex == "F" & !juvenile & stdf),
            standard = ~ standard | (juvenile & stdj),
            standard = ~ coalesce(standard, FALSE)) %>%
    select_("left", "right", "squirrel_id", "sex", "juvenile",
            "tags", "colours", "loc", "reflo",
            last_trapped = "date", "standard", "duplicate", "valid") %>%
    arrange_("juvenile", "sex", "left", "right")
  as.tbl(results)
}
