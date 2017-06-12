#' Display seasonal data collection progress
#'
#' Display an interactive visualization of data collection progress for the
#' season. All adult females caught in the given year are inlcuded provided
#' their most recent trapping record has fate 1-3.
#'
#' The status of a female is based on the most recent trapping record and the
#' litter table. If the most recent trapping record is dated prior to the date
#' the litter table was updated it is ignored, and only the litter table is
#' used. If nipple condition is 5, the status is LL (Lost Litter). Otherwise,
#' status is Parturition, N1, or Completed if fieldBDate, date1, or tagDt fields
#' in litter table are filled in, respectively. Finally, if litter table dates
#' are empty, rep_con field in trapping record is used and status is P0, P1, P2,
#' or P3 is assigned for rep_con = 1, 2, 3, 4, respectively.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; defaults to current year
#' @param data logical; if TRUE return data frame instead of plotting
#'
#' @return Displays and returns a \code{ggvis} plot of seasonal workflow
#'   progress for all females, unless \code{data} is TRUE, in which case a data
#'   frame is returned and nothing is plotted.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_progress(con, "JO", 2015, data = TRUE) %>%
#'   head()
#' krsp_progress(con, "KL", 2011)
krsp_progress <- function(con, grid, year, data) {
  UseMethod("krsp_progress")
}

#' @export
krsp_progress.krsp <- function(con, grid, year = current_year(), data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE),
              valid_grid(grid, single = TRUE))

  year <- as.integer(year)
  grid_choice <- grid

  # query for most recent trapping record
  female_query <- sprintf(
    "SELECT
      t.squirrel_id, t.date AS trap_date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy,
      t.ft, t.rep_con, t.nipple
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
    WHERE
      s.sex = 'F'
      AND s.gr = '%s'
      AND t.rep_con IS NOT NULL
      AND (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE YEAR(date) = %i AND rep_con IS NOT NULL
        GROUP BY squirrel_id)
      AND t.squirrel_id NOT IN (
        SELECT j.squirrel_id
        FROM JUVENILE     j
        LEFT JOIN LITTER  l
          ON j.litter_id = l.id
        WHERE
          YEAR(COALESCE(fieldBDate, date1, tagDt)) = %i
          AND GRID = '%s'
        );", grid_choice, year, year, grid_choice)
  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    females <- krsp_sql(con, female_query)
    litter <- tbl(con, "litter") %>%
      filter_(~ yr == year) %>%
      select_("id", "squirrel_id", "br", "ln",
              "fieldBDate", "date1", "tagDt") %>%
      collect()
  })

  # remove multiple trapping records from same date
  females <- females %>%
    # remove dead squirrels
    filter_(~ ft %in% 1:3) %>%
    group_by_("squirrel_id") %>%
    filter_(~ row_number() == 1) %>%
    ungroup()

  # bring in litter data
  rep_con_map <- c("P0", "P1", "P2", "P3")
  females <- left_join(females, litter, by = "squirrel_id") %>%
    arrange_("squirrel_id", "ln") %>%
    # sort out the various permutations of data - messy!
    mutate_(
      trap_date = ~ suppressWarnings(as.Date(lubridate::ymd(trap_date))),
      ln = ~ as.character(ifelse(ln %in% 1:3, ln, NA)),
      ln = ~ ifelse(is.na(id), "-", ln),
      litter_date = ~ pmax(fieldBDate, date1, tagDt, na.rm = TRUE),
      litter_date = ~ suppressWarnings(as.Date(lubridate::ymd(litter_date))),
      litter_status = ~ ifelse(!is.na(tagDt), "N2",
                             ifelse(!is.na(date1), "N1",
                                    ifelse(!is.na(fieldBDate), "Parturition", NA))),
      # use breeding status to assess non-breeders and lost litters
      litter_status = ~ ifelse(br == 0 & is.na(litter_status), "Non-breeder",
                             litter_status),
      litter_status = ~ ifelse(br %in% c(2, 4, 7) & litter_status != "N2",
                             "LL", litter_status),
      trap_status = ~ ifelse(!is.na(nipple) & nipple == 5, "LL",
                             ifelse(is.na(rep_con) | !rep_con %in% 1:4, NA,
                                    rep_con_map[rep_con])),
      # nest record more recent than trap record
      status = ~ ifelse(is.na(litter_status), trap_status, litter_status),
      # if trap later than nest and lost litter or pregnant, takes precedence
      trap_precedence = ~ (is.na(litter_date) | litter_date < trap_date),
      status = ~ ifelse(trap_precedence & trap_status != "P0", trap_status, status),
      # status = ~ ifelse(trap_precedence & !is.na(litter_date) & litter_status == "N2",
      #                   trap_status, status),
      status = ~ factor(status,
                        levels = c("P3", "P2", "P1",
                                   "Parturition", "N1",
                                   "LL", "P0", "N2", "Non-breeder")),
      completion = ~ ifelse(status %in% c("LL", "N2", "P0", "Non-breeder"),
                            as.character(status), "In Progress"),
      completion = ~ factor(completion,
                            levels = c("In Progress", "LL", "P0", "Non-breeder",
                                       "N2")),
      # convert trap and litter status to factor
      trap_status = ~ factor(trap_status, levels = c(rev(rep_con_map), "LL")),
      litter_status = ~ factor(litter_status,
                               levels = c("Parturition", "N1", "N2",
                                          "LL", "Non-breeder"))) %>%
    # prepare tags, colours, and locs
    mutate_(
      color_left = ~ ifelse(is.na(color_left) | color_left == "",
                            "-", color_left),
      color_right = ~ ifelse(is.na(color_right) | color_right == "",
                             "-", color_right),
      taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
      tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
      locx = ~ ifelse(is.na(locx) | locx == "", "-", locx),
      locy = ~ ifelse(is.na(locy) | locy == "", "-", locy),
      colours = ~ paste(color_left, color_right, sep = "/"),
      tags = ~ paste(taglft, tagrt, sep = "/"),
      loc = ~ paste(locx, locy, sep = "/"))
  # target trap date
  females <- females %>%
    mutate_(target_trap_date = ~ next_trap(as.character(status), trap_date))
  # sensible ordering
  females <- females %>%
    group_by_("squirrel_id") %>%
    summarize_(arr_comp = ~ min(as.integer(completion), na.rm = TRUE),
              arr_status = ~ min(as.integer(status), na.rm = TRUE)) %>%
    inner_join(females, by = "squirrel_id") %>%
    arrange_("arr_comp", "arr_status", "squirrel_id", "ln") %>%
    select_("squirrel_id", "tags", "colours", "loc", litter_number = "ln",
           "status", "litter_status", "litter_date",
           "trap_status", "trap_date", "target_trap_date")

  # return raw data frame or DataTable
  if (data) {
    return(females)
  } else {
    progress_datatable(females)
  }
}

progress_datatable <- function(df) {
  col_names <- c("ID", "Tags", "Colours", "Loc", "Litter",
                 "Status", "Litter Status", "Litter Date",
                 "Trap Status", "Last Trapped", "Trap By")
  dt <- DT::datatable(df,
                      rownames = FALSE,
                      colnames = col_names,
                      class = "nowrap stripe compact",
                      options = list(
                        paging = FALSE,
                        searching = FALSE,
                        info = FALSE,
                        columnDefs = list(list(className = 'dt-center',
                                               targets = c(2:10)))))
  # highlight based on status
  clr_lvl <- c("LL", "Non-breeder",
               "P0", "P1", "P2", "P3",
               "Parturition", "N1", "N2")
  clr_bg <- c("#555555", "#FFFFFF",
              "#FFFFFF", "#4DAF4A", "#4DAF4A", "#4DAF4A",
              "#E41A1C", "#FF7F00", "#377EB8")
  clr_txt <- c("#FFFFFF", "#000000",
               "#000000", "#FFFFFF", "#FFFFFF", "#FFFFFF",
               "#FFFFFF", "#FFFFFF", "#FFFFFF")
  dt <- dt %>%
    DT::formatStyle("status",
                    color = DT::styleEqual(clr_lvl, clr_txt),
                    textAlign = "center",
                    fontWeight = "bold",
                    backgroundColor = DT::styleEqual(clr_lvl, clr_bg))
  return(dt)
}
