#' Display seasonal data collection progress
#'
#' Display an interactive visualization of data collection progress for the 
#' season. All adult females and males caught in the given year are inlcuded
#' provided their most recent trapping record has fate 1-3.
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
#' Males are assigned status "S" or "A" for scrotal or abdominal.
#'
#' @param con Connection to KRSP database
#' @param grid character; one or more grids to search. Defaults to all grids.
#' @param year integer; defaults to current year
#' @param sex character; whether to return females, males, or both.
#' @param data logical; if TRUE return data frame instead of plotting
#'
#' @return Displays and returns a `ggvis` plot of seasonal workflow 
#'   progress for all squirrels, unless `data` is TRUE, in which case a
#'   data frame is returned and nothing is plotted.
#' @export
#' @examples
#' \dontrun{
#' con <- krsp_connect()
#' krsp_progress(con, "JO", 2015, data = TRUE) %>%
#'   head()
#' krsp_progress(con, "KL", 2011)
#' }

#' @export
krsp_progress <- function(con, grid, year = current_year(), 
                               sex = c("F", "M", "Both"), data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "MySQLConnection"),
              valid_year(year, single = TRUE),
              missing(grid) || valid_grid(grid))
  
  year <- as.integer(year)
  sex = match.arg(sex)
  
  # query for most recent trapping record
  if (missing(grid)) {
    grid_str <- ""
  } else {
    grid_str <- paste0("AND s.gr IN ('", paste(grid, collapse = "','"), "')")
  }
  sq_query <- sprintf(
    "SELECT
      s.gr AS grid, squirrel_id, s.sex, t.date AS trap_date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      t.locx, t.locy,
      t.ft, t.rep_con, t.nipple
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
    WHERE
      t.rep_con IS NOT NULL
      %s
      AND (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE YEAR(date) = %i AND rep_con IS NOT NULL
        GROUP BY squirrel_id)
      AND t.squirrel_id NOT IN (
        SELECT j.squirrel_id
        FROM juvenile     j
        LEFT JOIN litter  l
          ON j.litter_id = l.id
        WHERE
          YEAR(COALESCE(fieldBDate, date1, tagDt)) = %i
          %s
        );", grid_str, year, year, grid_str)
  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    squirrels <- krsp_sql(con, sq_query)
    litter <- tbl(con, "litter") %>%
      filter( yr == year) %>%
      select("id", "squirrel_id", "br", "ln",
              "fieldBDate", "date1", "tagDt") %>%
      collect()
  })
  
  # remove multiple trapping records from same date
  squirrels <- squirrels %>%
    # remove dead squirrels
    filter( ft %in% 1:3) %>%
    group_by(squirrel_id) %>%
    filter( row_number() == 1) %>%
    ungroup() %>% 
    # prepare tags, colours, and locs
    mutate(
      trap_date =  suppressWarnings(as.Date(lubridate::ymd(trap_date))),
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
      loc =  paste(locx, locy, sep = "/"))
      
  # split male and female
  females <- filter(squirrels,  sex == "F")
  males <- filter(squirrels,  sex == "M")
  
  # bring in litter data for females
  rep_con_map <- c("P0", "P1", "P2", "P3")
  females <- left_join(females, litter, by = "squirrel_id") %>%
    arrange(squirrel_id, ln) %>%
    # sort out the various permutations of data - messy!
    mutate(
      ln =  as.character(ifelse(ln %in% 1:3, ln, NA)),
      ln =  ifelse(is.na(id), "-", ln),
      litter_date =  pmax(fieldBDate, date1, tagDt, na.rm = TRUE),
      litter_date =  suppressWarnings(as.Date(lubridate::ymd(litter_date))),
      litter_status =  ifelse(!is.na(tagDt), "N2",
                               ifelse(!is.na(date1), "N1",
                                      ifelse(!is.na(fieldBDate), "Parturition", NA))),
      # use breeding status to assess non-breeders and lost litters
      litter_status =  ifelse(br == 0 & is.na(litter_status), "Non-breeder",
                               litter_status),
      litter_status =  ifelse(br %in% c(2, 4, 7) & litter_status != "N2",
                               "LL", litter_status),
      trap_status =  ifelse(!is.na(nipple) & nipple == 5, "LL",
                             ifelse(is.na(rep_con) | !rep_con %in% 1:4, NA,
                                    rep_con_map[rep_con])),
      # nest record more recent than trap record
      status =  ifelse(is.na(litter_status), trap_status, litter_status),
      # if trap later than nest and lost litter or pregnant, takes precedence
      trap_precedence =  (is.na(litter_date) | litter_date < trap_date),
      status =  ifelse(trap_precedence & trap_status != "P0", trap_status, status),
      # status =  ifelse(trap_precedence & !is.na(litter_date) & litter_status == "N2",
      #                   trap_status, status),
      completion =  ifelse(status %in% c("LL", "N2", "P0", "Non-breeder"),
                            as.character(status), "In Progress")) %>%
    select("grid", "squirrel_id", "sex", "tags", "colours", "loc", 
            litter_number = "ln",
            "status", "litter_status", "litter_date",
            "trap_status", "trap_date", "completion")
  
  # process males
  rep_con_map <- c("S", "A")
  males <- males %>% 
    mutate(litter_number =  NA, litter_status =  NA, litter_date = NA,
            completion =  "Male",
            trap_status =  rep_con_map[rep_con],
            status =  trap_status) %>%
    select("grid", "squirrel_id", "sex", "tags", "colours", "loc", 
            "litter_number", "status", "litter_status", "litter_date",
            "trap_status", "trap_date", "completion")
  
  squirrels <- bind_rows(females, males) %>% 
    mutate(
      status =  factor(status,
                        levels = c("P3", "P2", "P1",
                                   "Parturition", "N1",
                                   "LL", "P0", "N2", "Non-breeder",
                                   "S", "A")),
      trap_status =  factor(trap_status, levels = c(rev(rep_con_map), 
                                                     "LL")),
      completion =  factor(completion,
                            levels = c("In Progress", "LL", "P0", "Non-breeder",
                                       "N2", "Male")),
      litter_status =  factor(litter_status,
                               levels = c("Parturition", "N1", "N2",
                                          "LL", "Non-breeder")))
  
  # target trap date
  squirrels <- squirrels %>%
    mutate(target_trap_date =  next_trap(as.character(status), trap_date))
  # sensible ordering
  squirrels <- squirrels %>%
    group_by(squirrel_id) %>%
    summarize(arr_comp =  min(as.integer(completion), na.rm = TRUE),
               arr_status =  min(as.integer(status), na.rm = TRUE)) %>%
    inner_join(squirrels, by = "squirrel_id") %>%
    arrange(arr_comp, arr_status, squirrel_id, litter_number) %>%
    select("grid", "squirrel_id", "sex", "tags", "colours", "loc", 
            "litter_number", "status", "litter_status", "litter_date",
            "trap_status", "trap_date", "target_trap_date")
  
  if (sex == "F") {
    squirrels <- filter(squirrels,  sex == "F")
  } else if (sex == "M") {
    squirrels <- filter(squirrels,  sex == "M")
  }
  
  # return raw data frame or DataTable
  if (data) {
    return(squirrels)
  } else {
    progress_datatable(squirrels)
  }
}

progress_datatable <- function(df) {
  col_names <- c("Grid", "ID", "Sex", "Tags", "Colours", "Loc", "Litter",
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
               "Parturition", "N1", "N2",
               "A", "S")
  clr_bg <- c("#555555", "#FFFFFF",
              "#FFFFFF", "#4DAF4A", "#4DAF4A", "#4DAF4A",
              "#E41A1C", "#FF7F00", "#377EB8",
              "#FFFFFF", "#FFFFFF")
  clr_txt <- c("#FFFFFF", "#000000",
               "#000000", "#FFFFFF", "#FFFFFF", "#FFFFFF",
               "#FFFFFF", "#FFFFFF", "#FFFFFF",
               "#000000", "#000000")
  dt <- dt %>%
    DT::formatStyle("status",
                    color = DT::styleEqual(clr_lvl, clr_txt),
                    textAlign = "center",
                    fontWeight = "bold",
                    backgroundColor = DT::styleEqual(clr_lvl, clr_bg))
  return(dt)
}
