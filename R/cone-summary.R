#' calculate cone count summaries
#' These functions are helpers to calculate count counts on the krsp grids
#' 
#' * `cone_count()` returns individual stake data
#' * `annual_cone_counts()` computes cone counts by year
#' * `grids_cones_years()` computes cone counts by grid and year
#' 
#' @param con A \code{MySQLConnection} connection to the KRSP database
#' @param grids character; one or more grids to search. Defaults to all grids.
#' @param years integer; one or more years to search within. Defaults to all years
#' @returns A `tbl_MySQLConnection` query that can be gathered into memory using `collect()`
#' @examples 
#' \dontrun {
#' con <- krsp_connect(group = 'krsp-aws')
#' annual_cone_counts(con, years = 2010:2015)
#' cones_grids_years(con, grids = c("KL", "SU"))
#' cones_grids_years(con)
#' cones_grids_years(con, years = 2019, grids = "KL", "SU")
#' }
#' @param ... Reserved for future extensions
#' @export
cone_count <- function(con, ...) {
  cone_counts <- tbl(con, 'cones') %>% 
    mutate(across(
      .cols = c(Year, LocX, LocY, DBH, Per, NumNew),
      .fns = ~.x * 1
    )) %>% 
    mutate(cone_index = log(NumNew + 1),
           total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)))
  return(cone_counts)
}

#' @export
#' @rdname cone_count
annual_cone_counts <- function(con, years, ...) {
  # TODO: write test that check for null, vector, and single year
  if(missing(years)) years = 1980:lubridate::year(Sys.Date())
   cone_count(con) %>% 
      group_by(Year) %>% 
      summarize(
        num_trees = sum(!is.na(NumNew), na.rm = TRUE),
        cone_counts = mean(NumNew, na.rm = TRUE),
        cone_index = mean(cone_index, na.rm = TRUE),
        total_cones = mean(total_cones, na.rm = TRUE)
      ) %>% 
     filter(Year %in% years)
}

#' @export
#' @rdname cone_count
cones_grids_years <- function(con, grids, years, ...) {
  if(missing(grids)) grids = tbl(con, 'cones') %>% pull(Grid) %>% unique() 
  if(missing(years)) years = tbl(con, 'cones') %>% pull(Year) %>% unique() 
  #args = list(...)
  cone_counts = cone_count(con) %>% 
    filter(Grid %in% grids, Year %in% years) %>% 
    group_by(Grid, Year) %>% 
    summarize(
      num_trees = sum(!is.na(NumNew), na.rm = TRUE),
      cone_counts = mean(NumNew, na.rm = TRUE),
      cone_index = mean(cone_index, na.rm = TRUE),
      total_cones = mean(total_cones, na.rm = TRUE),
      .groups = 'drop'
    ) %>% 
    mutate(Year_tp1 = Year + 1,
           cone_index_t = case_when(
             (cone_index < 1e7)|| (cone_index > -1e7) ~ cone_index,
             TRUE ~ NA
           )) %>% 
    arrange(
      Year,
      Grid
    )
  
  cone_counts %>% 
    left_join(
      .,
      cone_counts %>% select(Grid, Year_tp1, cone_index_tm1 = cone_index_t),
      by = c("Grid", "Year" = "Year_tp1")
    ) %>% 
    select(-Year_tp1) %>% 
    code_mast_years(Grid, Year)
}


