#' code mast years by year
#' @usage `code_mast_years(.tbl, .gridCol, .yearCol)`
#' @param .tbl A dataframe to calculate mast years
#' @param .gridCol a column to extract grid names from. Can accept standard and non-standard evaluated column names
#' @param .yearCol a column to extract years from. Can accept standard and non standard evaluated column names
#' @details The output of `code_mast_years()` will contain columns 'mast', 'Exp', and 'EXP_label'
#' Returns a `data.frame` or `tbl_df` object passed as an argument as `.tbl`
#' @examples
#' \dontrun {
#' con <- krsp_connect(group = 'krsp-aws')
#' tbl(con, 'cones') %>% 
#' code_mast_years(Grid, Year)
#' } 
#' @export
code_mast_years <- function(.tbl, .gridCol, .yearCol) {
  # This should allow the fx to work on both db_sql tbls and dplyr tbls
  # TODO: write test for both dbplyr and dplyr and dataframe forms
  .tbl %>% 
    mutate(mast = case_when(
      ({{.gridCol}} %in% c("KL", "LL", "SU")) && ({{.yearCol}} == 1993) ~ "y",
      ({{.gridCol}} %in% c("KL", "LL", "SU")) && ({{.yearCol}} == 1998) ~ "y",
      ({{.gridCol}} %in% c("KL", "JO", "SU")) && ({{.yearCol}} == 2005) ~ "y",
      ({{.gridCol}} %in% c("KL", "LL", "SU", "CH", "JO", "AG")) && ({{.yearCol}}) == 2010 ~ "y",
      ({{.gridCol}} %in% c("KL", "LL", "SU", "CH", "JO", "AG")) && ({{.yearCol}}) == 2014 ~ "y",
      ({{.gridCol}} %in% c("KL", "LL", "SU", "CH", "JO", "AG")) && ({{.yearCol}}) == 2019 ~ "y",
      TRUE ~ "n"
    )) %>% 
    mutate(Exp = case_when(
      ({{.gridCol}} == "AG") && ({{.yearCol}} >2004) && ({{.yearCol}} < 2018) ~ "f",
      ({{.gridCol}} == "JO") && ({{.yearCol}} >2006) && ({{.yearCol}} < 2013) ~ "f",
      ({{.gridCol}} == "LL") && ({{.yearCol}} >2005) && ({{.yearCol}} < 2012) ~ "f",
      TRUE ~ "c"
    )) %>% 
    mutate(EXP_label = case_when(
      Exp == "f" ~ 19,
      TRUE ~ 1
    ))
}
