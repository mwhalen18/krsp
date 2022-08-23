con <- krsp_connect(group = 'krsp-aws')
test_that("code_mast_years() works for tbl_MySQL", {
  expect_s3_class(
    cone_count(con) %>% 
      code_mast_years(Grid, Year),
    'tbl_MySQLConnection'
  )
})

test_that("code_mast_years() works for dataframes", {
  expect_s3_class(
    cone_count(con) %>% 
      collect() %>% 
      code_mast_years(Grid, Year),
    'tbl_df'
  )
})
dbDisconnect(con)