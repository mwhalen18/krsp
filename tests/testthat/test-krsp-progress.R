con = krsp_connect(group = 'krsp-aws')
test_that("krsp_litter_lookup() returns a data frame", {
  expect_type(
    krsp_progress(con, grid = 'KL', year = 2019, sex = 'M', data = TRUE),
    'list'
  )
})
DBI::dbDisconnect(con)
