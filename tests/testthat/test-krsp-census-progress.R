con <- krsp_connect(group = 'krsp-aws')
test_that("krsp-census-progress() returns a data frame", {
  expect_type(
    suppressMessages({
      krsp_census_progress(con, 'KL', 2019, 'august')
    }),
    'list'
  )
})
DBI::dbDisconnect(con)
