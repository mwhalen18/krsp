test_that("krsp_litter_lookup() returns a data frame", {
  con = krsp_connect(group = 'krsp-aws')
  expect_type(
    krsp_litter_lookup(con, 2019, 19984, ln = 1),
    'list'
  )
  DBI::dbDisconnect(con)
})
