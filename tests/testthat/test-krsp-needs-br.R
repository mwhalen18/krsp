test_that("krsp_needs_br() returns a data frame", {
  con = krsp_connect(group = 'krsp-aws')
  expect_type(
    krsp_needs_br(con, 2019),
    'list'
  )
  DBI::dbDisconnect(con)
})
