con <- krsp_connect(group = 'krsp-aws')

test_that("krsp_sql() returns a query on the krsp database", {
  expect_s3_class(
    krsp_sql(con, 'select * from census'),
    'data.frame'
  )
})
DBI::dbDisconnect(con)