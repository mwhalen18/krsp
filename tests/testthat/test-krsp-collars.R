con = krsp_connect(group = 'krsp-aws')
test_that("krsp-collars() returns a data frame", {
  expect_type(
    krsp_collars(con, 'KL', 2019),
    'list'
  )
})
DBI::dbDisconnect(con)