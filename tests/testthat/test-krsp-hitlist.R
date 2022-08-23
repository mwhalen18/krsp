test_that("krsp-hitlist() returns a tibble", {
  con = krsp_connect(group = 'krsp-aws')
  expect_type(
    krsp_hitlist(con, 2019),
    'list'
  )
  DBI::dbDisconnect(con)
})
