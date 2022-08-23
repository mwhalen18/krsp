con = krsp_connect(group = 'krsp-aws')
test_that("multiplication works", {
  expect_type(
    krsp_colours(con, 'KL', 2019),
    'list'
  )
})
DBI::dbDisconnect(con)