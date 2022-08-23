test_that("krsp_connect() hits the database", {
  expect_s4_class(
    krsp_connect(group = 'krsp-aws'),
    "MySQLConnection"
  )
})
