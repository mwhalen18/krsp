con <- krsp_connect(group = 'krsp-aws')

test_that("check_collars() returns a data frame", {
  expect_type(
    check_collars(con, 'KL', year = 2019),
    'list'
  )
})
