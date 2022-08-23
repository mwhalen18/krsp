con <- krsp_connect(group = 'krsp-aws')
test_that("check_behavior_loc() returns a data frame", {
  expect_s3_class(
    check_behaviour_loc(con, "KL", 2019, "MRW"),
    'data.frame'
  )
})

test_that("check_behavior_loc() returns a message of success", {
  expect_message(
    check_behaviour_loc(con, "KL", 2019, "AAA"),
  )
})

test_that("check_behavior_mode() returns a data frame", {
  expect_s3_class(
    check_behaviour_loc(con, "KL", 2019, "MRW"),
    'data.frame'
  )
})

test_that("check_behavior_mode() returns a message of success", {
  expect_message(
    check_behaviour_loc(con, "KL", 2019, "AAA"),
  )
})

test_that("check_behavior_time() returns a data frame", {
  expect_s3_class(
    check_behaviour_loc(con, "KL", 2019, "MRW"),
    'data.frame'
  )
})

test_that("check_behavior_time() returns a message of success", {
  expect_message(
    check_behaviour_loc(con, "KL", 2019, "AAA"),
  )
})

test_that("check_behavior() returns a data frame", {
  expect_s3_class(
    check_behaviour_loc(con, "KL", 2019, "MRW"),
    'data.frame'
  )
})


DBI::dbDisconnect(con)