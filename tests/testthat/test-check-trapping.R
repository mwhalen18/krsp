con = krsp_connect(group = 'krsp-aws')
test_that("chec_trapping.loc() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_loc(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_colors() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_colours(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_tags() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_tags(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_tags() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_tags(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_collwt() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_collwt(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_dna() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_dna(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_newdna() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_newdna(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

test_that("chec_trapping_fate() returns a data frame", {
  expect_type(
    suppressMessages({
      check_trapping_fate(con, 'KL', 2019, 'MRW')
    }),
    'list'
  )
})

DBI::dbDisconnect(con)