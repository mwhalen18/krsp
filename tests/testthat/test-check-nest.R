con = krsp_connect(group = 'krsp-aws')
test_that("check_nest.loc() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_loc(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest.n1() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_n1loc(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest.n2() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_n2loc(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest.weight() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_weight(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest.dna() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_dna(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest.notch() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_notch(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest.grid() returns a data.frame", {
  expect_type( suppressMessages({
    check_nest_grid(con, 'KL', 2019) }),
    'list'
  )
})

test_that("check_nest returns a data.frame", {
  expect_type( suppressMessages({
    check_nest(con, 'KL', 2019) }),
    'list'
  )
})


DBI::dbDisconnect(con)
