con = krsp_connect(group = 'krsp-aws')
test_that("krsp-censusmap() returns a data frame", {
  expect_type(
    krsp_censusmap(con, 'KL', 2019, census = 'august', data = TRUE),
    'list'
  )
})

test_that("krsp-censusmap() returns a data frame", {
  expect_s3_class(
    krsp_censusmap(con, 'KL', 2019, census = 'august', data = FALSE),
    'ggvis'
  )
})

DBI::dbDisconnect(con)

#test_that("plot_census() returns a ggvis object", {
#  expect_s3_class(
#    plot_
#  )
#})