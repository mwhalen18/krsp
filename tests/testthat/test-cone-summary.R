con <- krsp_connect(group = 'krsp-aws')

test_that("cone_count() returns a MySQL query", {
  expect_s3_class(
    cone_count(con),
    'tbl_MySQLConnection'
  )
})

test_that("cone_count() returns all grids", {
  grids = tbl(con, 'cones') %>% pull(Grid) %>% unique() %>% sort()
  suppressWarnings({
    df <- cone_count(con) %>% pull(Grid) %>% unique() %>% sort()
    expect_equal(length(grids), length(df))
  })
})

test_that("annual_cone_counts() returns single row table", {
  suppressWarnings({
    df <- annual_cone_counts(con, years = 2019) %>% 
      collect()
    expect_equal(
      nrow(df),
      1
    )
  })
})

test_that("annual_cone_counts() returns a multi row table", {
  suppressWarnings({
    years <- c(2012, 2018, 2019)
    df <- annual_cone_counts(con, years)  %>% 
      collect()
    expect_equal(nrow(df), length(years))
  })
})

test_that("annual_cone_counts() returns all years", {
  suppressWarnings({
    years = tbl(con, 'cones') %>% pull(Year) %>% unique()
    df <- annual_cone_counts(con) %>% 
      collect()
    expect_equal(nrow(df), length(years))
  })
})

test_that("cones_grids_years() returns all grids", {
  suppressWarnings({
    grids = tbl(con, 'cones') %>% pull(Grid) %>% unique() %>% sort()
    
    df <- cones_grids_years(con)
    df_grids = df %>% pull(Grid) %>% unique()
    expect_equal(grids, df_grids)
  })
})

test_that("cones_grids_years() returns all years", {
  suppressWarnings({
    years = tbl(con, 'cones') %>% pull(Year) %>% unique() %>% as.numeric()
    
    df <- cones_grids_years(con)
    df_years = df %>% pull(Year) %>% unique() %>% sort()
    expect_equal(df_years, years)
  })
})


test_that("cones_grids_years() returns single grid and single year", {
  suppressWarnings({
    df <- cones_grids_years(con, "KL", 2019) %>% 
      collect()
    expect_equal(nrow(df), 1)
  })
})

DBI::dbDisconnect(con)
