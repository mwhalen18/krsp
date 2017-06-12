## ----options, include=F--------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
options(knitr.table.format = 'markdown')

## ----rmysql, collapse=TRUE-----------------------------------------------
library(RMySQL)
drv <- MySQL()
db <- dbConnect(drv, host = "localhost", user = "root", dbname = "krsp")
dbListTables(db)
dbDisconnect(db)

## ----send-query, collapse=TRUE-------------------------------------------
drv <- MySQL()
db <- dbConnect(drv, host = "localhost", user = "root", dbname = "krsp")
res <- dbGetQuery(db, "SELECT reflo, squirrel_id FROM census WHERE gr = 'JO' LIMIT 5")
dbDisconnect(db)
knitr::kable(res)

## ----query-females-------------------------------------------------------
no_br_status <- function(year, dbname = "krsp", user = "root", ...) {
  drv <- MySQL()
  db <- dbConnect(drv, dbname = dbname, user = user, ...)
  
  sql <- sprintf(
    "SELECT
      s.gr,
      s.id,
      s.colorlft, s.colorrt,
      s.taglft, s.tagrt,
      s.locx, s.locy,
      s.trap_date
    FROM
      litter                AS l
      INNER JOIN squirrel   AS s
        ON l.squirrel_id = s.id
    WHERE
      l.br IS NULL
      AND l.yr = %i
    ORDER BY
      s.gr, s.trap_date;",
    year)
  
  res <- dbGetQuery(db, sql)
  dbDisconnect(db)
  return(res)
}
res <- no_br_status(2015)
knitr::kable(head(res))

## ----dplyr---------------------------------------------------------------
library(dplyr)
db <- src_mysql("krsp", host = "localhost", user = "root")
tbl(db, "census")

## ------------------------------------------------------------------------
no_br_status_dplyr <- function(year, dbname = "krsp", ...) {
  db <- src_mysql("krsp", ...)
  
  litter <- tbl(db, "litter") %>% 
    select(squirrel_id, br, yr)
  squirrel <- tbl(db, "squirrel")
  inner_join(litter, squirrel, by = c("squirrel_id" = "id")) %>% 
    filter(is.null(br), yr == year) %>% 
    arrange(gr, trap_date) %>% 
    select(gr,
           id,
           colorlft, colorrt,
           taglft, tagrt,
           locx, locy,
           trap_date)
}
res <- no_br_status_dplyr(2015)
knitr::kable(head(res))

