---
title: "Querying MySQL Databases in R"
output:
  html_document:
    toc: true
    toc_depth: 2
    theme: united
vignette: >
  %\VignetteIndexEntry{Querying MySQL Databases in R}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



In this document, I'll explore a variety of ways of querying the squirrel database both directly via SQL queries within Navicat and through R. The idea is to come up with the best approach for this using the database both in and out of the field.

As an example, I'll consider a simple task: generate a "To Do" list of squirrels that appear in the litter table for a given year but don't have breeding codes filled in. The context is that every female squirrel caught in a given year must have a litter record with a valid breeding code. Since the field crew often forgets to fill in the breeding code for non-breeders, this list of squirrels would help remind them to confirm the status of these non-breeders.

# SQL Only

## Simple SQL Query

The simplest approach here is a plain SQL query, which would need to be run in Navicat each time this list needs to be generated. The query I've created returns a list of squirrels with grid, ID, colours, tags, and last trap date and location.

```sql
SET @year = 2015;

SELECT
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
  AND l.yr = @year
ORDER BY
  s.gr, s.trap_date;
```

Usage would typically involve:

1. Saving this code as a query in Navicat.
2. Open query
3. Set year variable as desired. 
4. Executing query.

Results will show up in the Results window within Navicat and likely be exported to text or Excel. The downside to this approach is that Navicat queries are not stored in the database, they're stored within that copy of Navicat. Therefore a library of .sql text files would have to be kept somewhere and these queries would have to be imported into each new copy of Navicat. Also, the queries are only accessible via Navicat, other tools that may interact with the database can't use them.

## Stored Procedure

MySQL has two methods of storing user defined, reusable chunks of code:

-  **Stored Functions** return a single value and are invoked within an SQL expression. For example, `SELECT my_function(my_variable) FROM my_table`, where the function `my_function()` takes the column `my_variable` from `my_table` as input, and returns some value.
-  **Stored Procedures** do not return values and are invoked via `CALL my_procedure(x, y)`, where x and y are parameters, and they perform some operation like modifying a table or retrieving records. They cannot be invoked within an SQL expression.

In both cases, the stored routines are saved directly in the MySQL database so as the database is moved around or backed up, these stored procedures go along with it.

For the task at hand, i.e. return a list of female squirrels without breeding statuses in a given year, a stored procedure is appropriate. The following code defines the stored procedure within the database:  

```sql
DELIMITER //

DROP PROCEDURE IF EXISTS no_br_status //

CREATE PROCEDURE no_br_status(IN focal_year INT(4))
BEGIN

  SELECT
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
    AND l.yr = focal_year
  ORDER BY
    s.gr, s.trap_date;

END //

DELIMITER ;
```

This stored procedure is called as follows:

```sql
CALL no_br_status(2015);
```

This approach is clearly much more flexible and portable than the previous approach using a straight SQL query.

# R Interface to MySQL

There are a couple approaches to working with MySQL databases in R. Both have the benefit of 
1. the power and flexibility of R, a more full-featured programming language than SQL 
2. Seamless integration with other R code. Presumably most queries will eventually end up in R for analysis, so why not avoid exporting from MySQL and importing to R.

## `RMySQL`

The R package `DBI` provides a interface for connecting to databases in R, and `RMySQL` provides the drivers and functionality specific to MySQL databases. Interacting with a MySQL database requires defining a MySQL driver, setting up a connection to a database using that driver, sending queries to the database, and finally closing the connection. Here's a simple example of connecting to a local instance of the KRSP database and listing the existing tables.


```r
library(RMySQL)
drv <- MySQL()
db <- dbConnect(drv, host = "localhost", user = "root", dbname = "krsp")
dbListTables(db)
##  [1] "FLastAll"              "behaviour"            
##  [3] "census"                "dbaAdmums"            
##  [5] "dbaBehaviour"          "dbaFLastAll"          
##  [7] "dbaJuvenile"           "dbaMidden"            
##  [9] "dbaTrapping"           "female_year"          
## [11] "historic_squirrel_ids" "juvenile"             
## [13] "litter"                "pb_data"              
## [15] "squirrel"              "squirrel_alias"       
## [17] "trapping"
dbDisconnect(db)
## [1] TRUE
```

`dbGetQuery` is used to extract data from a MySQL database and bring it into R as a data frame.


```r
drv <- MySQL()
db <- dbConnect(drv, host = "localhost", user = "root", dbname = "krsp")
res <- dbGetQuery(db, "SELECT reflo, squirrel_id FROM census WHERE gr = 'JO' LIMIT 5")
dbDisconnect(db)
## [1] TRUE
knitr::kable(res)
```



|reflo | squirrel_id|
|:-----|-----------:|
|H8.   |       11260|
|G.9   |       11260|
|D0.   |       13203|
|C1.   |       12493|
|D3    |       11816|

To apply this to the task of finding with females without a breeding status, I wrap this into an R function.


```r
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
```



|gr |    id|colorlft |colorrt |taglft |tagrt |locx |locy |trap_date  |
|:--|-----:|:--------|:-------|:------|:-----|:----|:----|:----------|
|AG | 19939|Y        |W       |H2309  |H2310 |P.8  |15.9 |2015-06-01 |
|AG | 13267|B        |B       |H1562  |H1563 |I.8  |21.5 |2015-06-10 |
|AG | 20005|OY       |G       |H3589  |H3590 |S.5  |4.0  |2015-06-12 |
|AG | 20370|Bk       |O       |H4491  |H4490 |J.5  |4.4  |2015-06-13 |
|AG | 20670|B        |Bk      |H4853  |H4854 |T.0  |21.9 |2015-06-13 |
|AG | 19926|Y        |G       |H3870  |H3871 |G.5  |6.7  |2015-06-13 |

## `dplyr`

The `dplyr` package provides a set of tools for intuitively and efficiently manipulating datasets in R. It is typically used with data frames, but can also work with databases (such as MySQL) directly. `dplyr` calls `RMySQL` under the hood, but works at a higher level of abstraction.

Most of the `dplyr` functions (`select`, `filter`, `group_by`, etc.) work the same whether you're using data frames or database tables. The only two additional functions required to get `dplyr` working with MySQL are `src_mysql()`, which defines the connection to the database, and `tbl()`, which connects to a given table within that database.


```r
library(dplyr)
db <- src_mysql("krsp", host = "localhost", user = "root")
tbl(db, "census")
```

```
## Source: mysql 5.7.9 [root@localhost:/krsp]
## From: census [5,379 x 19]
## 
##       id version census_date colorlft colorrt comments        date_created
##    (dbl)   (dbl)       (chr)    (chr)   (chr)    (chr)               (chr)
## 1      1       0  2013-05-15        -       G       NA 2013-04-16 20:12:54
## 2      2       1  2013-05-15        -       G       NA 2013-04-16 20:13:38
## 3      3       0  2013-05-15        R       B       NA 2013-04-16 20:16:02
## 4      4       0  2013-05-15       G!      B!       NA 2013-04-16 20:22:02
## 5      5       0  2013-05-15        O       R       NA 2013-04-16 20:25:01
## 6      6       1  2013-05-15        Y       P       NA 2013-04-16 20:28:36
## 7      8       0  2013-05-15        Y       B       NA 2013-04-16 20:34:10
## 8     10       0  2013-05-15        -      G!       NA 2013-04-16 20:38:39
## 9     11       0  2013-05-15        -      G!       NA 2013-04-16 20:39:20
## 10    12       0  2013-05-15        B       R       NA 2013-04-16 20:40:58
## ..   ...     ...         ...      ...     ...      ...                 ...
## Variables not shown: gr (chr), last_updated (chr), locx (chr), locy (chr),
##   obs (chr), reflo (chr), sex (chr), sq_fate (int), squirrel_id (dbl),
##   taglft (chr), tagrt (chr), trap (chr)
```

Applying this to the example task:


```r
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
```



|gr |    id|colorlft |colorrt |taglft |tagrt |locx |locy |trap_date  |
|:--|-----:|:--------|:-------|:------|:-----|:----|:----|:----------|
|AG | 19939|Y        |W       |H2309  |H2310 |P.8  |15.9 |2015-06-01 |
|AG | 13267|B        |B       |H1562  |H1563 |I.8  |21.5 |2015-06-10 |
|AG | 20005|OY       |G       |H3589  |H3590 |S.5  |4.0  |2015-06-12 |
|AG | 20370|Bk       |O       |H4491  |H4490 |J.5  |4.4  |2015-06-13 |
|AG | 20670|B        |Bk      |H4853  |H4854 |T.0  |21.9 |2015-06-13 |
|AG | 19926|Y        |G       |H3870  |H3871 |G.5  |6.7  |2015-06-13 |

### Laziness

One nice feature of `dplyr` is that it tries to be as "lazy" as possible when dealing with databases. This means:

- Data are never pulled from the database unless explicitly asked for, e.g. when printing
- The database is only queried at the last minute possible, `dplyr` keeps track of all the intermediate operations and only converts them to SQL and queries the database when results are explicitly requested
- Only the number of rows explicitly needed are pulled from the database

This is nice because it means queries to the database and pulling of data are kept to an absolute minimum. While `dplyr` documentation calls this behaviour "lazy", I'm inclined to just call it "smart".
