## ----setup, echo=F, message=F--------------------------------------------
library(RMySQL)
library(dplyr)
library(krsp)

## ----rmysql, eval=F------------------------------------------------------
#  db <-  dbConnect(MySQL(), group = "krsp-aws")

## ----src-mysql, eval=F---------------------------------------------------
#  db <- src_mysql(group = "krsp-aws", dbname = NULL, password = NULL, user = NULL)

## ----krsp-connect, eval=F------------------------------------------------
#  db <- krsp_connect(group = "krsp-aws")

