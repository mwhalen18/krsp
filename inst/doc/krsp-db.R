## ----options, include=F--------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
options(knitr.table.format = 'markdown')

## ----st-verbs------------------------------------------------------------
library(dplyr)
mtc <- tbl_df(mtcars)
mtc <- select(mtc, mpg, cyl, horse_power = hp)
mtc
filter(mtc, cyl == 4, horse_power < 70)
arrange(mtc, desc(cyl), horse_power)
mutate(mtc, mpg_per_hp = mpg / horse_power)
summarize(mtc, mean_mpg = mean(mpg), num_records = n())

## ----pipe-example--------------------------------------------------------
mean(sqrt(exp(mtc$mpg)))
mtc$mpg %>% 
  exp %>% 
  sqrt %>% 
  mean

## ----pipe-dplyr----------------------------------------------------------
mtc %>% 
  filter(cyl == 4, horse_power < 70) %>% 
  mutate(mpg_per_hp = mpg / horse_power) %>% 
  arrange(mpg_per_hp)

## ----group-by------------------------------------------------------------
mtc %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg))

## ----join-tables---------------------------------------------------------
superheroes <- data.frame(
  name = c("Magneto", "Storm", "Mystique", "Batman", 
           "Joker", "Catwoman", "Hellboy"),
  alignment = c("bad", "good", "bad", "good",
                "bad", "bad", "good"),
  publisher = c("Marvel", "Marvel", "Marvel", "DC", 
                "DC", "DC", "Dark Horse Comics"),
  stringsAsFactors = FALSE
  )
superheroes

publishers <- data.frame(
  publisher = c("DC", "Marvel", "Image"),
  yr_founded = c(1934, 1939, 1992),
  stringsAsFactors = FALSE
  )
publishers

## ----joins---------------------------------------------------------------
# Hellboy (superhero) and Image (publisher) are lost
inner_join(superheroes, publishers)
# Hellboy (superhero) has no yr_founded and Image (publisher) is lost
left_join(superheroes, publishers)
# Hellboy (superhero) is lost and Image (publisher) has no superheros
right_join(superheroes, publishers)

## ----explicit-join-------------------------------------------------------
inner_join(superheroes, publishers, by = "publisher")

## ----diff-join-----------------------------------------------------------
publishers <- rename(publishers, pub = publisher)
inner_join(superheroes, publishers, by = c("publisher" = "pub"))

## ----tbl-----------------------------------------------------------------
library(krsp)
con <- krsp_connect()
(trapping <- tbl(con, "trapping"))

## ----fat-----------------------------------------------------------------
trapping %>% 
  filter(scale_weight > 500) %>% 
  select(squirrel_id, tagLft, tagRt, scale_weight) %>% 
  arrange(scale_weight)

## ------------------------------------------------------------------------
litter <- tbl(con, "litter")
juvenile <- tbl(con, "juvenile")
litter_size <- inner_join(litter, juvenile, by = c("id" = "litter_id")) %>% 
  group_by(grid, litter_id) %>% 
  # count() counts the number of records within each group, stores as variable n
  count() %>% 
  summarize(mean_size = mean(n)) %>% 
  arrange(mean_size)
collect(litter_size)

## ----pure-sql------------------------------------------------------------
sql <- "
  SELECT gr, COUNT(*) AS n_squirrels
  FROM squirrel
  GROUP BY gr 
  ORDER BY n_squirrels DESC;
  "
krsp_sql(con, sql) %>% knitr::kable()

## ----error, error=TRUE---------------------------------------------------
third <- trapping %>% 
  filter(gr == "JO") %>% 
  summarize(third = quantile(scale_weight, probs = 0.75, na.rm = TRUE))
third

## ----error-fixed---------------------------------------------------------
third <- trapping %>% 
  filter(gr == "JO") %>% 
  collect() %>% 
  summarize(third = quantile(scale_weight, probs = 0.75, na.rm = TRUE))
third

## ----force-error, error=TRUE---------------------------------------------
# sd() has a SQL equivalent
trapping %>% 
  summarize(test = sd(scale_weight)) %>% 
  collect()
# but median() doesn't
trapping %>% 
  summarize(test = median(scale_weight)) %>% 
  collect()

## ---- error=TRUE---------------------------------------------------------
# fine
tbl(con, "dbaMidden") %>% 
  select(locX) %>% 
  head
# error
tbl(con, "dbaMidden") %>% 
  select(locx) %>% 
  head
# error
tbl(con, "census") %>% 
  select(locX) %>% 
  head
# fine
tbl(con, "census") %>% 
  select(locx) %>% 
  head

## ------------------------------------------------------------------------
# only the first 100,000 rows are returned
tbl(con, "trapping") %>% 
  collect() %>% 
  nrow()
# return all rows
tbl(con, "trapping") %>% 
  collect(n = Inf) %>% 
  nrow()

