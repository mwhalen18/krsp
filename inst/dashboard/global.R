#devtools::install_github("rstudio/pool")
library(shiny)
library(DBI)
library(pool)
library(krsp)
library(ggvis)
library(DT)
library(dplyr)
library(tidyr)
library(readr)

# set connection parameters for calling directly
if (!exists(".dbcon")) {
  .dbcon <- list(
    dbname = "krsp",
    host = "localhost",
    username = "root",
    password = "")
}

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = .dbcon$dbname,
  host = .dbcon$host,
  username = .dbcon$username,
  password = .dbcon$password
)
# shortcut for krsp_pool
kp <- krsp_pool

# list of grids in database
grids <- kp(pool) %>%
  krsp:::grid_list() %>%
  sort()
active_grids <- kp(pool) %>%
  krsp:::active_grids() %>%
  sort()
years <- kp(pool) %>%
  krsp:::year_list() %>%
  sort(decreasing = TRUE)

# valid colours
valid_colours <- c("B", "R", "G", "Y", "O", "W", "P", "Bk", "Gy")

# check query descriptions
check_descriptions <- read_csv("check-descriptions.csv")

# progress help
progress_help <- div(
  p("This tool provides an overview of data collection progress for ",
    "the season. All adult females caught in the given year are inlcuded ",
    "provided their most recent trapping record has fate 1-3. ",
    "The first four columns identify the squirrel by squirrel ID, tags, ",
    "colours, and location, respectively, all taken from their most recent ",
    "trapping record."),
  p("The ", strong("Litter"), " column identifies which litter number the ",
    "row applies to. Each litter a squirrel has will give rise to a ",
    "unique row in the table. For example, if a squirrel has given birth ",
    "to their second litter, they will have two rows, one with Litter = 1 ",
    "and another with Litter = 2. If a squirrel hasn't yet given birth to ",
    "a litter this year, or for some other reason doesn't appear in the ",
    "litter table, they will have a single row with Litter set to '-'."),
  p("The ", strong("Litter Status"), " and ", strong("Litter Date"),
    " columns are based on the corresponding entries in the litter ",
    "table. If a nest 2 date has been entered into the litter table, then ",
    "Litter Status will be 'N2' and the Litter Date will be the nest 2 date. ",
    "If a nest 2 date is missing, then columns are filled with the nest 1 ",
    "data. If there is no nest 1 data, but a field birthdate has ",
    "been entered into the litter table, this field birtdate is used and the ",
    "Litter status will be 'Parturition'. Finally, if the breeding status ",
    "indicates a non-breeder or lost litter then Litter Status will be ",
    "'Non-breeder' or 'LL' respectively."),
  p("The ", strong("Trap Status"), " and ", strong("Trap Date"), " columns ",
    "are derived from the most recent trapping record. Trap Date gives the ",
    "last time the female was trapped. Trap Status gives the reproductive ",
    "condition from this most recent trapping record (i.e. P0-P3), unless ",
    "the nipple condition indicates that the squirrel has lost its litter, ",
    "in which case Trap Status is 'LL'."),
  p("The ", strong("Status"), " column is a combination of Trap Status and ",
    "Litter Status. Status will be the same as Trap Status if Litter Status ",
    "is missing or Litter Date < Trap Date. If Litter Date > Trap Date, or ",
    "Trap Status is P0, then Status will be equal to Litter Status. ",
    "Of particular note, if a nest 2 has been completed and that ",
    "female has subsequently been trapped and found to be P0, then ",
    "Status will be P0."),
  p("Note that a single row may contain information on 2 litters. If a ",
    "female has completed her first litter and is pregnant with her second ",
    "litter, but a record for this second litter has not yet been entered ",
    "into the litter table, then there will be a row with Litter Status = ",
    "N2 and Trap Status = P3. To avoid this, the field crew should ",
    "immediately create a new litter record once a squirrel is pregnant.")
)

# help for the census tool
census_help <- div(
  p("This tool is used to track progress towards completing the ",
    "squirrel census. Two views are provided. The ",
    strong("Progress"), " tab is a squirrel focused view, which ",
    "can be used to highlight squirrels that have not yet been",
    "entered into the census. In contrast, the ", strong("Map"),
    "tab is a midden focused view, which can be used to highlight ",
    "middens that haven't yet been asigned an owner in the census."),
  p("The ", strong("Progress"), " tab displays a list of all ",
    "squirrels caught leading up to the given census, and indicates ",
    "if these squirrels have already been entered into the census. ",
    "For a May census, all squirrels caught between January 1st and ",
    "May 15th of that year are included. For an August census, all ",
    "squirrels caught since the previous May 15th are included.",
    "Note that Loc X and Loc Y refer to the location of the most ",
    "recent trapping record, while Reflo refers to the census."),
  p("The ", strong("Map"), " tab displays a map of middens from the ",
    "previous census, coloured according to their fate in the ",
    "current census. Middens that haven't yet been entered into the ",
    "current census are shown as squares. The ", strong("Map Data"),
    " tab shows this same data in tabular form. Excluded from this ",
    "view are all squirrels with floater fates (3, 14, and 17) as well as ",
    "juveniles sharing middens with their mothers (fate 7).")
)

# ggvis plot for no data
message_plot <- function(message = "No data found.") {
  p <- data.frame(x = 0, y = 0.8, m = message) %>%
    ggvis(~x, ~y) %>%
    layer_text(text := ~m, fontSize := 30, font := "Helvetica Neue") %>%
    scale_numeric("y", domain = c(0, 1)) %>%
    hide_axis("x") %>%
    hide_axis("y")
  return(p)
}
