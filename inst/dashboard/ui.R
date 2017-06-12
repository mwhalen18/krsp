shinyUI(navbarPage(theme = "style.css",

  # title
  "Squirrel Dashboard",

  ##########   Rattle Map   ##########
  tabPanel("Rattle Map",
    sidebarLayout(
      sidebarPanel(
        h2("Rattle Map"),
        p("Display a map of rattles from behaviour and trapping records."),
        h5("Grid and year: "),
        fluidRow(
          column(6, selectInput("grid_input_rattle", NULL, grids)),
          column(6, selectInput("year_input_rattle", NULL, years))
        ),
        selectInput("middens_rattle", "Show middens?",
                    c("No" = "none",
                      "August census, last year" = "august",
                      "May census, this year" = "may")),
        actionButton("submit_rattle", "Submit"),
        conditionalPanel(condition = "input.submit_rattle > 0",
          h3("Filter"),
          uiOutput("date_input_rattle"),
          uiOutput("locx_input_rattle"),
          uiOutput("locy_input_rattle"),
          downloadButton("download_data_rattle", "Download")
        ),
        width = 3
      ),

      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Map", ggvisOutput("plot_rattle"),
                   conditionalPanel(condition = "input.submit_rattle > 0",
                      p(strong("Note: "), "rattles appear as solid symbols, ",
                        "middens as hollow symbols. Colours distinguish ",
                        "different squirrels."))),
          tabPanel("Table", br(), DT::dataTableOutput("table_rattle"))
        )
      )
    )
  ),

  ##########   Progress   ##########
  tabPanel("Progress",
    sidebarLayout(
      sidebarPanel(
        h2("Progress"),
        p("Data collection progress for females on the given grid."),
        selectInput("grid_input_progress", "Grid:", grids),
        selectInput("year_input_progress", "Year:", years),
        actionButton("submit_progress", "Submit"),
        conditionalPanel(condition = "input.submit_progress > 0",
          h3("Colour key"),
          p("The ", strong("Status"), "column is colour-coded as follows:"),
          tags$table(
            tags$tr(tags$td("Pregnant", bgcolor = "#4DAF4A")),
            tags$tr(tags$td("Parturition", bgcolor = "#E41A1C")),
            tags$tr(tags$td("Nest in progress", bgcolor = "#FF7F00")),
            tags$tr(tags$td("Nest completed", bgcolor = "#377EB8")),
            tags$tr(tags$td("Lost litter", bgcolor = "#555555")),
            tags$tr(tags$td("P0/Non-breeder", bgcolor = "#FFFFFF",
                            style = "color: black; border: 1px solid black;")),
            style = "width: 100%; color: white; font-weight: bold; text-align: center; padding: 10px;"
          )
        ),
        width = 3
      ),

      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Results", DT::dataTableOutput("table_progress")),
          tabPanel("Help", progress_help)
        )
      )
    )
  ),

  ##########   Census   ##########
  tabPanel("Census",
    sidebarLayout(
      sidebarPanel(
        h2("Census"),
        p("Display progress towards completing the census."),
        h5("Which census are you completing?"),
        selectInput("grid_input_census", NULL, active_grids),
        fluidRow(
          column(6, selectInput("census_input_census", NULL,
                                c(May = "may", August = "august"))),
          column(6, selectInput("year_input_census", NULL,
                                years[years >= 2016]))
        ),
        actionButton("submit_census", "Submit"),
        conditionalPanel(condition = "input.submit_census > 0",
                         downloadButton("download_data_census", "Download")
        ),
        width = 3
      ),

      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Progress", br(), DT::dataTableOutput("table_census")),
          tabPanel("Map",
            ggvisOutput("plot_censusmap"),
            conditionalPanel(condition = "input.submit_census > 0",
                             align = "center",
                             img(src = 'fate-legend.svg', width = 600,
                                 align = "center"))),
          tabPanel("Map Data", br(), DT::dataTableOutput("table_censusmap")),
          tabPanel("Help", br(),
            census_help,
            h3("Fate descriptions"),
            DT::dataTableOutput("table_fate_descriptions"))
        )
      )
    )
  ),

  ##########   Collars   ##########
  tabPanel("Collars",
    sidebarLayout(
      sidebarPanel(
        h2("Collars"),
        p("Show squirrels that currently have radio collars on."),
        selectInput("grid_input_collars", "Grid:", c("All", grids)),
        selectInput("year_input_collars", "Year:", years),
        actionButton("submit_collars", "Submit"),
        width = 3
      ),

      # main panel
      mainPanel(
        DT::dataTableOutput("table_collars")
      )
    )
  ),

  ##########   Data Checking   ##########
  tabPanel("Data Checking",
    sidebarLayout(
      sidebarPanel(
        h2("Data Checking"),
        p("Run data integrity checks on the field data. ",
          "This tool only highlights potential errors, ",
          "which will need to be manually fixed in the database."),
        selectInput("grid_input_checks", "Grid:", c("All", grids)),
        selectInput("year_input_checks", "Year:", years),
        selectInput("type_input_checks", "Check:",
                    c("Trapping", "Nests", "Collars", "Behaviour")),
        uiOutput("description_checks"),
        fluidRow(
          column(width = 6, actionButton("submit_checks", "Submit")),
          column(width = 6,
                 conditionalPanel(
                   condition = "input.submit_checks > 0",
                   downloadButton("download_data_checks", "Download")
                  )
          )
        ),
        br(),
        conditionalPanel(condition = "input.submit_checks > 0",
          uiOutput("date_input_checks")
        ),
        width = 3
      ),

      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Results", br(), DT::dataTableOutput("table_checks")),
          tabPanel("Check Descriptions", br(),
                   p("Within each category, a variety of specific checks are ",
                     "performed as noted in the",
                     strong("check"),
                     "column. A brief description of each check is given ",
                     "below. Every check corresponds to a function in the ",
                     a("krsp", href = "https://github.com/mstrimas/krsp"),
                     " R package, and further details can be found in ",
                     "documention for these functions."),
                   DT::dataTableOutput("table_checks_descriptions"))
        )
      )
    )
  ),

  ##########   Colour Keys   ##########
  tabPanel("Colour Keys",
    sidebarLayout(
      sidebarPanel(
        h2("Colour Keys"),
        p("Look up tags and most recent trapping location by colours. ",
          "Female and male keys taken from most recent trapping event, ",
          "juvenile key based on trapping and litter records."),
        selectInput("grid_input_colours", "Grid:", grids),
        selectInput("year_input_colours", "Year:", years),
        fluidRow(
          column(width = 6, actionButton("submit_colours", "Submit")),
          column(width = 6,
                 conditionalPanel(
                   condition = "input.submit_colour > 0",
                   downloadButton("download_data_colours", "Download")
                 )
          )
        ),
        hr(),
        conditionalPanel(condition = "input.submit_colours > 0",
                         textOutput("more_colours")),
        width = 3
      ),

      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Female", br(),
                   div(DT::dataTableOutput("table_female_colours")),
                   style = "font-size:80%"),
          tabPanel("Male", br(),
                   div(DT::dataTableOutput("table_male_colours")),
                   style = "font-size:80%"),
          tabPanel("Juvenile", br(),
                   div(DT::dataTableOutput("table_juve_colours")),
                   style = "font-size:80%"),
          tabPanel("All", br(), DT::dataTableOutput("table_all_colours"))
        )
      )
    )
  ),

  ##########   Part Date Calculator   ##########
  tabPanel("Part Date",
    fluidRow(column(width = 6, offset = 3,
      h2("Parturition Date Calculator"),
      p("Estimate a litter's parturition date based on nest 1 weights. ",
        "If available, specify the last date the female was confirmed pregnant ",
        "and the first date she was confirmed lactating based on trapping events.",
        "Values can be populated from the database based on squirrel ID and ",
        "litter number."),
      p(strong("Look up squirrel:")),
      fluidRow(
        column(3, strong("Year:")),
        column(3, strong("Squirrel ID:")),
        column(3, strong("Litter #:"))
      ),
      fluidRow(
        column(3, selectInput("year_pdate", NA, years)),
        column(3, numericInput("sid_pdate", NA, NA, min = 1, max = 100000,
                               step = 1, width = "100%")),
        column(3, numericInput("ln_pdate", NA, NA, min = 1, max = 4,
                               step = 1, width = "100%")),
        column(3, actionButton("lookup_pdate", "Look Up"))
      ),
      p(strong("Nest 1 weights (grams):")),
      fluidRow(
        column(6, numericInput("w1", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%")),
        column(6, numericInput("w2", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%"))
      ),
      fluidRow(
        column(6, numericInput("w3", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%")),
        column(6, numericInput("w4", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%"))
      ),
      fluidRow(
        column(6, numericInput("w5", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%")),
        column(6, numericInput("w6", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%"))
      ),
      fluidRow(
        column(6, numericInput("w7", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%")),
        column(6, numericInput("w8", NA, NA, min = 1, max = 100,
                               step = 0.1, width = "100%"))
      ),
      fluidRow(
        column(4, dateInput("n1date_pdate", "Nest 1 date:", value = Sys.Date(),
                            min = "1987-01-01", max = Sys.Date() + 14)),
        column(4, dateInput("preg_pdate", "Last pregnant:", value = Sys.Date(),
                            min = "1987-01-01", max = Sys.Date() + 14)),
        column(4, dateInput("lac_pdate", "First lactating:", value = Sys.Date(),
                            min = "1987-01-01", max = Sys.Date() + 14))
      ),
      fluidRow(
        column(4, actionButton("submit_pdate", "Submit")),
        column(4, checkboxInput("usepreg_pdate", "Use last pregnant date",
                                value = TRUE)),
        column(4, checkboxInput("uselac_pdate", "Use first lactating date",
                                value = TRUE))
      ),
      hr(),
      conditionalPanel(condition = "input.submit_pdate > 0",
        fluidRow(column(4, strong("Litter Size:"),
                        textOutput("littersize_pdate")),
                 column(4, strong("Mean weight:"),
                        textOutput("meanweight_pdate")),
                 column(4, strong("Parturition date:"),
                        textOutput("pdate_pdate"))
        ),
        fluidRow(column(12,
          p(strong("Note:"), "This calculation is an", strong("estimate"),
            " and should never supersede information from ",
            strong("trapping events."))
        ))
      )
    ))
  ),

  ##########   Top Squirrelers   ##########
  tabPanel("Top Squirrelers",
    sidebarLayout(
      sidebarPanel(
        h2("Top Squirrelers"),
        p("Display the most productive squirrelers based ",
          "on a variety of metrics. Pick a specific year ",
          "or look at the all-time best squirrelers."),
        selectInput("year_input_top", "Year:", c("All Time", years)),
        selectInput("metric_input_top", "Metric:",
                    c("trapping", "collars", "behaviour")),
        uiOutput("description_top"),
        actionButton("submit_top", "Submit"),
        width = 3
      ),

      # main panel
      mainPanel(
        DT::dataTableOutput("table_top")
      )
    )
  )
))
