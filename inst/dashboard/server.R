shinyServer(function(input, output, session) {
  ##########   Rattle Map   ##########
  fresh_data_rattle <- TRUE

  # data
  rattles <- eventReactive(input$submit_rattle, {
    fresh_data_rattle <<- TRUE
    kp(pool) %>%
      krsp_rattlemap(grid = input$grid_input_rattle,
                     year = input$year_input_rattle,
                     middens = input$middens_rattle,
                     data = TRUE)
  })
  reverse_grid_rattle <- eventReactive(input$submit_rattle, {
    input$grid_input_rattle == "AG"
  })

  # filters for rattle data frame
  # date
  output$date_input_rattle <- renderUI({
    if (is.data.frame(rattles()) & nrow(rattles()) > 0) {
      dates <- range(rattles()$date)
    } else {
      dates <- rep(Sys.Date(), 2)
    }
    dateRangeInput("date_input_rattle", "Dates: ",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2],
                   separator = "-")
  })
  # loc x
  output$locx_input_rattle <- renderUI({
    if (is.data.frame(rattles()) & nrow(rattles()) > 0) {
      locs <- range(rattles()$x)
    } else {
      locs <- c(0, 22)
    }
    locs[1] <- floor(locs[1])
    locs[2] <- ceiling(locs[2])
    sliderInput("locx_input_rattle", "Loc X: ",
                min = locs[1], max = locs[2],
                value = locs,
                step = 1)
  })
  # loc y
  output$locy_input_rattle <- renderUI({
    if (is.data.frame(rattles()) & nrow(rattles()) > 0) {
      locs <- range(rattles()$y)
    } else {
      locs <- c(0, 22)
    }
    locs[1] <- floor(locs[1])
    locs[2] <- ceiling(locs[2])
    sliderInput("locy_input_rattle", "Loc Y: ",
                min = locs[1], max = locs[2],
                value = locs,
                step = 1)
  })

  # reactive expressions for filters
  # required because web page inputs not updated until after full flush
  # date
  date_range <- reactive({
    r <- rattles()
    i <- input$date_input_rattle
    # if new data have just been pulled reference data frame directly
    if (is.data.frame(r) & nrow(r) & fresh_data_rattle) {
      dates <- range(r$date)
    } else if (!is.null(i)) {
      dates <- i
      if (dates[1] > dates[2]) {
        message_plot("To date must be after from date.") %>%
          bind_shiny("plot_rattle")
        dates <- NULL
      }
    } else {
      dates <- NULL
    }
    return(dates)
  })
  # loc x
  locx_range <- reactive({
    r <- rattles()
    i <- input$locx_input_rattle
    # if new data have just been pulled reference data frame directly
    if (is.data.frame(r) & nrow(r) & fresh_data_rattle) {
      locs <- range(r$x)
      locs[1] <- floor(locs[1])
      locs[2] <- ceiling(locs[2])
    } else if (!is.null(i)) {
      locs <- i
    } else {
      locs <- NULL
    }
    return(locs)
  })
  # loc y
  locy_range <- reactive({
    r <- rattles()
    i <- input$locy_input_rattle
    # if new data have just been pulled reference data frame directly
    if (is.data.frame(r) & nrow(r) & fresh_data_rattle) {
      locs <- range(r$y)
      locs[1] <- floor(locs[1])
      locs[2] <- ceiling(locs[2])
    } else if (!is.null(i)) {
      locs <- i
    } else {
      locs <- NULL
    }
    return(locs)
  })

  # filtered data
  rattles_filtered <- reactive({
    # next 3 lines required to ensure reactive dependence on ranges
    dates <- date_range()
    locx <- locx_range()
    locy <- locy_range()
    if (is.data.frame(rattles()) & nrow(rattles()) > 0 &
        !is.null(dates) & !is.null(locx) & !is.null(locy)) {
      r <- rattles() %>%
        filter(date >= dates[1], date <= dates[2],
               x >= locx[1], x <= locx[2],
               y >= locy[1], y <= locy[2])
      if (nrow(r) == 0) {
        r <- NULL
      }
    } else {
      r <- NULL
    }
    return(r)
  })

  # plot
  observe({
    if (!is.null(rattles_filtered())) {
      rattles_filtered() %>%
        krsp:::plot_rattles(reverse_grid_rattle()) %>%
        bind_shiny("plot_rattle")
    } else {
      message_plot("No rattles found.") %>%
        bind_shiny("plot_rattle")
    }
    # ui now updated, data no longer fresh
    fresh_data_rattle <<- FALSE
  })

  # data table
  output$table_rattle <- DT::renderDataTable(
    if (!is.null(rattles_filtered())) {
      rattles_filtered() %>% select(-id, -grid) %>% mutate(source = factor(source))
    } else {NULL},
    server = TRUE,
    options = list(pageLength = 20, autoWidth = TRUE),
    class = 'nowrap stripe compact',
    rownames = FALSE,
    filter = "top",
    colnames = c(
      "ID" = "squirrel_id",
      "Loc X" = "x",
      "Loc Y" = "y",
      "Sex" = "sex",
      "Colours" = "colours",
      "Tags" = "tags",
      "Rattle Date" = "date",
      "Last Trapped" = "trap_date",
      "Source" = "source")
  )
  # download
  output$download_data_rattle <- downloadHandler(
    filename = function() {
      paste0("rattles-",
             tolower(input$grid_input_rattle), "-",
             input$year_input_rattle,
             ".csv")
    },
    content = function(file) {
      data <- rattles_filtered()
      validate(
        need(is.data.frame(data) & nrow(data) > 0,
             "No data to download")
      )
      write_csv(data, file)
    }
  )

  ##########   Progress   ##########

  # data
  progress <- eventReactive(input$submit_progress, {
    if (input$grid_input_progress == "All") {
      kp(pool) %>%
        krsp_progress(year = input$year_input_progress,
                      sex = input$sex_input_progress,
                      data = TRUE)
    } else {
      kp(pool) %>%
        krsp_progress(grid = input$grid_input_progress,
                      year = input$year_input_progress,
                      sex = input$sex_input_progress,
                      data = TRUE)
    }
  })
  
  # data table
  output$table_progress = DT::renderDataTable(
    if (!is.null(progress())) krsp:::progress_datatable(progress()) else NULL,
    server = TRUE
  )

  ##########   Census   ##########

  # data
  census <- eventReactive(input$submit_census, {
    kp(pool) %>%
      krsp_census_progress(grid = input$grid_input_census,
                           year = input$year_input_census,
                           census = input$census_input_census) %>%
      mutate_(in_census = ~ if_else(in_census, "Y", "N"))
  })
  census_map <- eventReactive(input$submit_census, {
    kp(pool) %>%
      krsp_censusmap(grid = input$grid_input_census,
                     year = input$year_input_census,
                     census = input$census_input_census,
                     data = TRUE)
  })
  reverse_grid_census <- eventReactive(input$submit_census, {
    input$grid_input_census == "AG"
  })

  # progress table
  output$table_census = DT::renderDataTable(
    if (!is.null(census())) {census() %>% select(-grid)} else {NULL},
    server = TRUE,
    options = list(pageLength = 20, autoWidth = TRUE),
    class = 'nowrap stripe compact',
    rownames = FALSE,
    filter = "top",
    colnames = c(
      "ID" = "squirrel_id",
      "Colours" = "colours",
      "Tags" = "tags",
      "Sex" = "sex",
      "Trap Date" = "trap_date",
      "Loc X" = "locx",
      "Loc Y" = "locy",
      "In census?" = "in_census",
      "Reflo" = "census_reflo",
      "Fate" = "census_fate")
  )

  # map
  observe({
    data <- census_map()
    if (is.data.frame(data) & nrow(data) > 0) {
      data %>%
        krsp:::plot_census(reverse_grid_census()) %>%
        bind_shiny("plot_censusmap")
    } else {
      message_plot("No records found.") %>%
        bind_shiny("plot_censusmap")
    }
  })
  # map data
  output$table_censusmap = DT::renderDataTable(
    if (!is.null(census_map())) {census_map() %>% select(-grid)} else {NULL},
    server = TRUE,
    options = list(pageLength = 20, autoWidth = TRUE),
    class = 'nowrap stripe compact',
    rownames = FALSE,
    filter = "top",
    colnames = c(
      "Reflo" = "reflo",
      "Loc X" = "locx",
      "Loc Y" = "locy",
      "Previous Squirrel" = "squirrel_id",
      "New Squirrel" = "squirrel_id_new",
      "Previous Fate" = "fate",
      "New Fate" = "fate_new"
    )
  )
  # fate lookup
  fate_loookup <- reactive(read_csv("fate-descriptions.csv"))
  output$table_fate_descriptions = DT::renderDataTable(
    fate_loookup(),
    server = FALSE,
    rownames = FALSE,
    options = list(
      info = FALSE,
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE
    ))

  # download
  output$download_data_census <- downloadHandler(
    filename = function() {
      paste0("census-progress-",
             tolower(input$grid_input_census), "-",
             input$census_input_census, "-",
             input$year_input_rattle,
             ".csv")
    },
    content = function(file) {
      data <- census()
      validate(
        need(is.data.frame(data) & nrow(data) > 0,
             "No data to download")
      )
      write_csv(data, file)
    }
  )

  ##########   Collars   ##########

  # data
  collars <- eventReactive(input$submit_collars, {
    if (input$grid_input_collars == "All") {
      results <- krsp_collars(kp(pool),
                              year = input$year_input_collars)
    } else {
      results <- krsp_collars(kp(pool),
                              grid = input$grid_input_collars,
                              year = input$year_input_collars)
    }
    results
  })
  # data table
  output$table_collars = DT::renderDataTable(
    if (!is.null(collars())) collars() %>% select(-id) else NULL,
    server = TRUE,
    options = list(pageLength = 20, autoWidth = TRUE),
    class = 'nowrap stripe compact',
    rownames = FALSE,
    filter = "top",
    colnames = c(
      "ID" = "squirrel_id",
      "Tags" = "tags",
      "Colours" = "colours",
      "Location" = "loc",
      "Grid" = "grid",
      "Year" = "year",
      "Observer" = "observer",
      "Collar Date" = "date_new",
      "Frequency" = "collar",
      "Last Trapped" = "last_trapped")
  )

  ##########   Data Checking   ##########

  # description of check
  output$description_checks <- renderUI({
    if (input$type_input_checks == "Trapping") {
      description <- "Perform data integrity checks on the trapping table."
    } else if (input$type_input_checks == "Nests") {
      description <- "Perform data integrity checks on the nest tables: litter and juvenile."
    } else if (input$type_input_checks == "Collars") {
      description <- "Check that radio collars have been correctly entered into the trapping table."
    } else if (input$type_input_checks == "Behaviour") {
      description <- "Perform data integrity checks on the behaviour table."
    } else {
      description <- ""
    }
    p(em(description))
  })
  # date range
  output$date_input_checks <- renderUI({
    sdate = paste0(input$year_input_checks, "-01-01")
    edate = paste0(input$year_input_checks, "-12-31")
    dateRangeInput("date_input_checks", "Restrict dates: ",
                   start = sdate, end = edate,
                   min = sdate, max = edate,
                   separator = "-")
  })
  # data
  checks <- eventReactive(input$submit_checks, {
    # determine which function to use
    if (input$type_input_checks == "Trapping") {
      check_fun <- check_trapping
    } else if (input$type_input_checks == "Nests") {
      check_fun <- check_nest
    } else if (input$type_input_checks == "Collars") {
      check_fun <- check_collars
    } else if (input$type_input_checks == "Behaviour") {
      check_fun <- check_behaviour
    } else {
      check_fun <- function(...) {return(NULL)}
    }
    # get the data
    if (input$grid_input_checks == "All") {
      results <- check_fun(kp(pool),
                           year = input$year_input_checks)
    } else {
      results <- check_fun(kp(pool),
                           grid = input$grid_input_checks,
                           year = input$year_input_checks)
    }
    # convert some columns to factor variables for better filtering
    if ("check" %in% names(results)) {
      results <- results %>% mutate(check = factor(check))
    }
    if ("grid" %in% names(results)) {
      results <- results %>% mutate(grid = factor(grid))
    }
    if ("observer" %in% names(results)) {
      results <- results %>% mutate(observer = factor(observer))
    }
    results
  })
  # data table
  output$table_checks = DT::renderDataTable(
    if (!is.null(checks()) && !is.null(input$date_input_checks)) {
      checks() %>% filter(date >= input$date_input_checks[1],
                          date <= input$date_input_checks[2])
    } else {
      NULL
    },
    server = TRUE,
    options = list(pageLength = 20, autoWidth = TRUE),
    class = 'nowrap stripe compact',
    rownames = FALSE,
    filter = "top"
  )
  # data table - check descriptions
  descriptions <- reactive({
    filter(check_descriptions, Category == input$type_input_checks) %>%
      select(-Category)
  })
  output$table_checks_descriptions = DT::renderDataTable(
    if (!is.null(descriptions())) descriptions() else NULL,
    server = FALSE,
    rownames = FALSE,
    options = list(
      info = FALSE,
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE
    ))
  # download
  output$download_data_checks <- downloadHandler(
    filename = function() {
      paste0("check-",
             tolower(input$type_input_checks), "-",
             tolower(input$grid_input_checks), "-",
             input$year_input_checks,
             ".csv")
    },
    content = function(file) {
      data <- checks()
      validate(
        need(is.data.frame(data) & nrow(data) > 0,
             "No data to download")
      )
      write_csv(data, file)
    }
  )

  ##########   Colour Keys   ##########

  # data
  colours <- eventReactive(input$submit_colours, {
      krsp_colours(kp(pool),
                   grid = input$grid_input_colours,
                   year = input$year_input_colours)
  })
  # details on more colours
  output$more_colours <-   renderText({
    paste0(
      "Note: Duplicate colours (i.e. more than one squirrel with the same ",
      "colour combination) and non-standard colour combinations (i.e. more ",
      "than one colour on a single ear or a mix of wires/pipes/bars) are not ",
      "included in these keys. These cases will be visible in the All tab.")
  })

  ## female
  female_colours <- reactive({
    if (!is.data.frame(colours())) {
      return(NULL)
    }
    # filter to desired squirrels
    card <- filter(colours(), sex == "F", !juvenile, standard, !duplicate, valid)

    if (nrow(card) == 0) {
      return(NULL)
    }

    card <- card %>%
      # combine tags and loc into single cell value
      mutate(cell_value = sprintf("%s<br><strong>%s</strong>", tags, reflo)) %>%
      # convert to factors to ensure correct ordering
      mutate(left = factor(left, levels = c('-', valid_colours)),
             right = factor(right, levels = c('-', valid_colours))) %>%
      select(left, right, cell_value)
    # transpose, long to wide
    card <- spread(card, right, cell_value)
    DT::datatable(card,
                  rownames = FALSE,
                  colnames = c("L/R" = "left"),
                  escape = FALSE,
                  class = 'nowrap stripe compact row-border',
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    ordering = FALSE,
                    columnDefs = list(list(className = 'dt-center',
                                           targets = 0:(ncol(card) - 1))))
    ) %>%
    formatStyle("L/R", textAlign = "center", fontWeight = "bold")
  })
  # output
  output$table_female_colours = DT::renderDataTable(
    if (!is.null(female_colours())) female_colours() else NULL,
    server = FALSE)

  ## male
  male_colours <- reactive({
    if (!is.data.frame(colours())) {
      return(NULL)
    }

    # filter to desired squirrels
    card <- filter(colours(), sex == "M", !juvenile, standard, !duplicate, valid)

    if (nrow(card) == 0) {
      return(NULL)
    }

    card <- card %>%
      # combine tags and loc into single cell value
      mutate(cell_value = sprintf("%s<br><strong>%s</strong>", tags, reflo)) %>%
      # convert to factors to ensure correct ordering
      mutate(left = factor(left, levels = c('-', paste0(valid_colours, "!"))),
             right = factor(right, levels = c('-', paste0(valid_colours, "!")))) %>%
      select(left, right, cell_value)
    # transpose, long to wide
    card <- spread(card, right, cell_value)
    DT::datatable(card,
                  rownames = FALSE,
                  colnames = c("L/R" = "left"),
                  escape = FALSE,
                  class = 'nowrap stripe compact row-border',
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    ordering = FALSE,
                    columnDefs = list(list(className = 'dt-center',
                                           targets = 0:(ncol(card) - 1))))
    ) %>%
      formatStyle("L/R", textAlign = "center", fontWeight = "bold")
  })
  # output
  output$table_male_colours = DT::renderDataTable(
    if (!is.null(male_colours())) male_colours() else NULL,
    server = FALSE)

    ## juvenile
  juve_colours <- reactive({
    if (!is.data.frame(colours())) {
      return(NULL)
    }

    # filter to desired squirrels
    card <- filter(colours(), juvenile, standard, !duplicate, valid)


    if (nrow(card) == 0) {
      return(NULL)
    }

    card <- card %>%
      # combine tags and loc into single cell value
      mutate(cell_value = sprintf("%s<br><strong>%s</strong>", tags, reflo)) %>%
      # convert to factors to ensure correct ordering
      mutate(left = factor(left, levels = c('-', paste0(valid_colours, "*"))),
             right = factor(right, levels = c('-', paste0(valid_colours, "*")))) %>%
      select(left, right, cell_value)
    # transpose, long to wide
    card <- spread(card, right, cell_value)
    DT::datatable(card,
                  rownames = FALSE,
                  colnames = c("L/R" = "left"),
                  escape = FALSE,
                  class = 'nowrap stripe compact row-border',
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    ordering = FALSE,
                    columnDefs = list(list(className = 'dt-center',
                                           targets = 0:(ncol(card) - 1))))
    ) %>%
      formatStyle("L/R", textAlign = "center", fontWeight = "bold")
  })
  # output
  output$table_juve_colours = DT::renderDataTable(
    if (!is.null(juve_colours())) juve_colours() else NULL,
    server = FALSE)

  # all
  output$table_all_colours = DT::renderDataTable(
    if (!is.null(colours())) colours() else NULL,
    server = TRUE,
    options = list(
      pageLength = 20,
      autoWidth = TRUE,
      searching = FALSE),
    class = 'nowrap stripe compact',
    rownames = FALSE,
    colnames = c(
      "Left" = "left",
      "Right" = "right",
      "ID" = "squirrel_id",
      "Sex" = "sex",
      "Juvenile" = "juvenile",
      "Tags" = "tags",
      "Colours" = "colours",
      "Location" = "reflo",
      "Last Trapped" = "last_trapped",
      "Standard" = "standard",
      "Duplicate" = "duplicate",
      "Valid" = "valid")
  )

  # download
  output$download_data_colours <- downloadHandler(
    filename = function() {
      paste0("colours-",
             tolower(input$grid_input_colours), "-",
             input$year_input_colours,
             ".csv")
    },
    content = function(file) {
      data <- colours()
      validate(
        need(is.data.frame(data) & nrow(data) > 0,
             "No data to download")
      )
      write_csv(data, file)
    }
  )

  ##########   Part Date Calculator   ##########

  observeEvent(input$lookup_pdate, {
    # lookup litter weights
    litter_lookup <- tryCatch({
      krsp_litter_lookup(kp(pool),
                         input$year_pdate,
                         input$sid_pdate,
                         input$ln_pdate)},
      error = function(e) NULL)
    if (!is.null(litter_lookup)) {
      weights <- na.omit(litter_lookup$weight)
      n1_date <- na.omit(litter_lookup$n1_date)[1]
      # get first date lac and last date pregnant
      lacpreg_date <- krsp:::lastpreg_firstlac(kp(pool), input$sid_pdate,
                                               n1_date)
      # populate weights
      for (i in seq_along(weights)) {
        updateNumericInput(session, paste0("w", i), value = weights[i])
      }
      # populate dates
      updateDateInput(session, "n1date_pdate", value = n1_date)
      # last pregnant date
      if (!is.na(lacpreg_date[1])) {
        updateDateInput(session, "preg_pdate", value = lacpreg_date[1])
        updateCheckboxInput(session, "usepreg_pdate", value = TRUE)
      } else {
        updateCheckboxInput(session, "usepreg_pdate", value = FALSE)
      }
      # first date lactating
      if (!is.na(lacpreg_date[2])) {
        updateDateInput(session, "lac_pdate", value = lacpreg_date[2])
        updateCheckboxInput(session, "uselac_pdate", value = TRUE)
      } else {
        updateCheckboxInput(session, "uselac_pdate", value = FALSE)
      }
    }
  })

  # display results
  litter_data <- eventReactive(input$submit_pdate, {
    weights <- rep(NA, 8)
    for (i in 1:8) {
      w <- input[[paste0("w", i)]]
      weights[i] <- ifelse(is.numeric(w) & w > 0, w, NA)
    }
    weights
  })
  # litter size
  output$littersize_pdate <- renderText({
    if (is.null(litter_data())) return("-")
    litter_size <- sum(!is.na(litter_data()))
    litter_size
  })
  # mean weight
  output$meanweight_pdate <- renderText({
    if (is.null(litter_data())) return("-")
    mean_weight <- mean(litter_data(), na.rm = T)
    if (!is.na(mean_weight)) {
      mean_weight <- round(mean_weight, 1)
      format(mean_weight, nsmall = 1, scientific = FALSE)
    } else {
      mean_weight <- "-"
    }
    mean_weight
  })
  # part date
  output$pdate_pdate <- renderText({
    if (is.null(litter_data())) return("-")
    mean_weight <- mean(litter_data(), na.rm = T)
    if (is.na(mean_weight)) {
      return("-")
    }
    growth_days <- max((mean_weight - 10) / 2, 0)
    part_date <- input$n1date_pdate - round(growth_days)
    if (input$usepreg_pdate) {
      part_date <- max(part_date, input$preg_pdate)
    }
    if (input$uselac_pdate) {
      part_date <- min(part_date, input$lac_pdate)
    }
    format(part_date, "%Y-%m-%d")
  })

  ##########   Top Squirrelers   ##########

  # description of check
  output$description_top <- renderUI({
    if (input$metric_input_top == "trapping") {
      description <- "Rank according to the number of squirrels trapped"
    } else if (input$metric_input_top == "collars") {
      description <- "Rank according to the number of new collars put on."
    } else if (input$metric_input_top == "behaviour") {
      description <- "Rank according to the number of behaviour observations."
    } else {
      description <- ""
    }
    p(em(description))
  })
  # data
  top <- eventReactive(input$submit_top, {
    if (input$year_input_top == "All Time") {
      results <- krsp_top(kp(pool))
    } else {
      results <- krsp_top(kp(pool), year = input$year_input_top)
    }
    # order properly
    if (input$metric_input_top == "behaviour") {
      results <- arrange(results, desc(n_behaviours), desc(n_trapped),
                         desc(n_collars)) %>%
        mutate(rank = row_number(desc(n_behaviours)))
    } else if (input$metric_input_top == "collars") {
      results <- arrange(results, desc(n_collars), desc(n_trapped),
                         desc(n_behaviours)) %>%
        mutate(rank = row_number(desc(n_collars)))
    } else {
      results <- arrange(results, desc(n_trapped), desc(n_collars),
                         desc(n_behaviours)) %>%
        mutate(rank = row_number(desc(n_trapped)))
    }
    # rank
    results <- select(results, rank, observer,
                      n_trapped, n_collars, n_behaviours)
    # convert to data table
    dt <- datatable(
      results,
      class = 'nowrap stripe compact',
      rownames = FALSE,
      options = list(pageLength = 10,
                     info = FALSE),
      colnames = c(
        "Rank" = "rank",
        "Squirreler" = "observer",
        "Squirrels Trapped" = "n_trapped",
        "Collars" = "n_collars",
        "Behaviour Observations" = "n_behaviours")) %>%
      formatStyle("Rank", textAlign = "center", fontWeight = "bold") %>%
      formatStyle("Squirreler", textAlign = "left", fontWeight = "bold") %>%
      formatStyle("Squirrels Trapped", textAlign = "center") %>%
      formatStyle("Collars", textAlign = "center") %>%
      formatStyle("Behaviour Observations", textAlign = "center")
    dt
  })
  # data table
  output$table_top = DT::renderDataTable(
    if (!is.null(top())) top() else NULL,
    server = FALSE
  )
})
