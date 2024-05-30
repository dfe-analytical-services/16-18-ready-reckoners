# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your
# app.
#
# Anything you create in the server file won't appear in your app until you call
# it in the UI file. This server script gives an example of a plot and value box
# that updates on slider input. There are many other elements you can add in
# too, and you can play around with their reactivity. The "outputs" section of
# the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run th
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  # Loading screen -------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  setBookmarkExclude(c(
    "cookies", "link_to_app_content_tab",
    "tabBenchmark_rows_current", "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected", "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected", "tabBenchmark_search",
    "tabBenchmark_rows_selected", "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A", "plotly_hover-A", "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts"
  ))

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  observe({
    if (input$navlistPanel == "dashboard") {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$selectPhase, ", ",
          input$selectArea
        )
      )
    } else {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$navlistPanel
        )
      )
    }
  })

  observeEvent(input$cookies, {
    if (!is.null(input$cookies)) {
      if (!("dfe_analytics" %in% names(input$cookies))) {
        shinyjs::show(id = "cookieMain")
      } else {
        shinyjs::hide(id = "cookieMain")
        msg <- list(
          name = "dfe_analytics",
          value = input$cookies$dfe_analytics
        )
        session$sendCustomMessage("analytics-consent", msg)
        if ("cookies" %in% names(input)) {
          if ("dfe_analytics" %in% names(input$cookies)) {
            if (input$cookies$dfe_analytics == "denied") {
              ga_msg <- list(name = paste0("_ga_", google_analytics_key))
              session$sendCustomMessage("cookie-remove", ga_msg)
            }
          }
        }
      }
    } else {
      shinyjs::hide(id = "cookieMain")
    }
  })

  # Need these set of observeEvent to create a path through the cookie banner
  observeEvent(input$cookieAccept, {
    msg <- list(
      name = "dfe_analytics",
      value = "granted"
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    shinyjs::show(id = "cookieAcceptDiv")
    shinyjs::hide(id = "cookieMain")
  })

  observeEvent(input$cookieReject, {
    msg <- list(
      name = "dfe_analytics",
      value = "denied"
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    shinyjs::show(id = "cookieRejectDiv")
    shinyjs::hide(id = "cookieMain")
  })

  observeEvent(input$hideAccept, {
    shinyjs::toggle(id = "cookieDiv")
  })

  observeEvent(input$hideReject, {
    shinyjs::toggle(id = "cookieDiv")
  })

  observeEvent(input$remove, {
    shinyjs::toggle(id = "cookieMain")
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
    print(input$cookies)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools,
    this site uses cookies to identify numbers of unique users as part of Google
    Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      "Cookies consent has not been confirmed."
    }
  })

  observeEvent(input$cookieLink, {
    # Need to link here to where further info is located.  You can
    # updateTabsetPanel to have a cookie page for instance
    updateTabsetPanel(session, "navlistPanel",
      selected = "Support and feedback"
    )
  })


  #  output$cookie_status <- renderText(as.character(input$cookies))

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- User data upload ----
  # -----------------------------------------------------------------------------------------------------------------------------

  user_data <- reactive({
    req(input$upload)

    # 1. validation test 1: check file extension
    ext <- tools::file_ext(input$upload$name)

    validate(
      need(ext == "csv", "Invalid file type; Please use the template .csv file")
    )

    # 2. if validation test 1 passes, upload the data ready for column name checking
    pupil_data <- vroom::vroom(input$upload$datapath, delim = ",")

    # 3. validation test 2: check column names
    expected_column_names <- c(
      "forename", "surname", "gender", "qualification_code", "qualification_name", "subject_code",
      "subject_name", "size", "cohort_name", "prior_attainment", "estimated_points", "actual_points",
      "value_added_score", "qual_id", "disadvantaged_status", "forvus_id", "laestab"
    )
    actual_column_names <- colnames(pupil_data)

    validate(
      need(identical(expected_column_names, actual_column_names), "Invalid column name detected or column missing; Please use the template to upload pupil data")
    )

    # 4. if validation test 2 fails the code will exit, but if it passes the code will continue to return the pupil data
    pupil_data <- pupil_data %>% mutate(
      qualification_code = as.character(qualification_code),
      subject_code = as.character(subject_code),
      qual_id = as.character(qual_id),
      disadvantaged_status = as.integer(disadvantaged_status),
      forvus_id = as.character(forvus_id),
      laestab = as.character(laestab)
    )

    return(pupil_data)
  })


  output$input_preview <- renderDataTable({
    datatable(
      head(user_data(), input$n),
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        info = FALSE,
        pageLength = FALSE,
        paging = FALSE
      )
    )
  })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Dropdown boxes ----
  # -----------------------------------------------------------------------------------------------------------------------------


  # ---- NATIONAL ----
  observe(if (input$data_source == "National data only") {
    observe({
      updateSelectInput(session,
        inputId = "dropdown_cohort",
        label = NULL,
        choices <- data$qualid_lookup %>%
          select(cohort_name) %>%
          distinct() %>%
          pull(cohort_name) %>%
          sort(.)
      )
    })
    observe({
      updateSelectInput(session,
        inputId = "dropdown_qualifications",
        label = NULL,
        choices <- data$qualid_lookup %>%
          select(cohort_name, qualification_name) %>%
          distinct() %>%
          filter(cohort_name == input$dropdown_cohort) %>%
          pull(qualification_name) %>%
          sort(.)
      )
    })
    observe({
      updateSelectInput(session,
        inputId = "dropdown_subjects",
        label = NULL,
        choices <- data$qualid_lookup %>%
          select(cohort_name, qualification_name, subject_name) %>%
          distinct() %>%
          filter(
            cohort_name == input$dropdown_cohort,
            qualification_name == input$dropdown_qualifications
          ) %>%
          pull(subject_name) %>%
          sort(.)
      )
    })
    observe({
      updateSelectInput(session,
        inputId = "dropdown_sizes",
        label = NULL,
        choices <- data$qualid_lookup %>%
          select(cohort_name, qualification_name, subject_name, size) %>%
          distinct() %>%
          filter(
            cohort_name == input$dropdown_cohort,
            qualification_name == input$dropdown_qualifications,
            subject_name == input$dropdown_subjects
          ) %>%
          pull(size) %>%
          sort(.)
      )
    })
  } else {
    observe({
      updateSelectInput(session,
        inputId = "dropdown_cohort",
        label = NULL,
        choices <- user_data() %>%
          select(cohort_name) %>%
          distinct() %>%
          pull(cohort_name) %>%
          sort(.)
      )
    })
    observe({
      updateSelectInput(session,
        inputId = "dropdown_qualifications",
        label = NULL,
        choices <- user_data() %>%
          select(cohort_name, qualification_name) %>%
          distinct() %>%
          filter(cohort_name == input$dropdown_cohort) %>%
          pull(qualification_name) %>%
          sort(.)
      )
    })
    observe({
      updateSelectInput(session,
        inputId = "dropdown_subjects",
        label = NULL,
        choices <- user_data() %>%
          select(cohort_name, qualification_name, subject_name) %>%
          distinct() %>%
          filter(
            cohort_name == input$dropdown_cohort,
            qualification_name == input$dropdown_qualifications
          ) %>%
          pull(subject_name) %>%
          sort(.)
      )
    })
    observe({
      updateSelectInput(session,
        inputId = "dropdown_sizes",
        label = NULL,
        choices <- user_data() %>%
          select(cohort_name, qualification_name, subject_name, size) %>%
          distinct() %>%
          filter(
            cohort_name == input$dropdown_cohort,
            qualification_name == input$dropdown_qualifications,
            subject_name == input$dropdown_subjects
          ) %>%
          pull(size) %>%
          sort(.)
      )
    })
  })





  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- QUAL_ID ----
  # -----------------------------------------------------------------------------------------------------------------------------

  ## create a reactive qual_id based on the selections currently in the drop down boxes

  reactive_qualid <- reactive({
    req(input$dropdown_cohort)
    req(input$dropdown_qualifications)
    req(input$dropdown_subjects)
    req(input$dropdown_sizes)

    data$qualid_lookup %>%
      filter(
        cohort_name == input$dropdown_cohort,
        qualification_name == input$dropdown_qualifications,
        subject_name == input$dropdown_subjects,
        size == input$dropdown_sizes
      ) %>%
      select(qual_id)
  })

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- SUBJECT CHART DATA ----
  # -----------------------------------------------------------------------------------------------------------------------------

  ## extract the relevant national data for the subject comparison chart based on the reactive qual_id
  national_subject_chart_data <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    full_chart_data <- data$national_bands %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(starts_with(c("x", "y"))) %>%
      pivot_longer(
        everything(),
        cols_vary = "slowest",
        names_to = c(".value", "set"),
        names_sep = "_"
      ) %>%
      mutate(source = "national") %>%
      select(-set)

    return(full_chart_data)
  })

  ## extract the relevant national AND institution data for the subject comparison chart based on the reactive qual_id
  user_subject_chart_data <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    line_chart_data <- data$national_bands %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(starts_with(c("x", "y"))) %>%
      pivot_longer(
        everything(),
        cols_vary = "slowest",
        names_to = c(".value", "set"),
        names_sep = "_"
      ) %>%
      mutate(source = "national") %>%
      select(-set)

    user_chart_data <- user_data() %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(x = prior_attainment, y = actual_points) %>%
      mutate(source = "user")

    full_chart_data <- rbind(line_chart_data, user_chart_data)

    return(full_chart_data)
  })



  ## select which input data to use for the chart based on the radio button selected in the app
  subject_chart_data <- reactive(if (input$data_source == "National data only") {
    national_subject_chart_data()
  } else {
    user_subject_chart_data()
  })





  output$subject_chart <- renderPlot({
    req(subject_chart_data())

    max_x <- subject_chart_data() %>%
      select(x) %>%
      max()

    ggplot(subject_chart_data(), aes(x = x, y = y, color = source, shape = source)) +
      geom_line(data = filter(subject_chart_data(), source == "national")) +
      geom_point(data = filter(subject_chart_data(), source == "user"), size = 4) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)
      ) +
      xlab("Prior Attainment (points)") +
      ylab("Outcome Attainment (points)") +
      scale_x_continuous(breaks = seq(0, max_x, by = 2)) +
      scale_colour_manual(values = c("black", "red")) +
      scale_shape_manual(values = c(NA, 4))
  })

















  # reactiveRevBal <- reactive({
  #   dfRevBal %>% filter(
  #     area_name == input$selectArea | area_name == "England",
  #     school_phase == input$selectPhase
  #   )
  # })
  #
  # # Define server logic required to draw a histogram
  # output$lineRevBal <- snapshotPreprocessOutput(
  #   renderGirafe({
  #     girafe(
  #       ggobj = createAvgRevTimeSeries(reactiveRevBal(), input$selectArea),
  #       options = list(opts_sizing(rescale = TRUE, width = 1.0)),
  #       width_svg = 9.6,
  #       height_svg = 5.0
  #     )
  #   }),
  #   function(value) {
  #     # Removing elements that cause issues with shinytest comparisons when run
  #     # on different environments
  #     svg_removed <- gsub(
  #       "svg_[0-9a-z]{8}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{12}",
  #       "svg_random_giraph_string", value
  #     )
  #     font_standardised <- gsub("Arial", "Helvetica", svg_removed)
  #     cleaned_positions <- gsub(
  #       "[a-z]*x[0-9]*='[0-9.]*' [a-z]*y[0-9]*='[0-9.]*'",
  #       "Position", font_standardised
  #     )
  #     cleaned_size <- gsub(
  #       "width='[0-9.]*' height='[0-9.]*'", "Size", cleaned_positions
  #     )
  #     cleaned_points <- gsub("points='[0-9., ]*'", "points", cleaned_size)
  #     cleaned_points
  #   }
  # )
  #
  # reactiveBenchmark <- reactive({
  #   dfRevBal %>%
  #     filter(
  #       area_name %in% c(input$selectArea, input$selectBenchLAs),
  #       school_phase == input$selectPhase,
  #       year == max(year)
  #     )
  # })
  #
  # output$colBenchmark <- snapshotPreprocessOutput(
  #   renderGirafe({
  #     girafe(
  #       ggobj = plotAvgRevBenchmark(reactiveBenchmark()),
  #       options = list(opts_sizing(rescale = TRUE, width = 1.0)),
  #       width_svg = 5.0,
  #       height_svg = 5.0
  #     )
  #   }),
  #   function(value) {
  #     # Removing elements that cause issues with shinytest comparisons when run on
  #     # different environments - should add to dfeshiny at some point.
  #     svg_removed <- gsub(
  #       "svg_[0-9a-z]{8}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{12}",
  #       "svg_random_giraph_string",
  #       value
  #     )
  #     font_standardised <- gsub("Arial", "Helvetica", svg_removed)
  #     cleaned_positions <- gsub(
  #       "x[0-9]*='[0-9.]*' y[0-9]*='[0-9.]*'",
  #       "Position", font_standardised
  #     )
  #     cleaned_size <- gsub(
  #       "width='[0-9.]*' height='[0-9.]*'",
  #       "Size", cleaned_positions
  #     )
  #     cleaned_points <- gsub("points='[0-9., ]*'", "points", cleaned_size)
  #     cleaned_points
  #   }
  # )
  #
  # output$tabBenchmark <- renderDataTable({
  #   datatable(
  #     reactiveBenchmark() %>%
  #       select(
  #         Area = area_name,
  #         `Average Revenue Balance (£)` = average_revenue_balance,
  #         `Total Revenue Balance (£m)` = total_revenue_balance_million
  #       ),
  #     options = list(
  #       scrollX = TRUE,
  #       paging = FALSE
  #     )
  #   )
  # })
  #
  # # Define server logic to create a box
  #
  # output$boxavgRevBal <- renderValueBox({
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(
  #       (reactiveRevBal() %>% filter(
  #         year == max(year),
  #         area_name == input$selectArea,
  #         school_phase == input$selectPhase
  #       ))$average_revenue_balance,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the latest value for the selected inputs"),
  #     color = "blue"
  #   )
  # })
  #
  # output$boxpcRevBal <- renderValueBox({
  #   latest <- (reactiveRevBal() %>% filter(
  #     year == max(year),
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #   penult <- (reactiveRevBal() %>% filter(
  #     year == max(year) - 1,
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(latest - penult,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the change on previous year"),
  #     color = "blue"
  #   )
  # })
  #
  # output$boxavgRevBal_small <- renderValueBox({
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(
  #       (reactiveRevBal() %>% filter(
  #         year == max(year),
  #         area_name == input$selectArea,
  #         school_phase == input$selectPhase
  #       ))$average_revenue_balance,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the latest value for the selected inputs"),
  #     color = "orange",
  #     fontsize = "small"
  #   )
  # })
  #
  # output$boxpcRevBal_small <- renderValueBox({
  #   latest <- (reactiveRevBal() %>% filter(
  #     year == max(year),
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #   penult <- (reactiveRevBal() %>% filter(
  #     year == max(year) - 1,
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(latest - penult,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the change on previous year"),
  #     color = "orange",
  #     fontsize = "small"
  #   )
  # })
  #
  # output$boxavgRevBal_large <- renderValueBox({
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(
  #       (reactiveRevBal() %>% filter(
  #         year == max(year),
  #         area_name == input$selectArea,
  #         school_phase == input$selectPhase
  #       ))$average_revenue_balance,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the latest value for the selected inputs"),
  #     color = "green",
  #     fontsize = "large"
  #   )
  # })
  #
  # output$boxpcRevBal_large <- renderValueBox({
  #   latest <- (reactiveRevBal() %>% filter(
  #     year == max(year),
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #   penult <- (reactiveRevBal() %>% filter(
  #     year == max(year) - 1,
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(latest - penult,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the change on previous year"),
  #     color = "green",
  #     fontsize = "large"
  #   )
  # })
  #
  # observeEvent(input$go, {
  #   toggle(id = "div_a", anim = T)
  # })
  #
  #
  # observeEvent(input$link_to_app_content_tab, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  # })
  #
  # # Download the underlying data button
  # output$download_data <- downloadHandler(
  #   filename = "shiny_template_underlying_data.csv",
  #   content = function(file) {
  #     write.csv(dfRevBal, file)
  #   }
  # )
  #
  # # Add input IDs here that are within the relevant drop down boxes to create
  # # dynamic text
  # output$dropdown_label <- renderText({
  #   paste0("Current selections: ", input$selectPhase, ", ", input$selectArea)
  # })


  # Stop app -------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
