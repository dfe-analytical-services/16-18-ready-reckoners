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

  expected_column_names <- c(
    "unique_identifier", "forename", "surname", "gender", "forvus_id",
    "cohort_code", "cohort_name",
    "qualification_code", "qualification_name",
    "subject_code", "subject_name", "size", "qual_id",
    "prior_attainment", "actual_points", "disadvantaged_status"
  )


  user_data <- reactive({
    req(input$upload)

    # 1. validation test 1: check file extension
    ext <- tools::file_ext(input$upload$name)

    validate(
      need(ext == "csv", "Invalid file type; Please use the template .csv file")
    )

    #  if validation test 1 passes, upload the data ready for column name checking
    pupil_data <- vroom::vroom(input$upload$datapath, delim = ",")

    # 2. validation test 2: check column names
    actual_column_names <- colnames(pupil_data)

    validate(
      need(identical(expected_column_names, actual_column_names), "Invalid column name detected or column missing; Please use the template to upload pupil data")
    )

    # 4. if validation test 2 fails the code will exit, but if it passes the code will continue to return the pupil data
    pupil_data <- pupil_data %>% mutate(
      forvus_id = as.character(forvus_id),
      qualification_code = as.character(qualification_code),
      subject_code = as.character(subject_code),
      qual_id = as.character(qual_id),
      size = as.character(size),
      disadvantaged_status = as.integer(disadvantaged_status)
    )

    return(pupil_data)
  })


  output$input_preview <- renderDataTable({
    datatable(
      head(user_data(), input$a),
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
  # ---- User data - joining lookups ----
  # -----------------------------------------------------------------------------------------------------------------------------

  user_data_with_lookup <- reactive({
    req(user_data)

    joined_data <- user_data() %>%
      select(-c(qual_id, cohort_name, qualification_name, subject_name)) %>%
      left_join(
        data$qualid_lookup %>% select(
          qual_id,
          cohort_code, cohort_name,
          qualification_code, qualification_name,
          subject_code, subject_name,
          size
        ),
        by = c("cohort_code", "qualification_code", "subject_code", "size")
      )

    return(joined_data)
  })


  ## 1. EXAM COHORT CHECKS
  ## Does cohort name and cohort code match as expected?

  cohort_check_differences <- reactive({
    req(user_data_with_lookup())

    cohort_differences <- setdiff(
      user_data() %>% select(unique_identifier, cohort_name, cohort_code),
      user_data_with_lookup() %>% select(unique_identifier, cohort_name, cohort_code)
    ) %>%
      left_join(user_data_with_lookup() %>% select(unique_identifier, cohort_name, cohort_code),
        by = "unique_identifier"
      ) %>%
      rename(
        "User cohort name" = cohort_name.x,
        "User cohort code" = cohort_code.x,
        "Updated cohort name" = cohort_name.y,
        "Updated cohort code" = cohort_code.y
      )
  })

  cohort_check_summary <- reactive({
    req(cohort_check_differences())

    cohort_differences_summary <- cohort_check_differences() %>%
      select(-unique_identifier) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$cohort_check_table <- renderDataTable({
    datatable(
      cohort_check_summary(),
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        info = FALSE,
        pageLength = FALSE,
        paging = FALSE
      )
    )
  })

  output$cohort_check_download <- downloadHandler(
    filename = "exam_cohort_check.csv",
    content = function(file) {
      write.csv(cohort_check_differences(), file, row.names = FALSE)
    }
  )

  output$cohort_infobox <- renderInfoBox({
    colour <- "green"
    infobox_text <- "No changes made to user data"
    icon_symbol <- "check"
    if (cohort_check_summary() %>% count() >= 1) {
      colour <- "red"
      infobox_text <- "Changes made to user data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })



  ## 2. QUALIFICATION CHECKS
  ## Does qualification name and qualification code match as expected?

  qualification_check_differences <- reactive({
    req(user_data_with_lookup())

    qualification_differences <- setdiff(
      user_data() %>% select(unique_identifier, qualification_name, qualification_code),
      user_data_with_lookup() %>% select(unique_identifier, qualification_name, qualification_code)
    ) %>%
      left_join(user_data_with_lookup() %>% select(unique_identifier, qualification_name, qualification_code),
        by = "unique_identifier"
      ) %>%
      rename(
        "User qualification name" = qualification_name.x,
        "User qualification code" = qualification_code.x,
        "Updated qualification name" = qualification_name.y,
        "Updated qualification code" = qualification_code.y
      )
  })

  qualification_check_summary <- reactive({
    req(qualification_check_differences())

    qualification_differences_summary <- qualification_check_differences() %>%
      select(-unique_identifier) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$qualification_check_table <- renderDataTable({
    datatable(
      qualification_check_summary(),
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        info = FALSE,
        pageLength = FALSE,
        paging = FALSE
      )
    )
  })

  output$qualification_check_download <- downloadHandler(
    filename = "qualification_check.csv",
    content = function(file) {
      write.csv(qualification_check_differences(), file, row.names = FALSE)
    }
  )

  output$qualification_infobox <- renderInfoBox({
    colour <- "green"
    infobox_text <- "No changes made to user data"
    icon_symbol <- "check"
    if (qualification_check_summary() %>% count() >= 1) {
      colour <- "red"
      infobox_text <- "Changes made to user data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })



  ## 3. SUBJECT CHECKS
  ## Does subject name and subject code match as expected?

  subject_check_differences <- reactive({
    req(user_data_with_lookup())

    subject_differences <- setdiff(
      user_data() %>% select(unique_identifier, subject_name, subject_code),
      user_data_with_lookup() %>% select(unique_identifier, subject_name, subject_code)
    ) %>%
      left_join(user_data_with_lookup() %>% select(unique_identifier, subject_name, subject_code),
        by = "unique_identifier"
      ) %>%
      rename(
        "User subject name" = subject_name.x,
        "User subject code" = subject_code.x,
        "Updated subject name" = subject_name.y,
        "Updated subject code" = subject_code.y
      )
  })

  subject_check_summary <- reactive({
    req(subject_check_differences())

    subject_differences_summary <- subject_check_differences() %>%
      select(-unique_identifier) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$subject_check_table <- renderDataTable({
    datatable(
      subject_check_summary(),
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        info = FALSE,
        pageLength = FALSE,
        paging = FALSE
      )
    )
  })

  output$subject_check_download <- downloadHandler(
    filename = "subject_check.csv",
    content = function(file) {
      write.csv(subject_check_differences(), file, row.names = FALSE)
    }
  )

  output$subject_infobox <- renderInfoBox({
    colour <- "green"
    infobox_text <- "No changes made to user data"
    icon_symbol <- "check"
    if (subject_check_summary() %>% count() >= 1) {
      colour <- "red"
      infobox_text <- "Changes made to user data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })



  ## 4. QUALID CHECKS
  ## Does QUALID in user data match lookup as expected?

  qualid_check_differences <- reactive({
    req(user_data_with_lookup())

    qualid_differences <- setdiff(
      user_data() %>% select(unique_identifier, qual_id),
      user_data_with_lookup() %>% select(unique_identifier, qual_id)
    ) %>%
      left_join(user_data_with_lookup() %>% select(unique_identifier, qual_id),
        by = "unique_identifier"
      ) %>%
      rename(
        "User qualification ID" = qual_id.x,
        "Updated qualification ID" = qual_id.y
      )
  })

  qualid_check_summary <- reactive({
    req(qualid_check_differences())

    qualid_differences_summary <- qualid_check_differences() %>%
      select(-unique_identifier) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$qualid_check_table <- renderDataTable({
    datatable(
      qualid_check_summary(),
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        info = FALSE,
        pageLength = FALSE,
        paging = FALSE
      )
    )
  })

  output$qualid_check_download <- downloadHandler(
    filename = "qualid_check.csv",
    content = function(file) {
      write.csv(qualid_check_differences(), file, row.names = FALSE)
    }
  )

  output$qualid_infobox <- renderInfoBox({
    colour <- "green"
    infobox_text <- "No changes made to user data"
    icon_symbol <- "check"
    if (qualid_check_summary() %>% count() >= 1) {
      colour <- "red"
      infobox_text <- "Changes made to user data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })




  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- User data - pupil value added ----
  # -----------------------------------------------------------------------------------------------------------------------------

  minpositive <- function(x) min(x[x >= 0])

  ## 1. determine the upper and lower bands each pupil prior attainment falls between

  ## 1.a. pivot the user data into long format
  ## 1.b calculate the difference between pupil prior attainment and each x band
  pupil_pava_bands <- reactive({
    req(user_data_with_lookup())

    user_data_with_lookup() %>%
      select(-c(qualification_name, subject_name, cohort_name)) %>%
      left_join(data$national_bands, by = "qual_id") %>%
      pivot_longer(
        cols = starts_with(c("x", "y")),
        cols_vary = "slowest",
        names_to = c(".value", "band"),
        names_sep = "_"
      ) %>%
      mutate(difference_prior_x = prior_attainment - x)
  })


  ## 1.c. identify which band has the smallest positive difference and set to TRUE
  ## 1.d. in some instances there may be multiple lower bands (all with the same value) so we want to select the highest possible band to be lower_band
  ## 1.e. we can then define upper_band to be lower_band plus 1
  pupil_pava_bands_flags <- reactive({
    req(pupil_pava_bands())

    pupil_pava_bands() %>%
      group_by(unique_identifier) %>%
      mutate(smallest_positive_difference = difference_prior_x == minpositive(difference_prior_x)) %>%
      filter(smallest_positive_difference == TRUE) %>%
      slice_max(band) %>%
      select(unique_identifier, lower_band = band) %>%
      mutate(upper_band = as.character(as.numeric(lower_band) + 1)) %>%
      ungroup()
  })


  ## 1.f. join the two tables and filter band to only include lower and upper band values
  ## 1.g. this should leave two rows per pupil, with the lower and upper x and y values from the pava
  pupil_pava_bands_filtered <- reactive({
    req(pupil_pava_bands_flags())

    pupil_pava_bands() %>%
      inner_join(pupil_pava_bands_flags(), by = "unique_identifier") %>%
      filter(band == lower_band | band == upper_band) %>%
      mutate(band = as.numeric(band)) %>%
      arrange(unique_identifier, band)
  })


  ## calculating estimated points and value added for each pupil
  pupil_va <- reactive({
    req(pupil_pava_bands_filtered())

    pupil_pava_bands_filtered() %>%
      group_by(unique_identifier) %>%
      mutate(numbering = row_number()) %>%
      mutate(band_position = case_when(
        numbering == 1 ~ "lower",
        numbering == 2 ~ "upper",
        TRUE ~ "error"
      )) %>%
      select(-c(band, numbering)) %>%
      pivot_wider(
        names_from = band_position,
        values_from = c(x, y, difference_prior_x)
      ) %>%
      ungroup() %>%
      mutate(
        delta_x = x_upper - x_lower,
        delta_y = y_upper - y_lower,
        estimated_points = (delta_y / delta_x) * (prior_attainment - x_lower) + y_lower,
        value_added = actual_points - estimated_points
      ) %>%
      select(all_of(expected_column_names), estimated_points, value_added) %>%
      left_join(data$subject_variance %>% select(qual_id, subj_weighting, weighting), by = "qual_id") %>%
      mutate(
        value_added_subj_weight = value_added * (subj_weighting / as.numeric(size)),
        value_added_qual_weight = value_added * (weighting / as.numeric(size))
      )
  })



  ## calculating value added and confidence intervals for each qualification/subject combination (each qual_id)
  subject_va <- reactive({
    req(pupil_va())

    pupil_va() %>%
      group_by(
        cohort_code, cohort_name, qualification_code, qualification_name,
        subject_code, subject_name, size, qual_id
      ) %>%
      summarise(
        student_count = n(),
        subject_va_pt1 = mean(value_added)
      ) %>%
      left_join(data$subject_variance %>% select(qual_id, sd_suqu), by = "qual_id") %>%
      mutate(
        subject_va_grade = subject_va_pt1 / 10 / as.numeric(size),
        standard_error = sd_suqu / sqrt(student_count),
        lower_confidence_interval = subject_va_grade - (1.96 * standard_error),
        upper_confidence_interval = subject_va_grade + (1.96 * standard_error)
      ) %>%
      ungroup()
  })

  output$student_va_scores <- renderDataTable({
    datatable(
      head(
        pupil_va() %>%
          select(
            forename, surname, cohort_name, qualification_name, subject_name, size, qual_id,
            prior_attainment, actual_points, estimated_points, value_added
          ) %>%
          mutate(
            prior_attainment = round2(prior_attainment, 4),
            estimated_points = round2(estimated_points, 4),
            value_added = round2(value_added, 4)
          ),
        input$n
      ),
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
        choices <- user_data_with_lookup() %>%
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
        choices <- user_data_with_lookup() %>%
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
        choices <- user_data_with_lookup() %>%
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
        choices <- user_data_with_lookup() %>%
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

    user_chart_data <- user_data_with_lookup() %>%
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



  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- SUBJECT CHART VA DATA ----
  # -----------------------------------------------------------------------------------------------------------------------------



  ## extract the relevant national data for the subject comparison chart based on the reactive qual_id
  number_of_entries <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- subject_va() %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(student_count) %>%
      pull()

    return(n)
  })


  output$entries <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0(number_of_entries()),
      # add subtitle to explain what it's showing
      paste0("Number of pupils"),
      color = "blue"
    )
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
