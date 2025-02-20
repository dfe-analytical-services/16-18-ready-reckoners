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

  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

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

  observeEvent(input$link_to_user_upload_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "data_upload_dashboard")
  })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER DATA UPLOAD - OUTPUT IN THE DATA UPLOAD TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------

  expected_column_names <- c(
    "Forename", "Surname", "Sex",
    "Qualification code", "Qualification name",
    "Subject code", "Subject name",
    "Size", "Cohort",
    "Prior attainment", "Estimated points (new points)", "Actual points (new points)", "Value Added score",
    "QUAL_ID", "Disadvantaged status", "CYPMD ID", "DfE number"
  )


  user_data <- reactive({
    req(input$upload)

    # 1. validation test 1: check file extension
    ext <- tools::file_ext(input$upload$name)

    validate(
      need(ext == "csv", "Invalid file type; Please use the template .csv file")
    )

    #  if validation test 1 passes, upload the data ready for column name checking
    student_data <- vroom::vroom(input$upload$datapath, delim = ",")

    # 2. validation test 2: check column names
    actual_column_names <- colnames(student_data)

    validate(
      need(identical(expected_column_names, actual_column_names), "Invalid column name detected or column missing; Please use the template to upload pupil data")
    )


    # 3. validation test 3: check numerical unique identifiers
    # unique_identifier_type_check <- student_data %>%
    #   mutate(unique_identifier = as.numeric(unique_identifier)) %>%
    #   filter(is.na(unique_identifier))
    #
    # validate(
    #   need(nrow(unique_identifier_type_check) == 0, "Non-numerical unique identifiers detected in pupil data. Please amend the unique identifiers used to only include numerical values and re-upload.")
    # )
    #
    # # 4. validation test 4: check unique identifiers are unique
    # unique_identifier_value_check <- student_data %>% distinct(unique_identifier)
    #
    # validate(
    #   need(nrow(student_data) == nrow(unique_identifier_value_check), "Duplicate unique identifiers detected in pupil data. Please amend the unique identifiers used to only include unique numerical values and re-upload.")
    # )


    # 5. if validation test 2 or 3 fails the code will exit, but if they pass the code will continue to return the pupil data
    student_data <- student_data %>%
      rename_with(., ~ to_snake_case(.x)) %>%
      rename(
        cohort_name = cohort,
        actual_points = actual_points_new_points,
        estimated_points_user_input = estimated_points_new_points,
        value_added_score_user_input = value_added_score
      ) %>%
      mutate(
        qualification_code = as.character(qualification_code),
        subject_code = as.character(subject_code),
        qual_id = as.character(qual_id),
        size = as.character(size),
        disadvantaged_status = as.integer(disadvantaged_status),
        row_id = row_number(),
        # cohort_name = tolower(cohort_name),
        cohort_code = case_when(
          tolower(cohort_name) == "a level" ~ "1",
          tolower(cohort_name) == "academic" ~ "2",
          tolower(cohort_name) == "applied general" ~ "3",
          qualification_code == "699" ~ "4",
          tolower(cohort_name) == "tech level" ~ "5",
          tolower(cohort_name) == "technical certificate" ~ "6",
          TRUE ~ "unknown"
        )
      )


    return(student_data)
  })


  output$input_preview <- renderReactable({
    validate(need(input$a, message = "To preview data, please select number of rows needed."))

    reactable(
      head(user_data(), input$a)
    )
  })

  # output$input_preview <- renderDataTable({
  #   validate(need(input$a, message = "To preview data, please select number of rows needed."))
  #
  #   datatable(
  #     head(user_data(), input$a),
  #     options = list(
  #       scrollX = TRUE,
  #       scrollY = "250px",
  #       info = FALSE,
  #       pageLength = FALSE,
  #       paging = FALSE
  #     )
  #   )
  # })

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- UNDERLYING DATA DOWNLOADS - OUTPUT IN THE DATA UPLOAD TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------

  output$model_data_download <- downloadHandler(
    filename = "national_model_data.csv",
    content = function(file) {
      write.csv(data$national_bands, file, row.names = FALSE)
    }
  )

  output$subject_variance_download <- downloadHandler(
    filename = "subject_variance_data.csv",
    content = function(file) {
      write.csv(data$subject_variance, file, row.names = FALSE)
    }
  )

  output$disadvantaged_subject_variance_download <- downloadHandler(
    filename = "disadvantaged_subject_variance_data.csv",
    content = function(file) {
      write.csv(data$disadvantaged_subject_variance, file, row.names = FALSE)
    }
  )

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER TEMPLATES - OUTPUT IN THE DATA UPLOAD TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------

  output$student_data_template_download <- downloadHandler(
    filename = "student_data_template.csv",
    content = function(file) {
      write.csv(template_data, file, row.names = FALSE)
    }
  )

  output$qualid_lookup_download <- downloadHandler(
    filename = "qualification_lookup.csv",
    content = function(file) {
      write.csv(data$qualid_lookup, file, row.names = FALSE)
    }
  )

  output$qan_lookup_download <- downloadHandler(
    filename = "qan_lookup.csv",
    content = function(file) {
      write.csv(data$qan_lookup, file, row.names = FALSE)
    }
  )


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER "DATA MISSING" MESSAGES ----
  # -----------------------------------------------------------------------------------------------------------------------------

  ## for use in the data checking tab (displayed at top)
  missing_upload_message1 <- reactive({
    if (is.null(input$upload)) {
      # text <- paste(icon("triangle-exclamation"), "<font color=\"#C65102\"><b>", "Please upload your student data", "</b></font>")
      text <- paste("<font color=\"#C65102\"><b>", "Please upload your student data.", "</b></font>")
    }
  })

  output$no_user_data1 <- renderText({
    missing_upload_message1()
  })




  ## for use in the student value added table tab (displayed at top)
  missing_upload_message2 <- reactive({
    if (is.null(input$upload)) {
      text <- paste("<font color=\"#C65102\"><b>", "Please upload your student data.", "</b></font>")
    } else {
      text <- paste("Download your institution data with individual value added scores calculated for each student:")
    }
  })

  output$no_user_data2 <- renderText({
    missing_upload_message2()
  })


  ## for use in the subject chart tab (to replace table when national and student data radio button is selected)
  missing_upload_message3 <- reactive({
    if (is.null(input$upload) & input$data_source == "National and student data") {
      text <- paste("<font color=\"#C65102\"><b>", "Please upload your student data.", "</b></font>")
    }
  })

  output$no_user_data3 <- renderText({
    missing_upload_message3()
  })


  ## for use in the cohort value added tab (displayed at top)
  missing_upload_message4 <- reactive({
    if (is.null(input$upload)) {
      text <- paste("<font color=\"#C65102\"><b>", "Please upload your student data.", "</b></font>")
    } else {
      text <- paste("Download your institution data with headline value added scores calculated:")
    }
  })

  output$no_user_data4 <- renderText({
    missing_upload_message4()
  })

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER DATA CREATING ACADEMIC ----
  # -----------------------------------------------------------------------------------------------------------------------------

  user_data_max_unique_id <- reactive({
    req(user_data())

    user_data() %>%
      select(row_id) %>%
      max()
  })


  user_data_academic <- reactive({
    req(user_data())

    user_data() %>%
      filter(cohort_code == "1") %>%
      mutate(
        cohort_code = "2",
        cohort_name = "Academic",
        qual_id = paste0(cohort_code, substr(qual_id, 2, nchar(qual_id))),
        row_id = row_number() + user_data_max_unique_id()
      ) %>%
      bind_rows(user_data())
  })

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER DATA JOINING LOOKUPS ----
  # -----------------------------------------------------------------------------------------------------------------------------

  user_data_with_lookup <- reactive({
    req(user_data())

    joined_data <- user_data_academic() %>%
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
      ) %>%
      mutate(across(c(qual_id, cohort_name, qualification_name, subject_name), ~ replace_na(., "Removed")))

    return(joined_data)
  })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER DATA CHECKS - OUTPUT IN THE DATA CHECKS TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------
  # colours for infoboxes
  # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.


  ## 1. REMOVED DATA
  ## Do all of the exam cohort, qualification code, subject code and size combinations entered by the user exist?

  removed_check <- reactive({
    req(user_data_with_lookup())

    user_data_with_lookup() %>%
      left_join(user_data_academic() %>% select(row_id, cohort_name),
        by = "row_id"
      ) %>%
      filter(qual_id == "Removed") %>%
      select(row_id, cohort_name.y, cohort_code, qualification_code, subject_code, size)
  })

  removed_check_summary <- reactive({
    req(removed_check())

    removed_summary <- removed_check() %>%
      select(-row_id, -cohort_code) %>%
      count(pick(everything())) %>%
      rename(
        "User cohort name" = cohort_name.y,
        "User qualification code" = qualification_code,
        "User subject code" = subject_code,
        "User size" = size,
        "Number of rows removed" = n
      )
  })

  output$removed_table <- renderReactable({
    reactable(
      removed_check_summary()
    )
  })

  # output$removed_table <- renderDataTable({
  #   datatable(
  #     removed_check_summary(),
  #     options = list(
  #       scrollX = TRUE,
  #       scrollY = "250px",
  #       info = FALSE,
  #       pageLength = FALSE,
  #       paging = FALSE
  #     )
  #   )
  # })

  output$removed_download <- downloadHandler(
    filename = "removed_check.csv",
    content = function(file) {
      write.csv(removed_check(), file, row.names = FALSE)
    }
  )

  output$removed_infobox <- renderInfoBox({
    colour <- "olive"
    infobox_text <- "No changes made to student data"
    icon_symbol <- "check"
    if (removed_check_summary() %>% count() >= 1) {
      colour <- "maroon"
      infobox_text <- "Data has been removed"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })


  ## 2. EXAM COHORT CHECKS
  ## Are there any cohort names the app does not recognise?

  # cohort_check_differences <- reactive({
  #   req(user_data_with_lookup())
  #
  #   cohort_differences <- user_data_academic() %>%
  #     filter(cohort_code == "unknown") %>%
  #     select(row_id, cohort_name) %>%
  #     rename("User cohort name" = cohort_name)
  # })
  #
  # cohort_check_summary <- reactive({
  #   req(cohort_check_differences())
  #
  #   cohort_differences_summary <- cohort_check_differences() %>%
  #     select(-row_id) %>%
  #     count(pick(everything())) %>%
  #     rename("Number of rows updated" = n)
  # })
  #
  # output$cohort_check_table <- renderReactable({
  #   reactable(
  #     cohort_check_summary()
  #   )
  # })
  #
  # output$cohort_check_download <- downloadHandler(
  #   filename = "exam_cohort_check.csv",
  #   content = function(file) {
  #     write.csv(cohort_check_differences(), file, row.names = FALSE)
  #   }
  # )
  #
  # output$cohort_infobox <- renderInfoBox({
  #   colour <- "olive"
  #   infobox_text <- "No changes made to student data"
  #   icon_symbol <- "check"
  #   if (cohort_check_summary() %>% count() >= 1) {
  #     colour <- "maroon"
  #     infobox_text <- "Data has been removed"
  #     icon_symbol <- "exclamation"
  #   }
  #   infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  # })



  ## 3. QUALIFICATION CHECKS
  ## Does qualification name and qualification code match as expected?

  qualification_check_differences <- reactive({
    req(user_data_with_lookup())

    qualification_differences <- setdiff(
      user_data_academic() %>% select(row_id, qualification_name, qualification_code),
      user_data_with_lookup() %>% select(row_id, qualification_name, qualification_code)
    ) %>%
      left_join(user_data_with_lookup() %>% select(row_id, qualification_name, qualification_code),
        by = "row_id"
      ) %>%
      filter(qualification_name.y != "Removed") %>%
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
      select(-row_id) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$qualification_check_table <- renderReactable({
    reactable(
      qualification_check_summary()
    )
  })

  output$qualification_check_download <- downloadHandler(
    filename = "qualification_check.csv",
    content = function(file) {
      write.csv(qualification_check_differences(), file, row.names = FALSE)
    }
  )

  output$qualification_infobox <- renderInfoBox({
    colour <- "olive"
    infobox_text <- "No changes made to student data"
    icon_symbol <- "check"
    if (qualification_check_summary() %>% count() >= 1) {
      colour <- "maroon"
      infobox_text <- "Changes made to student data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })



  ## 4. SUBJECT CHECKS
  ## Does subject name and subject code match as expected?

  subject_check_differences <- reactive({
    req(user_data_with_lookup())

    subject_differences <- setdiff(
      user_data_academic() %>% select(row_id, subject_name, subject_code),
      user_data_with_lookup() %>% select(row_id, subject_name, subject_code)
    ) %>%
      left_join(user_data_with_lookup() %>% select(row_id, subject_name, subject_code),
        by = "row_id"
      ) %>%
      filter(subject_name.y != "Removed") %>%
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
      select(-row_id) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$subject_check_table <- renderReactable({
    reactable(
      subject_check_summary()
    )
  })

  output$subject_check_download <- downloadHandler(
    filename = "subject_check.csv",
    content = function(file) {
      write.csv(subject_check_differences(), file, row.names = FALSE)
    }
  )

  output$subject_infobox <- renderInfoBox({
    colour <- "olive"
    infobox_text <- "No changes made to student data"
    icon_symbol <- "check"
    if (subject_check_summary() %>% count() >= 1) {
      colour <- "maroon"
      infobox_text <- "Changes made to student data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })



  ## 5. QUALID CHECKS
  ## Does QUALID in user data match lookup as expected?

  qualid_check_differences <- reactive({
    req(user_data_with_lookup())

    qualid_differences <- setdiff(
      user_data_academic() %>% select(row_id, qual_id),
      user_data_with_lookup() %>% select(row_id, qual_id)
    ) %>%
      left_join(user_data_with_lookup() %>% select(row_id, qual_id),
        by = "row_id"
      ) %>%
      filter(qual_id.y != "Removed") %>%
      rename(
        "User qualification ID" = qual_id.x,
        "Updated qualification ID" = qual_id.y
      )
  })

  qualid_check_summary <- reactive({
    req(qualid_check_differences())

    qualid_differences_summary <- qualid_check_differences() %>%
      select(-row_id) %>%
      count(pick(everything())) %>%
      rename("Number of rows updated" = n)
  })

  output$qualid_check_table <- renderReactable({
    reactable(
      qualid_check_summary()
    )
  })

  output$qualid_check_download <- downloadHandler(
    filename = "qualid_check.csv",
    content = function(file) {
      write.csv(qualid_check_differences(), file, row.names = FALSE)
    }
  )

  output$qualid_infobox <- renderInfoBox({
    colour <- "olive"
    infobox_text <- "No changes made to student data"
    icon_symbol <- "check"
    if (qualid_check_summary() %>% count() >= 1) {
      colour <- "maroon"
      infobox_text <- "Changes made to student data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })


  ## 6. PRIOR ATTAINMENT CHECKS
  ## Does PRIOR ATTAINMENT in user data exceed the prior attainment in the maximum pava band for that subject?

  prioratt_exceeds_upper <- reactive({
    req(user_data_with_lookup())

    prioratt_exceeds_upper <- user_data_academic() %>%
      select(-c(qualification_name, subject_name, cohort_name)) %>%
      left_join(data$national_bands, by = "qual_id") %>%
      filter(prior_attainment > x_21) %>%
      mutate(x_0 = "-")
  })

  prioratt_exceeds_lower <- reactive({
    req(user_data_with_lookup())

    prioratt_exceeds_lower <- user_data_academic() %>%
      select(-c(qualification_name, subject_name, cohort_name)) %>%
      left_join(data$national_bands, by = "qual_id") %>%
      filter(prior_attainment < x_0) %>%
      mutate(x_21 = "-")
  })

  prioratt_check_differences <- reactive({
    req(prioratt_exceeds_upper())
    req(prioratt_exceeds_lower())

    prioratt_check_differences <- rbind(prioratt_exceeds_upper(), prioratt_exceeds_lower()) %>%
      arrange(row_id)
  })

  prioratt_check_summary <- reactive({
    req(prioratt_check_differences())

    prioratt_differences_summary <- prioratt_check_differences() %>%
      select(row_id, qual_id, prior_attainment, x_21, x_0) %>%
      rename(
        "Pupil unique identifier" = row_id,
        "User qualification ID" = qual_id,
        "Pupil prior attainment" = prior_attainment,
        "Value added model max prior attainment" = x_21,
        "Value added model min prior attainment" = x_0
      )
  })

  output$prioratt_check_table <- renderReactable({
    reactable(
      prioratt_check_summary()
    )
  })

  output$prioratt_check_download <- downloadHandler(
    filename = "prioratt_check.csv",
    content = function(file) {
      write.csv(prioratt_check_differences(), file, row.names = FALSE)
    }
  )

  output$prioratt_infobox <- renderInfoBox({
    colour <- "olive"
    infobox_text <- "No changes made to student data"
    icon_symbol <- "check"
    if (prioratt_check_summary() %>% count() >= 1) {
      colour <- "maroon"
      infobox_text <- "Changes made to student data"
      icon_symbol <- "exclamation"
    }
    infoBox(value = infobox_text, title = "Summary", color = colour, icon = icon(icon_symbol))
  })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- USER DATA FINAL ----
  # -----------------------------------------------------------------------------------------------------------------------------

  user_data_final <- reactive({
    req(user_data_with_lookup)
    req(prioratt_check_differences)

    final_data <- user_data_with_lookup() %>%
      filter(
        qual_id != "Removed",
        cohort_name != "Removed",
        qualification_name != "Removed",
        subject_name != "Removed",
        row_id %not_in% c(prioratt_exceeds_upper()$row_id),
        row_id %not_in% c(prioratt_exceeds_lower()$row_id)
      )

    return(final_data)
  })





  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- VA DERIVATIONS - BY STUDENT - OUTPUT IN THE STUDENT VALUE ADDED TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------

  common_column_names <- c(
    "row_id", "forename", "surname", "sex",
    "cohort_code", "cohort_name",
    "qualification_code", "qualification_name",
    "subject_code", "subject_name", "size", "qual_id",
    "prior_attainment", "actual_points", "disadvantaged_status"
  )

  minpositive <- function(x) min(x[x >= 0])

  ## 1. determine the upper and lower bands each pupil prior attainment falls between

  ## 1.a. pivot the user data into long format
  ## 1.b calculate the difference between pupil prior attainment and each x band
  student_pava_bands_full <- reactive({
    req(user_data_final())

    user_data_final() %>%
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


  student_pava_bands <- reactive({
    req(user_data_final())

    user_data_final() %>%
      select(-c(qualification_name, subject_name, cohort_name)) %>%
      left_join(data$national_bands, by = "qual_id")
  })


  ## 1.c. identify which band has the smallest positive difference and set to TRUE
  ## 1.d. in some instances there may be multiple lower bands (all with the same value) so we want to select the highest possible band to be lower_band
  ## 1.e. we can then define upper_band to be lower_band plus 1


  pupil_pava_bands_flags_pt1 <- reactive({
    req(student_pava_bands())

    student_pava_bands() %>%
      filter(prior_attainment == x_21) %>%
      pivot_longer(
        cols = starts_with(c("x", "y")),
        cols_vary = "slowest",
        names_to = c(".value", "band"),
        names_sep = "_"
      ) %>%
      mutate(difference_prior_x = prior_attainment - x) %>%
      group_by(row_id) %>%
      mutate(smallest_positive_difference = difference_prior_x == minpositive(difference_prior_x)) %>%
      filter(smallest_positive_difference == TRUE) %>%
      slice_min(as.numeric(band)) %>%
      select(row_id, upper_band = band) %>%
      mutate(lower_band = as.character(as.numeric(upper_band) - 1)) %>%
      ungroup()
  })

  pupil_pava_bands_flags_pt2 <- reactive({
    req(student_pava_bands())

    student_pava_bands() %>%
      filter(prior_attainment != x_21) %>%
      pivot_longer(
        cols = starts_with(c("x", "y")),
        cols_vary = "slowest",
        names_to = c(".value", "band"),
        names_sep = "_"
      ) %>%
      mutate(difference_prior_x = prior_attainment - x) %>%
      group_by(row_id) %>%
      mutate(smallest_positive_difference = difference_prior_x == minpositive(difference_prior_x)) %>%
      filter(smallest_positive_difference == TRUE) %>%
      slice_max(as.numeric(band)) %>%
      select(row_id, lower_band = band) %>%
      mutate(upper_band = as.character(as.numeric(lower_band) + 1)) %>%
      ungroup()
  })


  pupil_pava_bands_flags <- reactive({
    req(pupil_pava_bands_flags_pt1())
    req(pupil_pava_bands_flags_pt2())

    bind_rows(pupil_pava_bands_flags_pt1(), pupil_pava_bands_flags_pt2())
  })


  ## 1.f. join the two tables and filter band to only include lower and upper band values
  ## 1.g. this should leave two rows per pupil, with the lower and upper x and y values from the pava
  pupil_pava_bands_filtered <- reactive({
    req(pupil_pava_bands_flags())

    student_pava_bands_full() %>%
      inner_join(pupil_pava_bands_flags(), by = "row_id") %>%
      filter(band == lower_band | band == upper_band) %>%
      mutate(band = as.numeric(band)) %>%
      arrange(row_id, band)
  })


  ## calculating estimated points and value added for each pupil
  pupil_va <- reactive({
    req(pupil_pava_bands_filtered())

    pupil_pava_bands_filtered() %>%
      group_by(row_id) %>%
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
      select(all_of(common_column_names), estimated_points, value_added) %>%
      left_join(data$subject_variance %>% select(qual_id, qual_co_id, subj_weighting, weighting), by = "qual_id") %>%
      mutate(
        value_added_subj_weight = value_added * (subj_weighting / as.numeric(size)),
        value_added_qual_weight = value_added * (weighting / as.numeric(size))
      )
  })



  ## pupil VA download

  output$pupil_va_download2 <- renderUI({
    req(pupil_va())
    downloadButton("pupil_va_download", label = "Student value added scores")
  })

  output$pupil_va_download <- downloadHandler(
    filename = "pupil_va.csv",
    content = function(file) {
      write.csv(pupil_va(), file, row.names = FALSE)
    }
  )


  ## pupil VA preview

  output$student_va_scores <- renderReactable({
    reactable(
      head(
        pupil_va() %>%
          select(
            forename, surname, cohort_name, qualification_name, subject_name, size, qual_id,
            prior_attainment, actual_points, estimated_points, value_added
          ) %>%
          mutate(
            prior_attainment = round2(prior_attainment, 2),
            estimated_points = round2(estimated_points, 2),
            value_added = round2(value_added, 2)
          ),
        input$n
      )
    )
  })


  # output$student_va_scores <- renderDataTable({
  #   datatable(
  #     head(
  #       pupil_va() %>%
  #         select(
  #           forename, surname, cohort_name, qualification_name, subject_name, size, qual_id,
  #           prior_attainment, actual_points, estimated_points, value_added
  #         ) %>%
  #         mutate(
  #           prior_attainment = round2(prior_attainment, 2),
  #           estimated_points = round2(estimated_points, 2),
  #           value_added = round2(value_added, 2)
  #         ),
  #       input$n
  #     ),
  #     options = list(
  #       scrollX = TRUE,
  #       scrollY = "250px",
  #       info = FALSE,
  #       pageLength = FALSE,
  #       paging = FALSE
  #     )
  #   )
  # })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- VA DERIVATIONS - BY SUBJECT - USED IN THE NATIONAL COMPARISON TAB (IN THE INFO BOXES) ----
  # -----------------------------------------------------------------------------------------------------------------------------

  ## calculating value added and confidence intervals for each qualification/subject combination (each qual_id)
  subject_va <- reactive({
    req(pupil_va())

    pupil_va() %>%
      group_by(
        cohort_code, cohort_name, qualification_code, qualification_name,
        subject_code, subject_name, size, qual_id
      ) %>%
      summarise(
        subject_student_count = n(),
        subject_va_pt1 = mean(value_added)
      ) %>%
      left_join(
        data$subject_variance %>%
          select(qual_id, qual_co_id, sd_suqu),
        by = "qual_id"
      ) %>%
      mutate(
        subject_va_grade = subject_va_pt1 / 10 / as.numeric(size),
        subject_standard_error = sd_suqu / sqrt(subject_student_count),
        lower_confidence_interval = subject_va_grade - (1.96 * subject_standard_error),
        upper_confidence_interval = subject_va_grade + (1.96 * subject_standard_error)
      ) %>%
      ungroup()
  })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- HEADLINE VA DERIVATIONS - BY QUALIFICATION TYPE ----
  # -----------------------------------------------------------------------------------------------------------------------------

  ## qual_standard_error_pt1 equivalent to sd_qual_step1 in subject tab
  ## qual_standard_error_pt2 equivalent to sd_qual_step1 in qualification tab
  qualification_va <- reactive({
    req(pupil_va())

    pupil_va() %>%
      group_by(qual_co_id, cohort_code, cohort_name, qualification_code, qualification_name, size) %>%
      summarise(
        qual_student_count = n(),
        qual_va_numerator = sum(value_added_qual_weight, na.rm = TRUE),
        qual_va_denominator = sum(weighting, na.rm = TRUE)
      ) %>%
      group_by(
        qual_co_id, cohort_code, cohort_name, qualification_code, qualification_name, size,
        qual_student_count, qual_va_numerator, qual_va_denominator
      ) %>%
      left_join(
        subject_va() %>%
          select(qual_co_id, subject_student_count, subject_standard_error),
        by = "qual_co_id"
      ) %>%
      mutate(qual_standard_error_pt1 = (subject_standard_error * subject_student_count / qual_student_count)^2) %>%
      summarise(qual_standard_error_pt2 = sum(qual_standard_error_pt1, na.rm = TRUE)) %>%
      mutate(
        qual_va_grade = qual_va_numerator / qual_va_denominator / 10,
        qual_standard_error = sqrt(qual_standard_error_pt2),
        lower_confidence_interval = qual_va_grade - (1.96 * qual_standard_error),
        upper_confidence_interval = qual_va_grade + (1.96 * qual_standard_error)
      ) %>%
      ungroup()
  })




  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- HEADLINE VA DERIVATIONS - BY COHORT ----
  # -----------------------------------------------------------------------------------------------------------------------------


  ## need to add in flag for academic cohort to pupil_va

  ## cohort_standard_error_pt1 equivalent to sd_alev in qualification tab
  ## cohort_standard_error_pt2 equivalent to sum(sd_step1) in headline measures tab
  cohort_va <- reactive({
    req(pupil_va())

    pupil_va() %>%
      group_by(cohort_code, cohort_name) %>%
      summarise(
        cohort_student_count = n(),
        cohort_va_numerator = sum(value_added_qual_weight, na.rm = TRUE),
        cohort_va_denominator = sum(weighting, na.rm = TRUE)
      ) %>%
      group_by(
        cohort_code, cohort_name,
        cohort_student_count, cohort_va_numerator, cohort_va_denominator
      ) %>%
      left_join(
        qualification_va() %>%
          select(cohort_code, cohort_name, qual_student_count, qual_standard_error, size),
        by = c("cohort_code", "cohort_name")
      ) %>%
      mutate(
        student_count_byqualsize = (qual_student_count * as.numeric(size)),
        sum_student_count_byqualsize = sum(student_count_byqualsize, na.rm = TRUE),
        cohort_standard_error_pt1 = (qual_standard_error * student_count_byqualsize / sum_student_count_byqualsize)^2
      ) %>%
      summarise(cohort_standard_error_pt2 = sum(cohort_standard_error_pt1, na.rm = TRUE)) %>%
      mutate(
        cohort_va_grade = cohort_va_numerator / cohort_va_denominator / 10,
        cohort_standard_error = sqrt(cohort_standard_error_pt2),
        lower_confidence_interval = cohort_va_grade - (1.96 * cohort_standard_error),
        upper_confidence_interval = cohort_va_grade + (1.96 * cohort_standard_error)
      ) %>%
      ungroup()
  })




  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- DISADVANTAGED HEADLINE VA DERIVATIONS - BY COHORT ----
  # -----------------------------------------------------------------------------------------------------------------------------


  count_user_disadvantaged <- reactive({
    req(pupil_pava_bands_filtered())

    sum(pupil_pava_bands_filtered()$disadvantaged_status)
  })



  pupil_va_disadvantaged <- reactive({
    req(pupil_pava_bands_filtered())

    pupil_pava_bands_filtered() %>%
      filter(disadvantaged_status == 1) %>%
      group_by(row_id) %>%
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
      select(all_of(common_column_names), estimated_points, value_added) %>%
      left_join(data$disadvantaged_subject_variance %>% select(qual_id, qual_co_id, subj_weighting, weighting), by = "qual_id") %>%
      mutate(
        value_added_subj_weight = value_added * (subj_weighting / as.numeric(size)),
        value_added_qual_weight = value_added * (weighting / as.numeric(size))
      )
  })

  subject_va_disadvantaged <- reactive({
    req(pupil_va_disadvantaged())

    pupil_va_disadvantaged() %>%
      group_by(
        cohort_code, cohort_name, qualification_code, qualification_name,
        subject_code, subject_name, size, qual_id
      ) %>%
      summarise(
        subject_student_count = n(),
        subject_va_pt1 = mean(value_added)
      ) %>%
      left_join(
        data$disadvantaged_subject_variance %>%
          select(qual_id, qual_co_id, sd_suqu),
        by = "qual_id"
      ) %>%
      mutate(
        subject_va_grade = subject_va_pt1 / 10 / as.numeric(size),
        subject_standard_error = sd_suqu / sqrt(subject_student_count),
        lower_confidence_interval = subject_va_grade - (1.96 * subject_standard_error),
        upper_confidence_interval = subject_va_grade + (1.96 * subject_standard_error)
      ) %>%
      ungroup()
  })

  qualification_va_disadvantaged <- reactive({
    req(pupil_va_disadvantaged())

    pupil_va_disadvantaged() %>%
      group_by(qual_co_id, cohort_code, cohort_name, qualification_code, qualification_name, size) %>%
      summarise(
        qual_student_count = n(),
        qual_va_numerator = sum(value_added_qual_weight, na.rm = TRUE),
        qual_va_denominator = sum(weighting, na.rm = TRUE)
      ) %>%
      group_by(
        qual_co_id, cohort_code, cohort_name, qualification_code, qualification_name, size,
        qual_student_count, qual_va_numerator, qual_va_denominator
      ) %>%
      left_join(
        subject_va_disadvantaged() %>%
          select(qual_co_id, subject_student_count, subject_standard_error),
        by = "qual_co_id"
      ) %>%
      mutate(qual_standard_error_pt1 = (subject_standard_error * subject_student_count / qual_student_count)^2) %>%
      summarise(qual_standard_error_pt2 = sum(qual_standard_error_pt1, na.rm = TRUE)) %>%
      mutate(
        qual_va_grade = qual_va_numerator / qual_va_denominator / 10,
        qual_standard_error = sqrt(qual_standard_error_pt2),
        lower_confidence_interval = qual_va_grade - (1.96 * qual_standard_error),
        upper_confidence_interval = qual_va_grade + (1.96 * qual_standard_error)
      ) %>%
      ungroup()
  })

  cohort_va_disadvantaged <- reactive({
    req(pupil_va_disadvantaged())

    pupil_va_disadvantaged() %>%
      group_by(cohort_code, cohort_name) %>%
      summarise(
        cohort_student_count = n(),
        cohort_va_numerator = sum(value_added_qual_weight, na.rm = TRUE),
        cohort_va_denominator = sum(weighting, na.rm = TRUE)
      ) %>%
      group_by(
        cohort_code, cohort_name,
        cohort_student_count, cohort_va_numerator, cohort_va_denominator
      ) %>%
      left_join(
        qualification_va_disadvantaged() %>%
          select(cohort_code, cohort_name, qual_student_count, qual_standard_error, size),
        by = c("cohort_code", "cohort_name")
      ) %>%
      mutate(
        student_count_byqualsize = (qual_student_count * as.numeric(size)),
        sum_student_count_byqualsize = sum(student_count_byqualsize, na.rm = TRUE),
        cohort_standard_error_pt1 = (qual_standard_error * student_count_byqualsize / sum_student_count_byqualsize)^2
      ) %>%
      summarise(cohort_standard_error_pt2 = sum(cohort_standard_error_pt1, na.rm = TRUE)) %>%
      mutate(
        cohort_va_grade = cohort_va_numerator / cohort_va_denominator / 10,
        cohort_standard_error = sqrt(cohort_standard_error_pt2),
        lower_confidence_interval = cohort_va_grade - (1.96 * cohort_standard_error),
        upper_confidence_interval = cohort_va_grade + (1.96 * cohort_standard_error)
      ) %>%
      ungroup()
  })




  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- DROPDOWN BOXES - OUTPUT IN THE NATIONAL COMPARISON TAB ----
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
        choices <- user_data_final() %>%
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
        choices <- user_data_final() %>%
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
        choices <- user_data_final() %>%
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
        choices <- user_data_final() %>%
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
  # ---- SUBJECT CHART DATA - OUTPUT IN THE NATIONAL COMPARISON TAB ----
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

    user_chart_data <- user_data_final() %>%
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

    # max_x <- subject_chart_data() %>%
    #   select(x) %>%
    #   max()

    # print(max_x())

    ggplot(subject_chart_data(), aes(x = x, y = y, color = source, shape = source)) +
      labs(
        title = "KS4 prior attainment (points) compared with 16-18 attainment outcomes (points).",
        alt = "By defualt this line chart is displaying the national average value added line
           for the qualification and subject chosen using the drop down boxes.
           If the user selects to include National and student data the chart will update to
           add the users student data as scatter points over the original national average line"
      ) +
      geom_line(data = filter(subject_chart_data(), source == "national")) +
      geom_point(data = filter(subject_chart_data(), source == "user"), size = 4) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
          color = "azure3",
          linewidth = 0.5,
          linetype = 2
        ),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0, color = "black", size = 20, face = "bold")
      ) +
      xlab("Prior Attainment (points)") +
      ylab(" ") +
      # scale_x_continuous(breaks = seq(0, max_x, by = 2)) +
      scale_colour_manual(values = c("black", "red")) +
      scale_shape_manual(values = c(NA, 4))
  })



  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- SUBJECT LEVEL VA DATA BOXES - OUTPUT IN THE NATIONAL COMPARISON TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------



  ## 1. derive number of entries for chosen qual_id
  subject_entries0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- subject_va() %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(subject_student_count) %>%
      pull()

    return(n)
  })

  output$subject_entries <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (input$data_source == "National data only") {
      valueBox(
        paste0("-"),
        paste0("Select 'National and student data'"),
        color = "purple"
      )
    } else {
      valueBox(
        # take input number
        paste0(subject_entries0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "purple"
      )
    }
  })


  ## 2. derive VA for chosen qual_id
  subject_va_grade0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- subject_va() %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(subject_va_grade) %>%
      pull()

    return(n)
  })

  output$subject_va_grade <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (input$data_source == "National data only") {
      valueBox(
        paste0("-"),
        paste0("Select 'National and student data'"),
        color = "purple"
      )
    } else {
      # Put value into box to plug into app
      valueBox(
        # take input number
        paste0(round2(subject_va_grade0(), 2)),
        # add subtitle to explain what it's showing
        paste0("Value added grade"),
        color = "purple"
      )
    }
  })


  ## 3. derive CI for chosen qual_id
  ci_lower0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- subject_va() %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  ci_upper0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- subject_va() %>%
      filter(qual_id == as.character(reactive_qualid())) %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$ci <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (input$data_source == "National data only") {
      valueBox(
        paste0("-"),
        paste0("Select 'National and student data'"),
        color = "purple"
      )
    } else {
      valueBox(
        # take input number
        paste0("[", round2(ci_lower0(), 2), ", ", round2(ci_upper0(), 2), "]"),
        # add subtitle to explain what it's showing
        paste0("Confidence intervals"),
        color = "purple"
      )
    }
  })



  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- SUBJECT CHART POINTS/GRADE CORRELATION - OUTPUT IN THE NATIONAL COMPARISON TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------

  grade_point_relation <- reactive({
    req(input$dropdown_qualifications)
    req(input$dropdown_sizes)

    data$points_lookup %>%
      arrange(desc(points), grade) %>%
      filter(
        cohort_name == input$dropdown_cohort,
        qualification_name == input$dropdown_qualifications,
        subject_name == input$dropdown_subjects,
        size == input$dropdown_sizes
      ) %>%
      select(grade, points)
  })

  output$grade_point_table <- renderReactable({
    reactable(
      grade_point_relation()[-1] %>%
        rename("Outcome attainment points" = points) %>%
        t() %>%
        as.data.frame() %>%
        setNames(grade_point_relation()[, 1])
    )
  })


  # output$grade_point_table <- renderDataTable({
  #   datatable(
  #     grade_point_relation()[-1] %>%
  #       rename("Outcome attainment points" = points) %>%
  #       t() %>%
  #       as.data.frame() %>%
  #       setNames(grade_point_relation()[, 1]),
  #     options = list(
  #       scrollX = TRUE,
  #       scrollY = "250px",
  #       info = FALSE,
  #       pageLength = FALSE,
  #       paging = FALSE,
  #       searching = FALSE
  #     )
  #   )
  # })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- COHORT LEVEL VA DATA BOXES - OUTPUT IN THE VA COHORT TAB ----
  # -----------------------------------------------------------------------------------------------------------------------------

  # colours for infoboxes
  # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

  output$test_table <- renderDataTable({
    datatable(qualification_va())
  })


  ## 1. derive number of entries for each cohort
  cohort_alev_entries0 <- reactive({
    req(cohort_va())

    # print(cohort_va())
    n <- cohort_va() %>%
      filter(cohort_name == "A level") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_alev_entries <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_alev_entries0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_alev_entries0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_acad_entries0 <- reactive({
    req(cohort_va())

    n <- cohort_va() %>%
      filter(cohort_name == "Academic") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_acad_entries <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_acad_entries0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_acad_entries0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_agen_entries0 <- reactive({
    req(cohort_va())

    n <- cohort_va() %>%
      filter(cohort_name == "Applied general") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_agen_entries <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_agen_entries0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_agen_entries0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techlev_entries0 <- reactive({
    req(cohort_va())

    n <- cohort_va() %>%
      filter(cohort_name == "Tech level") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_techlev_entries <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_techlev_entries0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_techlev_entries0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techcert_entries0 <- reactive({
    req(cohort_va())

    n <- cohort_va() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_techcert_entries <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_techcert_entries0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_techcert_entries0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------
  ## --------------------------------------------------------------------------

  ## 2. derive VA for chosen qual_id
  cohort_alev_va_grade0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "A level") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_alev_va_grade <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_alev_entries0() != 0)) {
      # Put value into box to plug into app
      valueBox(
        paste0(round2(cohort_alev_va_grade0(), 2)),
        paste0("Value added grade"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_acad_va_grade0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Academic") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_acad_va_grade <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_acad_entries0() != 0)) {
      valueBox(
        paste0(round2(cohort_acad_va_grade0(), 2)),
        paste0("Value added grade"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_agen_va_grade0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Applied general") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_agen_va_grade <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_agen_entries0() != 0)) {
      valueBox(
        paste0(round2(cohort_agen_va_grade0(), 2)),
        paste0("Value added grade"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techlev_va_grade0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Tech level") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_techlev_va_grade <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_techlev_entries0() != 0)) {
      valueBox(
        paste0(round2(cohort_techlev_va_grade0(), 2)),
        paste0("Value added grade"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techcert_va_grade0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_techcert_va_grade <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_techcert_entries0() != 0)) {
      valueBox(
        paste0(round2(cohort_techcert_va_grade0(), 2)),
        paste0("Value added grade"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------
  ## --------------------------------------------------------------------------


  ## 3. derive CI for chosen qual_id
  cohort_alev_ci_lower0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "A level") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_alev_ci_upper0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "A level") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_alev_ci <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_alev_entries0() != 0)) {
      valueBox(
        paste0("[", round2(cohort_alev_ci_lower0(), 2), ", ", round2(cohort_alev_ci_upper0(), 2), "]"),
        paste0("Confidence intervals"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_acad_ci_lower0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Academic") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_acad_ci_upper0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Academic") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_acad_ci <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_acad_entries0() != 0)) {
      valueBox(
        paste0("[", round2(cohort_acad_ci_lower0(), 2), ", ", round2(cohort_acad_ci_upper0(), 2), "]"),
        paste0("Confidence intervals"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_agen_ci_lower0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Applied general") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_agen_ci_upper0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Applied general") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_agen_ci <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_agen_entries0() != 0)) {
      valueBox(
        paste0("[", round2(cohort_agen_ci_lower0(), 2), ", ", round2(cohort_agen_ci_upper0(), 2), "]"),
        paste0("Confidence intervals"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techlev_ci_lower0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Tech level") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_techlev_ci_upper0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Tech level") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_techlev_ci <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_techlev_entries0() != 0)) {
      valueBox(
        paste0("[", round2(cohort_techlev_ci_lower0(), 2), ", ", round2(cohort_techlev_ci_upper0(), 2), "]"),
        paste0("Confidence intervals"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techcert_ci_lower0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_techcert_ci_upper0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_techcert_ci <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (length(cohort_techcert_entries0() != 0)) {
      valueBox(
        paste0("[", round2(cohort_techcert_ci_lower0(), 2), ", ", round2(cohort_techcert_ci_upper0(), 2), "]"),
        paste0("Confidence intervals"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })


  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- COHORT LEVEL VA DATA BOXES - OUTPUT IN THE VA COHORT TAB ---- DISADVANTAGED
  # -----------------------------------------------------------------------------------------------------------------------------


  ## 1. derive number of entries for each cohort
  cohort_alev_entries_dis0 <- reactive({
    req(cohort_va_disadvantaged())

    # print(cohort_va_disadvantaged())
    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "A level") %>%
      select(cohort_student_count) %>%
      pull()

    print(n)

    return(n)
  })

  output$cohort_alev_entries_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_alev_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_alev_entries_dis0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_acad_entries_dis0 <- reactive({
    req(cohort_va_disadvantaged())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Academic") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_acad_entries_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    } else if (length(cohort_acad_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_acad_entries_dis0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_agen_entries_dis0 <- reactive({
    req(cohort_va_disadvantaged())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Applied general") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_agen_entries_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_agen_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_agen_entries_dis0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techlev_entries_dis0 <- reactive({
    req(cohort_va_disadvantaged())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Tech level") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_techlev_entries_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    } else if (length(cohort_techlev_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_techlev_entries_dis0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techcert_entries_dis0 <- reactive({
    req(cohort_va_disadvantaged())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(cohort_student_count) %>%
      pull()

    return(n)
  })

  output$cohort_techcert_entries_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_techcert_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(cohort_techcert_entries_dis0()),
        # add subtitle to explain what it's showing
        paste0("Number of students"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------
  ## --------------------------------------------------------------------------

  ## 2. derive VA for chosen qual_id
  cohort_alev_va_grade_dis0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "A level") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_alev_va_grade_dis <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_alev_entries_dis0() != 0)) {
      # Put value into box to plug into app
      valueBox(
        # take input number
        paste0(round2(cohort_alev_va_grade_dis0(), 2)),
        # add subtitle to explain what it's showing
        paste0("Value added grade"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_acad_va_grade_dis0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Academic") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_acad_va_grade_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    } else if (length(cohort_acad_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(round2(cohort_acad_va_grade_dis0(), 2)),
        # add subtitle to explain what it's showing
        paste0("Value added grade"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_agen_va_grade_dis0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Applied general") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_agen_va_grade_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_agen_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(round2(cohort_agen_va_grade_dis0(), 2)),
        # add subtitle to explain what it's showing
        paste0("Value added grade"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techlev_va_grade_dis0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Tech level") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_techlev_va_grade_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    } else if (length(cohort_techlev_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(round2(cohort_techlev_va_grade_dis0(), 2)),
        # add subtitle to explain what it's showing
        paste0("Value added grade"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techcert_va_grade_dis0 <- reactive({
    req(reactive_qualid())

    # print(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(cohort_va_grade) %>%
      pull()

    return(n)
  })

  output$cohort_techcert_va_grade_dis <- renderValueBox({
    # Put value into box to plug into app
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_techcert_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0(round2(cohort_techcert_va_grade_dis0(), 2)),
        # add subtitle to explain what it's showing
        paste0("Value added grade"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------
  ## --------------------------------------------------------------------------


  ## 3. derive CI for chosen qual_id
  cohort_alev_ci_lower_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "A level") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_alev_ci_upper_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "A level") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_alev_ci_dis <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_alev_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0("[", round2(cohort_alev_ci_lower_dis0(), 2), ", ", round2(cohort_alev_ci_upper_dis0(), 2), "]"),
        # add subtitle to explain what it's showing
        paste0("Confidence intervals"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_acad_ci_lower_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Academic") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_acad_ci_upper_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Academic") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_acad_ci_dis <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    } else if (length(cohort_acad_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0("[", round2(cohort_acad_ci_lower_dis0(), 2), ", ", round2(cohort_acad_ci_upper_dis0(), 2), "]"),
        # add subtitle to explain what it's showing
        paste0("Confidence intervals"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_agen_ci_lower_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Applied general") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_agen_ci_upper_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Applied general") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_agen_ci_dis <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_agen_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0("[", round2(cohort_agen_ci_lower_dis0(), 2), ", ", round2(cohort_agen_ci_upper_dis0(), 2), "]"),
        # add subtitle to explain what it's showing
        paste0("Confidence intervals"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techlev_ci_lower_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Tech level") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_techlev_ci_upper_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Tech level") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_techlev_ci_dis <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    } else if (length(cohort_techlev_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0("[", round2(cohort_techlev_ci_lower_dis0(), 2), ", ", round2(cohort_techlev_ci_upper_dis0(), 2), "]"),
        # add subtitle to explain what it's showing
        paste0("Confidence intervals"),
        color = "blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "blue"
      )
    }
  })

  ## --------------------------------------------------------------------------

  cohort_techcert_ci_lower_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(lower_confidence_interval) %>%
      pull()

    return(n)
  })

  cohort_techcert_ci_upper_dis0 <- reactive({
    req(reactive_qualid())

    n <- cohort_va_disadvantaged() %>%
      filter(cohort_name == "Technical certificate") %>%
      select(upper_confidence_interval) %>%
      pull()

    return(n)
  })

  output$cohort_techcert_ci_dis <- renderValueBox({
    if (is.null(input$upload)) {
      valueBox(
        paste0("-"),
        paste0("Please upload student data"),
        color = "purple"
      )
    } else if (count_user_disadvantaged() == 0) {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    } else if (length(cohort_techcert_entries_dis0() != 0)) {
      valueBox(
        # take input number
        paste0("[", round2(cohort_techcert_ci_lower_dis0(), 2), ", ", round2(cohort_techcert_ci_upper_dis0(), 2), "]"),
        # add subtitle to explain what it's showing
        paste0("Confidence intervals"),
        color = "dark-blue"
      )
    } else {
      valueBox(
        paste0("-"),
        paste0("No entries"),
        color = "dark-blue"
      )
    }
  })


  # Stop app -------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
