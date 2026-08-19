# message("Sourcing dashboard panels")


homepage_panel <- function() {
  tabPanel(
    "User guide and information",
    # modalDialog(
    #   title = "Missing data",
    #   "The 16 to 18 ready reckoner does not include vocational and technical qualifications for 2024 provisional data
    #   due to a data collection issue. This will be resolved in the revised publication.",
    #   size = "l",
    #   easyClose = TRUE
    # ),
    gov_main_layout(
      gov_row(
        column(
          12,
          heading_text(
            "16-18 Ready Reckoner: 2025 final data",
            size = "l",
            level = 1
          )
        ),

        ## Upper panel -------------------------------------------------------

        column(
          12,
          bslib::card(
            bslib::card_header(
              heading_text("Information", size = "m", level = 2)
            ),
            bslib::card_body(
              heading_text("Introduction", size = "s", level = 3),
              gov_text(
                "Welcome to the 16-18 ready reckoner app."
              ),
              gov_text(
                "The 16-18 ready reckoner is a tool that can be used to both
                understand the value added model better, and to manipulate
                student data and inform target setting."
              ),
              gov_text(
                "Instructions on how to use this app are found below."
              ),
              gov_text(
                "To begin uploading your student data please navigate to the Data upload tab."
              ),
              gov_text(
                actionLink("link_to_user_upload_tab", "User data upload panel")
              ),
              gov_text(
                "A level and other 16 to 18 results data are now all available on the statistics platform, ",
                actionLink("ees_publication", "Explore Education Statistics (EES)")
              ),
              heading_text("Value added", size = "s", level = 3),
              gov_text(
                "16 to 18 value added measures show how well students did in their qualifications
                compared to other students with similar prior attainment nationally."
              ),
              gov_text(
                "Information on how we calculated value added measures can be found in the ",
                a("16-18 technical guide",
                  href = "https://www.gov.uk/government/publications/16-to-19-accountability-headline-measures-technical-guide"
                )
              ),
              gov_text("The underlying data used by this app to calculate each student's value added result and the confidence intervals can be downloaded here as .csv files.
            These files include the national model data, the subject variance data, and the subject variance data for the disadvantaged cohort."),
              layout_column_wrap(
                width = 1 / 3,
                downloadButton(
                  outputId = "model_data_download",
                  label = "Model data (csv, 500KB)",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                ),
                downloadButton(
                  outputId = "subject_variance_download",
                  label = "Subject variance (csv, 50KB)",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                ),
                downloadButton(
                  outputId = "disadvantaged_subject_variance_download",
                  label = "Disadvantaged subject variance (csv, 50KB)",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                )
              )
            )
          )
        )
      ),

      ## Lower panel ------------------------------------------------------
      gov_row(
        column(
          12,
          bslib::card(
            bslib::card_header(
              heading_text("Instructions", size = "m", level = 2)
            ),
            bslib::card_body(
              heading_text("1. Data upload", size = "s", level = 3),
              gov_text("The data upload tab is the area for you to upload a .csv file holding your students data."),
              gov_text(
                "Your student data can be taken directly from the pupil level file available on the ",
                a("checking website,",
                  href = "https://onlinecollections.des.fasst.org.uk/fastform/school-checking-exercise-16to18"
                ),
                "which can be saved as a csv file and uploaded within this app."
              ),
              gov_text("Alternatively, there are a number of downloads available to assist with the data upload, also found in this panel under the 'Templates and lookups' tab."),
              tags$ol(
                tags$li("The template.csv file holds the required structure of the file necessary for the app to work.
                        This can be populated with your data, however the column names should remain unchanged.
                        Names and codes for cohorts, qualifications, and subjects can be found in the lookup table."),
                tags$li("A lookup table holds a complete list of exam cohort names and codes,
                        qualification names and codes, subject names and codes, qualification sizes,
                        and qualification identifiers (qual_id). The lookup table can assist with
                        finding the correct codes to input into the template for your student data.")
              ),
              # br(),
              heading_text("2. Data checks", size = "s", level = 3),
              gov_text("The data checking tab reviews the uploaded student data to ensure it is compatable with the app."),
              gov_text("The checks include:"),
              tags$ol(
                tags$li("The combination of exam cohort code, qualification code, subject code, and size entered for each student exists.
                Any combinations that are not recognised will be removed from the data and the app will proceed without including them in the value added calculations."),
                # tags$li("The cohort name exists.
                #         If an unexpected cohort name is found in the data, the row will be removed from the data
                #         and the app will proceed without including these rows in the value added calculations."),
                tags$li("The combination of qualification code and qualification name is correct.
                        If the cohort code and name do not match as expected the app will first look to correct the
                        qualfication name, based on the qualification code provided by the user.
                        If the app is unable to correct the qualification name, the row will be removed from the data
                        and the app will proceed without including these rows in the value added calculations."),
                tags$li("The combination of subject code and subject name is correct.
                        If the cohort code and name do not match as expected the app will first look to correct the
                        subject name, based on the subject code provided by the user.
                        If the app is unable to correct the subject name, the row will be removed from the data
                        and the app will proceed without including these rows in the value added calculations."),
                tags$li("The qualification ID entered has been derived correctly.
                        If a discrepancy is found the app will overwrite the user inputted ID to the ID expected for the
                        inputted exam cohort, qualification, subject and size."),
                tags$li("The prior attainment entered for each student does not exceed, or fall below, the prior
                        attainment range used in the value added model for that subject.
                        If the student prior attainment is too high, or too low, the row will be removed from the data
                        and the app will proceed without including this data in the value added calculations.")
              ),
              # br(),
              heading_text("3. Value added: Student level", size = "s", level = 3),
              gov_text("The value added: student level tab derives the value added data for each student."),
              gov_text("This data can be viewed within the table in the app, or alternatively, it is available for download."),
              # br(),
              heading_text("4. Value added: Subject level", size = "s", level = 3),
              gov_text("The value added: subject level tab derives the value added scores for your students aggregated to subject level."),
              gov_text("On this page you can use drop down boxes to select any subject of interest
                and the chart will update to show the national value added line."),
              gov_text("You can also switch data sources to include both national and student data. In this instance the chart will
                continue to display the national value added line, however your student data will also be plotted to provide a
                visual comparison. Value boxes on the right will update to display the number of students, value added score,
                and confidence intervals for your chosen subject. When using the combined data source the drop down boxes will only
                offer subject options that are included in your uploaded data."),
              # br(),
              heading_text("5. Value added: Cohort level", size = "s", level = 3),
              gov_text("The value added: cohort level tab derives the value added scores for your students aggregated to cohort level."),
              gov_text("Cohorts include A level, academic, applied general, tech level, and technical certificate.
                The cohort value added scores are available for all students, and for disadvantaged students only.")
            )
          )
        )
      )
    )
  )
}


data_upload_panel <- function() {
  tabPanel(
    value = "data_upload_dashboard",
    "Data upload",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          heading_text(
            "Data upload",
            size = "m",
            level = 1
          )
        )
      ),
      br(),
      column(
        width = 12,
        tabsetPanel(
          id = "dataupload_tabsetpanels",
          tabPanel(
            "Student data",
            fluidRow(
              column(
                width = 12,
                bslib::card(
                  bslib::card_header(
                    heading_text("Your student data", size = "m", level = 2)
                  ),
                  bslib::card_body(
                    heading_text("Step 1:", size = "s", level = 3),
                    gov_text("Please select the academic year your institution data corresponds to."),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "dropdown_year",
                        label = "Select a year",
                        # choices = unique(full_data$national_bands),
                        # selected = "2025"
                        choices = unique(paste0(full_data$national_bands$year - 1, "/", substr(full_data$national_bands$year, 3, 4))),
                        selected = paste0(max(full_data$national_bands$year) - 1, "/", substr(max(full_data$national_bands$year), 3, 4))
                      )
                    ),
                    heading_text("Step 2:", size = "s", level = 3),
                    gov_text("Please upload a .csv file containing the student data for your institution, ready to be used in the Ready Reckoner."),
                    gov_text("The student data template and a lookup table for cohort, qualification and subject codes can be found in the 'Templates and lookups' tab above."),
                    fileInput("upload", NULL, buttonLabel = "Browse", accept = c(".csv"))
                  )
                ),
                column(
                  width = 12,
                  bslib::card(
                    bslib::card_body(
                      heading_text("Student data preview:", size = "m", level = 2),
                      numericInput("a", "Number of rows to preview", value = 5, min = 1, step = 1),
                      reactableOutput("input_preview")
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Templates and lookups",
            fluidRow(
              column(
                width = 12,
                bslib::card(
                  bslib::card_header(
                    heading_text("Additional resources", size = "m", level = 2)
                  ),
                  bslib::card_body(
                    gov_text("Download the template ready for populating with your student data:"),
                    column(
                      width = 4,
                      downloadButton(
                        outputId = "student_data_template_download",
                        label = "Student data template (csv, 1KB)",
                        icon = NULL,
                        class = "gov-uk-button-secondary"
                      )
                    ),
                    br(),
                    gov_text("Download the lookup table showing valid code and name combinations for the exam cohort, qualification, subject and size:"),
                    column(
                      width = 4,
                      downloadButton(
                        outputId = "qualid_lookup_download",
                        label = "L3VA subject lookup table (csv, 50KB)",
                        icon = NULL,
                        class = "gov-uk-button-secondary"
                      )
                    ),
                    br(),
                    gov_text("Download the lookup table showing how qualification number
                            (as used in the Ofqual Register, and formerly known as QAN) relates to
                            each qualification id (qual_id), exam cohort, qualification, subject and size:"),
                    column(
                      width = 4,
                      downloadButton(
                        outputId = "qan_lookup_download",
                        label = "Qualification number lookup table (csv, 150KB)",
                        icon = NULL,
                        class = "gov-uk-button-secondary"
                      )
                    ),
                    br(),
                    gov_text("Download the lookup table showing the points/grade available for each qualification id (qual_id)."),
                    column(
                      width = 4,
                      downloadButton(
                        outputId = "points_lookup_download",
                        label = "Points lookup table (csv, 300KB)",
                        icon = NULL,
                        class = "gov-uk-button-secondary"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}


data_checking_panel <- function() {
  tabPanel(
    value = "data_check_dashboard",
    "Data checks",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          bslib::card(
            bslib::card_header(
              heading_text("Student data checking", size = "l", level = 1)
            ),
            bslib::card_body(
              gov_text("Please use this tab to review your uploaded data, and make note of any changes that have been applied by this app."),
              br(),
              gov_text("A series of test have been run to check the exam cohort, qualification and subject names and codes match the lookup."),
              gov_text("The tables below will highlight where the uploaded data has been altered due to discrepancies."),
              gov_text("If the correction applied is incorrect, please review your csv and re-upload the data with the appropriate corrections applied."),
              br(),
              htmlOutput("no_user_data1")
            )
          )
        )
      ),
      gov_row(
        bslib::card(
          bslib::card_header(
            heading_text("1. Removed data check:", size = "m", level = 2)
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(9, 3),
              tagList(
                gov_text("This check confirms the exam cohort code, qualification code, subject code and size combination provided for each student exist."),
                gov_text("The table below will reveal any unknown combinations which will be removed from the data, and the additional download will provide more details."),
                reactableOutput("removed_table")
              ),
              infoBoxOutput("removed_infobox")
            ),
            br(),
            gov_text("Download the full comparison for the removed user data check:"),
            column(
              width = 4,
              downloadButton(
                outputId = "removed_download",
                label = "Removed data check",
                icon = NULL,
                class = "gov-uk-button-secondary"
              )
            )
          )
        )
      ),
      # gov_row(
      #   column(
      #     width = 6,
      #     tags$div(HTML('<h2 class="govuk-heading-m"> 2. Exam cohort name check: </h2>')),
      #     p("This check confirms the exam cohort name is recognised as expected."),
      #     p("The table below will reveal any discrepancies which will be removed from the data, and the additional download will provide more details."),
      #     reactableOutput("cohort_check_table")
      #   ),
      #   column(
      #     width = 6,
      #     infoBoxOutput("cohort_infobox")
      #   ),
      #   column(
      #     width = 12,
      #     br(),
      #     p("Download the full comparison for the exam cohort check:"),
      #     downloadButton(
      #       outputId = "cohort_check_download",
      #       label = "Exam cohort check",
      #       icon = NULL,
      #       class = "gov-uk-button-secondary"
      #     )
      #   )
      # ),
      gov_row(
        bslib::card(
          bslib::card_header(
            heading_text("2. Qualification code/name check:", size = "m", level = 2)
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(9, 3),
              tagList(
                gov_text("This check confirms the qualification code and qualification name match as expected."),
                gov_text("The table below will reveal any discrepancies and the additional download will provide more details."),
                reactableOutput("qualification_check_table")
              ),
              infoBoxOutput("qualification_infobox")
            ),
            br(),
            gov_text("Download the full comparison for the qualification check:"),
            column(
              width = 4,
              downloadButton(
                outputId = "qualification_check_download",
                label = "Qualification check",
                icon = NULL,
                class = "gov-uk-button-secondary"
              )
            )
          )
        )
      ),
      gov_row(
        bslib::card(
          bslib::card_header(
            heading_text("3. Subject code/name check:", size = "m", level = 2)
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(9, 3),
              tagList(
                gov_text("This check confirms the subject code and subject name match as expected."),
                gov_text("The table below will reveal any discrepancies and the additional download will provide more details."),
                reactableOutput("subject_check_table")
              ),
              infoBoxOutput("subject_infobox")
            ),
            br(),
            gov_text("Download the full comparison for the subject check:"),
            column(
              width = 4,
              downloadButton(
                outputId = "subject_check_download",
                label = "Subject check",
                icon = NULL,
                class = "gov-uk-button-secondary"
              )
            )
          )
        )
      ),
      gov_row(
        bslib::card(
          bslib::card_header(
            heading_text("4. Qualification ID check:", size = "m", level = 2)
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(9, 3),
              tagList(
                gov_text("This check confirms the qualification ID (qual_id) has been derived correctly from the exam cohort, qualification, subject and size codes uploaded by the user."),
                gov_text("The table below will reveal any discrepancies and the additional download will provide more details."),
                reactableOutput("qualid_check_table")
              ),
              infoBoxOutput("qualid_infobox")
            ),
            br(),
            gov_text("Download the full comparison for the qualification ID check:"),
            column(
              width = 4,
              downloadButton(
                outputId = "qualid_check_download",
                label = "Qualification ID check",
                icon = NULL,
                class = "gov-uk-button-secondary"
              )
            )
          )
        )
      ),
      gov_row(
        bslib::card(
          bslib::card_header(
            heading_text("5. Pupil prior attainment check:", size = "m", level = 2)
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(9, 3),
              tagList(
                gov_text("This check confirms the pupil prior attainment entered by the user is within the range of prior attainments used in the value added model for that subject."),
                gov_text("The table below will reveal any pupils in the user data which have a prior attainment higher, or lower, than its respective value added model.
                  These pupils will be removed from the data.
                  To include the pupils below, please adjust the user data such that these pupils have a prior attainment which falls within the limits shown in the table and re-upload the data.
                  The additional download will provide more details."),
                reactableOutput("prioratt_check_table")
              ),
              infoBoxOutput("prioratt_infobox")
            ),
            br(),
            gov_text("Download the full comparison for the pupil prior attainment check:"),
            column(
              width = 4,
              downloadButton(
                outputId = "prioratt_check_download",
                label = "Pupil prior attainment check",
                icon = NULL,
                class = "gov-uk-button-secondary"
              )
            )
          )
        )
      )
    )
  )
}


student_va_panel <- function() {
  tabPanel(
    value = "va_student_dashboard",
    "Value added: student level",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          bslib::card(
            bslib::card_header(
              heading_text("Value added scores for each student", size = "l", level = 1)
            ),
            bslib::card_body(
              htmlOutput("no_user_data2"),
              column(
                width = 4,
                uiOutput("pupil_va_download2")
              )
            )
          )
        )
      ),
      gov_row(
        column(
          width = 12,
          bslib::card(
            bslib::card_header(
              heading_text("Student data value added preview:", size = "m", level = 2)
            ),
            bslib::card_body(
              numericInput("n", "Number of rows to preview", value = 5, min = 1, step = 1),
              reactableOutput("student_va_scores")
            )
          )
        )
      )
    )
  )
}


subject_va_panel <- function() {
  tabPanel(
    value = "va_subject_dashboard",
    "Value added: subject level",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          bslib::card(
            bslib::card_header(
              heading_text("National subject level comparison between prior attainment and outcome attainment",
                size = "l",
                level = 1
              )
            ),
            bslib::card_body(
              heading_text("Please use the drop down boxes below to update the chart and information boxes",
                size = "s",
                level = 3
              ),
              gov_text("National data is displayed by default.
                To include your institution data please ensure you have uploaded your data,
                reviewed the data checking tab, and then switched the data source to 'National and student data' using the buttons below."),
              br(),
              layout_column_wrap(
                width = 0.5,
                selectizeInput(
                  inputId = "dropdown_cohort",
                  label = "Select an exam cohort",
                  choices = unique(data()$qualid_lookup$cohort_name),
                  selected = "A level"
                ),
                selectizeInput(
                  inputId = "dropdown_qualifications",
                  label = "Select a qualification",
                  choices = unique(data()$qualid_lookup$qualification_name),
                  selected = "GCE A level"
                ),
                selectizeInput(
                  inputId = "dropdown_subjects",
                  label = "Select a subject",
                  choices = unique(data()$qualid_lookup$subject_name),
                  selected = "Mathematics"
                ),
                selectizeInput(
                  inputId = "dropdown_sizes",
                  label = "Select a size",
                  choices = unique(data()$qualid_lookup$size)
                ),
                radioButtons(
                  inputId = "data_source",
                  label = "Select data source: ",
                  choices = c("National data only", "National and student data")
                )
              )
            )
          )
        )
      ),
      column(
        width = 12,
        htmlOutput("no_user_data3")
      ),
      gov_row(
        bslib::card(
          bslib::card_header(
            heading_text(
              "KS4 prior attainment (points) compared with 16-18 attainment outcomes (points).",
              size = "m",
              level = 2
            )
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(9, 3),

              # Left side
              plotOutput(
                "subject_chart",
                height = "15cm"
              ) %>% withSpinner(color = "#1d70b8"),

              # Right side
              div(
                style = "
                  height: 15cm;
                  display:flex;
                  flex-direction:column;
                  gap:1rem;
                  justify-content: space-between;
                ",
                uiOutput("subject_entries", width = NULL),
                uiOutput("subject_va_grade", width = NULL),
                uiOutput("ci", width = NULL)
              )
            )
          )
        )
      ),
      bslib::card(
        column(
          width = 12,
          bslib::card_header(
            heading_text("Use the table below to understand how the
                      outcome attainment points translate to grades for your selected qualification. ",
              size = "s",
              level = 3
            )
          ),
          bslib::card_body(
            reactableOutput("grade_point_table")
            # DTOutput("grade_point_table"),
          )
        )
      )
    )
  )
}


cohort_va_panel <- function() {
  tabPanel(
    value = "va_cohort_dashboard",
    "Value added: cohort level",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          bslib::card(
            bslib::card_header(
              heading_text("Headline measures", size = "l", level = 1)
            ),
            bslib::card_body(
              htmlOutput("no_user_data4")
            )
          )
        )
      ),
      tabsetPanel(
        id = "va_tabsetpanels",
        tabPanel(
          "All Students",
          fluidRow(
            gov_row(
              column(
                width = 12,
                bslib::card(
                  bslib::card_body(
                    heading_text("A level cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_alev_entries"),
                      uiOutput("cohort_alev_va_grade"),
                      uiOutput("cohort_alev_ci"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Academic cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_acad_entries"),
                      uiOutput("cohort_acad_va_grade"),
                      uiOutput("cohort_acad_ci"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Applied general cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_agen_entries"),
                      uiOutput("cohort_agen_va_grade"),
                      uiOutput("cohort_agen_ci"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Tech level cohort", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_techlev_entries"),
                      uiOutput("cohort_techlev_va_grade"),
                      uiOutput("cohort_techlev_ci"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Technical certificate cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_techcert_entries"),
                      uiOutput("cohort_techcert_va_grade"),
                      uiOutput("cohort_techcert_ci"),
                      col_widths = c(4, 4, 4)
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Disadvantaged Students",
          fluidRow(
            gov_row(
              column(
                width = 12,
                bslib::card(
                  bslib::card_body(
                    heading_text("A level disadvantaged cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_alev_entries_dis"),
                      uiOutput("cohort_alev_va_grade_dis"),
                      uiOutput("cohort_alev_ci_dis"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Academic disadvantaged cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_acad_entries_dis"),
                      uiOutput("cohort_acad_va_grade_dis"),
                      uiOutput("cohort_acad_ci_dis"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Applied general disadvantaged cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_agen_entries_dis"),
                      uiOutput("cohort_agen_va_grade_dis"),
                      uiOutput("cohort_agen_ci_dis"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Tech level disadvantaged cohort", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_techlev_entries_dis"),
                      uiOutput("cohort_techlev_va_grade_dis"),
                      uiOutput("cohort_techlev_ci_dis"),
                      col_widths = c(4, 4, 4)
                    ),
                    # br(),
                    heading_text("Technical certificate disadvantaged cohort:", size = "m", level = 2),
                    bslib::layout_columns(
                      uiOutput("cohort_techcert_entries_dis"),
                      uiOutput("cohort_techcert_va_grade_dis"),
                      uiOutput("cohort_techcert_ci_dis"),
                      col_widths = c(4, 4, 4)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
