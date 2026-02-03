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
          tags$div(HTML('<h1 class="govuk-heading-l"> 16-18 Ready Reckoner: 2025 provisional data </h1>')),
          br(),
          br()
        ),

        ## Left panel -------------------------------------------------------

        column(
          12,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold;
                background-color: #1d70b8;",
                tags$div(HTML('<h2 class="govuk-heading-m"> Information </h2>'))
              ),
              div(
                class = "panel-body",
                tags$div(
                  tags$div(HTML('<h3 class="govuk-heading-s"> Introduction </h3>')),
                  p("Welcome to the 16-18 ready reckoner app."),
                  p("The 16-18 ready reckoner is a tool that can be used to both
                  understand the value added model better, and to manipulate
                  student data and inform target setting."),
                  p("Instructions on how to use this app are found below."),
                  p("To begin uploading your student data please navigate to the Data upload tab."),
                  p(actionLink("link_to_user_upload_tab", "User data upload panel")),
                  br(),
                  p(
                    "A level and other 16 to 18 results data are now all available on the statistics platform, ",
                    a("Explore Education Statistics (EES)",
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results"
                    )
                  ),
                  br(),
                  tags$div(HTML('<h3 class="govuk-heading-s"> Value added </h3>')),
                  p("16 to 18 value added measures show how well students did in their qualifications
                  compared to other students with similar prior attainment nationally."),
                  p(
                    "Information on how we calculated value added measures can be found in the ",
                    a("16-18 technical guide",
                      href = "https://www.gov.uk/government/publications/16-to-19-accountability-headline-measures-technical-guide"
                    )
                  ),
                  br(),
                  p("The underlying data used by this app to calculate each student's value added result and the confidence intervals can be downloaded here as .csv files.
                  These files include the national model data, the subject variance data, and the subject variance data for the disadvantaged cohort."),
                  downloadButton(
                    outputId = "model_data_download",
                    label = "Model data (csv, 500KB)",
                    icon = NULL,
                    class = "gov-uk-button-secondary"
                  ),
                  downloadButton(
                    outputId = "subject_variance_download",
                    label = "Subject variance (csv, 50KB)",
                    icon = NULL,
                    class = "gov-uk-button-secondary"
                  ),
                  downloadButton(
                    outputId = "disadvantaged_subject_variance_download",
                    label = "Disadvantaged subject variance (csv, 50KB)",
                    icon = NULL,
                    class = "gov-uk-button-secondary"
                  )
                )
              )
            )
          ),
        ),

        ## Right panel ------------------------------------------------------

        column(
          12,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold;
                background-color: #1d70b8;",
                tags$div(HTML('<h2 class="govuk-heading-m"> Instructions </h2>'))
              ),
              div(
                class = "panel-body",
                tags$div(HTML('<h3 class="govuk-heading-s"> 1. Data upload </h3>')),
                p("The data upload tab is the area for you to upload a .csv file holding your students data."),
                p(
                  "Your student data can be taken directly from the pupil level file available on the ",
                  a("checking website,",
                    href = "https://onlinecollections.des.fasst.org.uk/fastform/school-checking-exercise-16to18"
                  ),
                  "which can be saved as a csv file and uploaded within this app."
                ),
                p("Alternatively, there are a number of downloads available to assist with the data upload, also found in this panel under the 'Templates and lookups' tab."),
                tags$ol(
                  tags$li("The template.csv file holds the required structure of the file necessary for the app to work.
                          This can be populated with your data, however the column names should remain unchanged.
                          Names and codes for cohorts, qualifications, and subjects can be found in the lookup table."),
                  tags$li("A lookup table holds a complete list of exam cohort names and codes,
                          qualification names and codes, subject names and codes, qualification sizes,
                          and qualification identifiers (qual_id). The lookup table can assist with
                          finding the correct codes to input into the template for your student data.")
                ),
                br(),
                tags$div(HTML('<h3 class="govuk-heading-s"> 2. Data checks </h3>')),
                p("The data checking tab reviews the uploaded student data to ensure it is compatable with the app."),
                p("The checks include:"),
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
                br(),
                tags$div(HTML('<h3 class="govuk-heading-s"> 3. Value added: Student level </h3>')),
                p("The value added: student level tab derives the value added data for each student."),
                p("This data can be viewed within the table in the app, or alternatively, it is available for download."),
                br(),
                tags$div(HTML('<h3 class="govuk-heading-s"> 4. Value added: Subject level </h3>')),
                p("The value added: subject level tab derives the value added scores for your students aggregated to subject level."),
                p("On this page you can use drop down boxes to select any subject of interest
                  and the chart will update to show the national value added line."),
                p("You can also switch data sources to include both national and student data. In this instance the chart will
                  continue to display the national value added line, however your student data will also be plotted to provide a
                  visual comparison. Value boxes on the right will update to display the number of students, value added score,
                  and confidence intervals for your chosen subject. When using the combined data source the drop down boxes will only
                  offer subject options that are included in your uploaded data."),
                br(),
                tags$div(HTML('<h3 class="govuk-heading-s"> 5. Value added: Cohort level </h3>')),
                p("The value added: cohort level tab derives the value added scores for your students aggregated to cohort level."),
                p("Cohorts include A level, academic, applied general, tech level, and technical certificate.
                  The cohort value added scores are available for all students, and for disadvantaged students only.")
              )
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
          tags$div(HTML('<h1 class="govuk-heading-l"> Data upload </h1>'))
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
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Your student data </h2>'))
              ),
              br(),
              column(
                width = 12,
                h4("Step 1:"),
                br(),
                p("Please select the academic year your institution data corresponds to."),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_year",
                    label = "Select a year",
                    choices = unique(full_data$national_bands),
                    selected = "2025"
                  )
                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                h4("Step 2:"),
                br(),
                p("Please upload a .csv file containing the student data for your institution, ready to be used in the Ready Reckoner."),
                p("The student data template and a lookup table for cohort, qualification and subject codes can be found in the 'Templates and lookups' tab above."),
                fileInput("upload", NULL, buttonLabel = "Browse", accept = c(".csv")),
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Student data preview: </h2>')),
                # h2("Student data preview:", style = "color:#12436D; font-style:italic"),
                numericInput("a", "Number of rows to preview", value = 5, min = 1, step = 1),
                reactableOutput("input_preview")
              )
            )
          ),
          tabPanel(
            "Templates and lookups",
            fluidRow(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Additional resources </h2>'))
              ),
              br(),
              br(),
              column(
                width = 12,
                p("Download the template ready for populating with your student data:"),
                downloadButton(
                  outputId = "student_data_template_download",
                  label = "Student data template (csv, 1KB)",
                  icon = NULL,
                  class = "gov-uk-button-secondary"
                ),
                br(),
                br()
              ),
              br(),
              column(
                width = 12,
                p("Download the lookup table showing valid code and name combinations for the exam cohort, qualification, subject and size:"),
                downloadButton(
                  outputId = "qualid_lookup_download",
                  label = "L3VA subject lookup table (csv, 50KB)",
                  icon = NULL,
                  class = "gov-uk-button-secondary"
                ),
                br(),
                br()
              ),
              br(),
              column(
                width = 12,
                p("Download the lookup table showing how qualification number
                (as used in the Ofqual Register, and formerly known as QAN) relates to
                  each qualification id (qual_id), exam cohort, qualification, subject and size:"),
                downloadButton(
                  outputId = "qan_lookup_download",
                  label = "Qualification number lookup table (csv, 150KB)",
                  icon = NULL,
                  class = "gov-uk-button-secondary"
                ),
                br(),
                br()
              ),
              br(),
              column(
                width = 12,
                p("Download the lookup table showing the points/grade available for each qualification id (qual_id)."),
                downloadButton(
                  outputId = "points_lookup_download",
                  label = "Points lookup table (csv, 300KB)",
                  icon = NULL,
                  class = "gov-uk-button-secondary"
                ),
                br(),
                br()
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
          tags$div(HTML('<h1 class="govuk-heading-l"> Student data checking </h1>')),
          p("Please use this tab to review your uploaded data, and make note of any changes that have been applied by this app."),
          br(),
          p("A series of test have been run to check the exam cohort, qualification and subject names and codes match the lookup."),
          p("The tables below will highlight where the uploaded data has been altered due to discrepancies."),
          p("If the correction applied is incorrect, please review your csv and re-upload the data with the appropriate corrections applied."),
        )
      ),
      br(),
      gov_row(
        column(
          width = 12,
          htmlOutput("no_user_data1")
        ),
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          tags$div(HTML('<h2 class="govuk-heading-m"> 1. Removed data check: </h2>')),
          p("This check confirms the exam cohort code, qualification code, subject code and size combination provided for each student exist."),
          p("The table below will reveal any unknown combinations which will be removed from the data, and the additional download will provide more details."),
          reactableOutput("removed_table")
        ),
        column(
          width = 6,
          infoBoxOutput("removed_infobox")
        ),
        column(
          width = 12,
          br(),
          p("Download the full comparison for the removed user data check:"),
          downloadButton(
            outputId = "removed_download",
            label = "Removed data check",
            icon = NULL,
            class = "gov-uk-button-secondary"
          )
        )
      ),
      br(),
      br(),
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
      # br(),
      # br(),
      gov_row(
        column(
          width = 6,
          tags$div(HTML('<h2 class="govuk-heading-m"> 2. Qualification code/name check: </h2>')),
          p("This check confirms the qualification code and qualification name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          reactableOutput("qualification_check_table")
        ),
        column(
          width = 6,
          infoBoxOutput("qualification_infobox")
        ),
        column(
          width = 12,
          br(),
          p("Download the full comparison for the qualification check:"),
          downloadButton(
            outputId = "qualification_check_download",
            label = "Qualification check",
            icon = NULL,
            class = "gov-uk-button-secondary"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          tags$div(HTML('<h2 class="govuk-heading-m"> 3. Subject code/name check: </h2>')),
          p("This check confirms the subject code and subject name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          reactableOutput("subject_check_table")
        ),
        column(
          width = 6,
          infoBoxOutput("subject_infobox")
        ),
        column(
          width = 12,
          br(),
          p("Download the full comparison for the subject check:"),
          downloadButton(
            outputId = "subject_check_download",
            label = "Subject check",
            icon = NULL,
            class = "gov-uk-button-secondary"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          tags$div(HTML('<h2 class="govuk-heading-m"> 4. Qualification ID check: </h2>')),
          p("This check confirms the qualification ID (qual_id) has been derived correctly from the exam cohort, qualification, subject and size codes uploaded by the user."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          reactableOutput("qualid_check_table")
        ),
        column(
          width = 6,
          infoBoxOutput("qualid_infobox")
        ),
        column(
          width = 12,
          br(),
          p("Download the full comparison for the qualification ID check:"),
          downloadButton(
            outputId = "qualid_check_download",
            label = "Qualification ID check",
            icon = NULL,
            class = "gov-uk-button-secondary"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          tags$div(HTML('<h2 class="govuk-heading-m"> 5. Pupil prior attainment check: </h2>')),
          p("This check confirms the pupil prior attainment entered by the user is within the range of prior attainments used in the value added model for that subject."),
          p("The table below will reveal any pupils in the user data which have a prior attainment higher, or lower, than its respective value added model.
            These pupils will be removed from the data.
            To include the pupils below, please adjust the user data such that these pupils have a prior attainment which falls within the limits shown in the table and re-upload the data.
            The additional download will provide more details."),
          reactableOutput("prioratt_check_table")
        ),
        column(
          width = 6,
          infoBoxOutput("prioratt_infobox")
        ),
        column(
          width = 12,
          br(),
          p("Download the full comparison for the pupil prior attainment check:"),
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
}


student_va_panel <- function() {
  tabPanel(
    value = "va_student_dashboard",
    "Value added: student level",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          tags$div(HTML('<h1 class="govuk-heading-l"> Value added scores for each student </h1>'))
        ),
      ),
      br(),
      column(
        width = 12,
        htmlOutput("no_user_data2"),
        br()
      ),
      br(),
      gov_row(
        column(
          width = 12,
          uiOutput("pupil_va_download2"),
          br()
        )
      ),
      gov_row(
        column(
          width = 12,
          tags$div(HTML('<h2 class="govuk-heading-m"> Student data value added preview: </h2>')),
          # h2("Student data value added preview:", style = "color:#12436D; font-style:italic"),
          numericInput("n", "Number of rows to preview", value = 5, min = 1, step = 1),
          reactableOutput("student_va_scores"),
          # DTOutput("student_va_scores")
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
          tags$div(HTML('<h1 class="govuk-heading-l"> National subject level comparison between prior attainment and outcome attainment </h1>')),
          br(),
          br(),
        ),
        column(
          width = 12,
          tags$div(HTML('<h3 class="govuk-heading-s"> Please use the drop down boxes below to update the chart and information boxes. </h3>')),
          p("National data is displayed by default.
            To include your institution data please ensure you have uploaded your data,
            reviewed the data checking tab, and then switched the data source to 'National and student data' using the buttons below."),
          br(),
          column(
            width = 12,
            div(
              class = "well",
              style = "min-height: 100%; height: 100%; overflow-y: visible",
              gov_row(
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_cohort",
                    label = "Select an exam cohort",
                    choices = unique(data()$qualid_lookup$cohort_name),
                    selected = "A level"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_qualifications",
                    label = "Select a qualification",
                    choices = unique(data()$qualid_lookup$qualification_name),
                    selected = "GCE A level"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_subjects",
                    label = "Select a subject",
                    choices = unique(data()$qualid_lookup$subject_name),
                    selected = "Mathematics"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_sizes",
                    label = "Select a size",
                    choices = unique(data()$qualid_lookup$size)
                  )
                ),
                column(
                  width = 12,
                  radioButtons(
                    inputId = "data_source",
                    label = "Select data source: ",
                    choices = c("National data only", "National and student data")
                  )
                )
              )
            ),
            column(
              width = 12,
              htmlOutput("no_user_data3")
            ),
            column(
              width = 10,
              br(),
              br(),
              plotOutput("subject_chart", height = "15cm") %>% withSpinner(color = "#1d70b8"),
              br(),
              br()
            ),
            column(
              width = 2,
              br(),
              br(),
              valueBoxOutput("subject_entries", width = NULL),
              br(),
              br(),
              valueBoxOutput("subject_va_grade", width = NULL),
              br(),
              br(),
              valueBoxOutput("ci", width = NULL)
            )
          ),
          column(
            width = 9,
            tags$div(HTML('<h3 class="govuk-heading-s"> Use the table below to understand how the
                          outcome attainment points translate to grades for your selected qualification. </h3>')),
            br(),
            reactableOutput("grade_point_table"),
            # DTOutput("grade_point_table"),
            br(),
            br()
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
          tags$div(HTML('<h1 class="govuk-heading-l"> Headline measures </h1>'))
        ),
      ),
      br(),
      column(
        width = 12,
        htmlOutput("no_user_data4"),
        br()
      ),
      br(),
      tabsetPanel(
        id = "va_tabsetpanels",
        tabPanel(
          "All Students",
          fluidRow(
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> A level cohort: </h2>')),
                # h2("A level cohort:", style = "color:#12436D; font-style:italic"),
                # DTOutput("test_table"),
                valueBoxOutput("cohort_alev_entries", width = 4),
                valueBoxOutput("cohort_alev_va_grade", width = 4),
                valueBoxOutput("cohort_alev_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Academic cohort: </h2>')),
                valueBoxOutput("cohort_acad_entries", width = 4),
                valueBoxOutput("cohort_acad_va_grade", width = 4),
                valueBoxOutput("cohort_acad_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Applied general cohort: </h2>')),
                valueBoxOutput("cohort_agen_entries", width = 4),
                valueBoxOutput("cohort_agen_va_grade", width = 4),
                valueBoxOutput("cohort_agen_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Tech level cohort: </h2>')),
                valueBoxOutput("cohort_techlev_entries", width = 4),
                valueBoxOutput("cohort_techlev_va_grade", width = 4),
                valueBoxOutput("cohort_techlev_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Technical certificate cohort: </h2>')),
                valueBoxOutput("cohort_techcert_entries", width = 4),
                valueBoxOutput("cohort_techcert_va_grade", width = 4),
                valueBoxOutput("cohort_techcert_ci", width = 4)
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
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> A level disadvantaged cohort: </h2>')),
                # h2("A level disadvantaged cohort:", style = "color:#12436D; font-style:italic"),
                # DTOutput("test_table"),
                valueBoxOutput("cohort_alev_entries_dis", width = 4),
                valueBoxOutput("cohort_alev_va_grade_dis", width = 4),
                valueBoxOutput("cohort_alev_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Academic disadvantaged cohort: </h2>')),
                valueBoxOutput("cohort_acad_entries_dis", width = 4),
                valueBoxOutput("cohort_acad_va_grade_dis", width = 4),
                valueBoxOutput("cohort_acad_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Applied general disadvantaged cohort: </h2>')),
                valueBoxOutput("cohort_agen_entries_dis", width = 4),
                valueBoxOutput("cohort_agen_va_grade_dis", width = 4),
                valueBoxOutput("cohort_agen_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Tech level disadvantaged cohort: </h2>')),
                valueBoxOutput("cohort_techlev_entries_dis", width = 4),
                valueBoxOutput("cohort_techlev_va_grade_dis", width = 4),
                valueBoxOutput("cohort_techlev_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                br(),
                tags$div(HTML('<h2 class="govuk-heading-m"> Technical certificate disadvantaged cohort: </h2>')),
                valueBoxOutput("cohort_techcert_entries_dis", width = 4),
                valueBoxOutput("cohort_techcert_va_grade_dis", width = 4),
                valueBoxOutput("cohort_techcert_ci_dis", width = 4)
              )
            )
          )
        )
      )
    )
  )
}
