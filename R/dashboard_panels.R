# message("Sourcing dashboard panels")



homepage_panel <- function() {
  tabPanel(
    "User guide and information",
    modalDialog(
      title = "Missing data",
      "The 16 to 18 ready reckoner does not include vocational and technical qualifications for 2024 provisional data
      due to a data collection issue. This will be resolved in the revised publication.",
      size = "l",
      easyClose = TRUE
    ),
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("16-18 Ready Reckoner: 2019 revised data"),
          br(),
          br()
        ),

        ## Left panel -------------------------------------------------------

        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold;
                background-color: #1d70b8;",
                h2("Introduction")
              ),
              div(
                class = "panel-body",
                tags$div(
                  h3("Introduction"),
                  p("Welcome to the 16-18 ready reckoner app."),
                  p("The 16-18 ready reckoner is a tool that can be used to both
                  understand the value added model better, and to manipulate
                  student data and inform target setting."),
                  p("Instructions on how to use this app are found below."),
                  p("To begin uploading your student data please navigate to the Data upload tab."),
                  p(actionLink("link_to_user_upload_tab", "User data upload panel")),
                  h3("Value added")
                ),
                br()
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
                h2("Instructions")
              ),
              div(
                class = "panel-body",
                h3("1. Data upload"),
                p("The data upload tab is the area for you to upload a .csv file holding your students data."),
                p("There are a number of downloads available to assist with the data upload, also found in this panel under the 'Templates and lookups' tab."),
                tags$ol(
                  tags$li("The template.csv file holds the required structure of the file necessary for the app to work.
                          This can be populated with your data, however the column names should remain unchanged.
                          The unique identifier column should be a unique number for each row. We recommended using consecutive numbers.
                          Names and codes for cohorts, qualifications, and subjects can be found in the lookup table."),
                  tags$li("A lookup table holds a complete list of exam cohort names and codes,
                          qualification names and codes, subject names and codes, qualification sizes,
                          and qualification identifiers (qual_id). The lookup table can assist with
                          finding the correct codes to input into the template for your student data.")
                ),
                h3("2. Data checks"),
                p("The data checking tab reviews the uploaded student data to ensure it is compatable with the app."),
                p("The checks include:"),
                tags$ol(
                  tags$li("The combination of exam cohort code, qualification code, subject code, and size entered for each student exists.
                  Any combinations that are not recognised will be removed from the data and the app will proceed without including them in the value added calculations."),
                  tags$li("The combination of cohort code and cohort name is correct.
                          If the cohort code and name do not match as expected the row will be removed from the data
                          and the app will proceed without including them in the value added calculations."),
                  tags$li("The combination of qualification code and qualification name is correct.
                          If the cohort code and name do not match as expected the row will be removed from the data
                          and the app will proceed without including them in the value added calculations."),
                  tags$li("The combination of subject code and subject name is correct.
                          If the cohort code and name do not match as expected the row will be removed from the data
                          and the app will proceed without including them in the value added calculations."),
                  tags$li("The qualification ID entered has been derived correctly.
                          If a discrepancy is found the app will overwrite the user inputted ID to the ID expected for the
                          inputted exam cohort, qualification, subject and size."),
                  tags$li("The prior attainment entered for each student does not exceed, or fall below,
                  the prior attainment range used in the value added model for that subject.
                          If the student prior attainment is too high, or too low, the row will be removed from the data
                          and the app will proceed without including this data in the value added calculations.")
                ),
                h3("3. Value added: Student level"),
                p("The value added: student level tab derives the value added data for each student.
                  This data can be viewed within the table in the app, or alternatively is available for download."),
                h3("4. Value added: Subject level"),
                p("The value added: subject level tab derives the value added scores for your students aggregated to subject level.
                  On this page you can use drop down boxes to select any subject of interest
                  and the chart will update to show the national value added line.
                  You can also switch data sources to include both national and student data. In this instance the chart will
                  continue to display the national value added line, however your student data will also be plotted to provide a
                  visual comparison. Value boxes on the right will update to display the number of students, value added score,
                  and confidence intervals for your chosen subject. When using the combined data source the drop down boxes will only
                  offer subject options that are included in your uploaded data."),
                h3("5. Value added: Cohort level"),
                p("The value added: cohort level tab derives the value added scores for your students aggregated to cohort level.
                Cohorts include A level, academic, applied general, tech level, and technical certificate.
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
          h1("Data upload"),
        )
      ),
      br(),
      column(
        width = 12,
        tabsetPanel(
          id = "tabsetpanels",
          tabPanel(
            "Student data",
            fluidRow(
              column(
                width = 12,
                h2("Your student data")
              ),
              br(),
              column(
                width = 12,
                p("Please upload a .csv file containing the student data for your institution, ready to be used in the Ready Reckoner."),
                p("The student data template and a lookup table for cohort, qualification and subject codes can be found in the 'Templates and lookups' tab above."),
                fileInput("upload", NULL, buttonLabel = "Browse", accept = c(".csv")),
                br(),
                h2("Student data preview:", style = "color:#12436D; font-style:italic"),
                numericInput("a", "Number of rows to preview", value = 5, min = 1, step = 1),
                DTOutput("input_preview")
              )
            )
          ),
          tabPanel(
            "Templates and lookups",
            fluidRow(
              column(
                width = 12,
                h2("Additional resources")
              ),
              br(),
              br(),
              column(
                width = 12,
                h4("Download the template ready for populating with your student data:"),
                downloadButton(
                  outputId = "student_data_template_download",
                  label = "Student data template",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                ),
                br(),
                br()
              ),
              br(),
              column(
                width = 12,
                h4("Download the lookup table showing valid code and name combinations for the exam cohort, qualification, subject and size:"),
                downloadButton(
                  outputId = "user_lookup_download",
                  label = "Lookup table",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                ),
                br(),
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
          h1("Student data checking"),
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
          h2("1. Removed data check:"),
          p("This check confirms the exam cohort code, qualification code, subject code and size combination provided for each student exist."),
          p("The table below will reveal any unknown combinations which will be removed from the data, and the additional download will provide more details."),
          DTOutput("removed_table")
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
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          h2("2. Exam cohort code/name check:"),
          p("This check confirms the exam cohort code and exam cohort name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("cohort_check_table")
        ),
        column(
          width = 6,
          infoBoxOutput("cohort_infobox")
        ),
        column(
          width = 12,
          br(),
          p("Download the full comparison for the exam cohort check:"),
          downloadButton(
            outputId = "cohort_check_download",
            label = "Exam cohort check",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          h2("3. Qualification code/name check:"),
          p("This check confirms the qualification code and qualification name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("qualification_check_table")
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
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          h2("4. Subject code/name check:"),
          p("This check confirms the subject code and subject name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("subject_check_table")
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
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          h2("5. Qualification ID check:"),
          p("This check confirms the qualification ID (qual_id) has been derived correctly from the exam cohort, qualification, subject and size codes uploaded by the user."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("qualid_check_table")
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
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        )
      ),
      br(),
      br(),
      gov_row(
        column(
          width = 6,
          h2("6. Pupil prior attainment check:"),
          p("This check confirms the pupil prior attainment entered by the user is lower than the maximum prior attainment used in the value added model for that subject."),
          p("The table below will reveal any pupils in the user data which have a prior attainment exceeding its respective value added model.
            These pupils will be removed from the data.
            To include the pupils below, please adjust the user data such that these pupils do not have a prior attainment exceeding the maximum as shown in the table and re-upload the data.
            The additional download will provide more details."),
          DTOutput("prioratt_check_table")
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
            icon = shiny::icon("download"),
            class = "downloadButton"
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
          h1("Value added scores for each student")
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
          h2("Student data value added preview:", style = "color:#12436D; font-style:italic"),
          numericInput("n", "Number of rows to preview", value = 5, min = 1, step = 1),
          DTOutput("student_va_scores")
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
          h1("National subject level comparison between prior attainment and outcome attainment"),
          br(),
          br(),
        ),
        column(
          width = 12,
          h3("Please use the drop down boxes below to update the chart and information boxes."),
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
                    choices = unique(data$qualid_lookup$cohort_name),
                    selected = "A level"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_qualifications",
                    label = "Select a qualification",
                    choices = unique(data$qualid_lookup$qualification_name),
                    selected = "GCE A level"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_subjects",
                    label = "Select a subject",
                    choices = unique(data$qualid_lookup$subject_name),
                    selected = "Mathematics"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "dropdown_sizes",
                    label = "Select a size",
                    choices = unique(data$qualid_lookup$size)
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
            h3("Use the table below to understand how the outcome attainment points translate to grades for your selected qualification."),
            br(),
            DTOutput("grade_point_table"),
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
          h1("Headline measures")
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
        id = "tabsetpanels",
        tabPanel(
          "All Students",
          fluidRow(
            gov_row(
              column(
                width = 12,
                h2("A level cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                # DTOutput("test_table"),
                valueBoxOutput("cohort_alev_entries", width = 4),
                valueBoxOutput("cohort_alev_va_grade", width = 4),
                valueBoxOutput("cohort_alev_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Academic cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                valueBoxOutput("cohort_acad_entries", width = 4),
                valueBoxOutput("cohort_acad_va_grade", width = 4),
                valueBoxOutput("cohort_acad_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Applied general cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                valueBoxOutput("cohort_agen_entries", width = 4),
                valueBoxOutput("cohort_agen_va_grade", width = 4),
                valueBoxOutput("cohort_agen_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Tech level cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                valueBoxOutput("cohort_techlev_entries", width = 4),
                valueBoxOutput("cohort_techlev_va_grade", width = 4),
                valueBoxOutput("cohort_techlev_ci", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Technical certificate cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
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
                h2("A level disadvantaged cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                # DTOutput("test_table"),
                valueBoxOutput("cohort_alev_entries_dis", width = 4),
                valueBoxOutput("cohort_alev_va_grade_dis", width = 4),
                valueBoxOutput("cohort_alev_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Academic disadvantaged cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                valueBoxOutput("cohort_acad_entries_dis", width = 4),
                valueBoxOutput("cohort_acad_va_grade_dis", width = 4),
                valueBoxOutput("cohort_acad_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Applied general disadvantaged cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                valueBoxOutput("cohort_agen_entries_dis", width = 4),
                valueBoxOutput("cohort_agen_va_grade_dis", width = 4),
                valueBoxOutput("cohort_agen_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Tech level disadvantaged cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
                valueBoxOutput("cohort_techlev_entries_dis", width = 4),
                valueBoxOutput("cohort_techlev_va_grade_dis", width = 4),
                valueBoxOutput("cohort_techlev_ci_dis", width = 4)
              )
            ),
            gov_row(
              column(
                width = 12,
                h2("Technical certificate disadvantaged cohort:", style = "color:#12436D; font-style:italic"),
                br(),
                br(),
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
















#
#
#
#
#
# homepage_panel <- function() {
#   tabPanel(
#     "User guide and information",
#     gov_main_layout(
#       gov_row(
#         column(
#           12,
#           h1("DfE Analytical Services R-Shiny data dashboard template (h1)"),
#           br(),
#           br()
#         ),
#
#         ## Left panel -------------------------------------------------------
#
#         column(
#           6,
#           div(
#             div(
#               class = "panel panel-info",
#               div(
#                 class = "panel-heading",
#                 style = "color: white;font-size: 18px;font-style: bold;
#                 background-color: #1d70b8;",
#                 h2("Contents (h2)")
#               ),
#               div(
#                 class = "panel-body",
#                 tags$div(
#                   h3("Introduction (h3)"),
#                   p("This app demonstrates the DfE Analytical Services R-Shiny
#                     data dashboard template."),
#                   p("You might want to add some brief introductory text here
#                     alongside some links to different tabs within your
#                     dashboard. Here's an example of a link working:"),
#                   p(actionLink("link_to_app_content_tab", "Dashboard panel")),
#                   p("You need to add an observeEvent() function to the server.R
#                     script for any link that navigates within your App.")
#                 ),
#                 br()
#               )
#             )
#           ),
#         ),
#
#         ## Right panel ------------------------------------------------------
#
#         column(
#           6,
#           div(
#             div(
#               class = "panel panel-info",
#               div(
#                 class = "panel-heading",
#                 style = "color: white;font-size: 18px;font-style: bold;
#                 background-color: #1d70b8;",
#                 h2("Background Info (h2)")
#               ),
#               div(
#                 class = "panel-body",
#                 h3("Context and purpose (h3)"),
#                 p("This app is the DfE Analytical Service's R-Shiny template
#                   demonstration app and is being developed to provide a coherent
#                   styling for DfE dashboards alongside some useful example
#                   componenets that teams can adapt for their own uses."),
#                 p("DfE teams using this template should avoid changing the
#                   styling and layout, keeping the header, footer and side
#                   navigation list formats."),
#                 p("You might want to add some relevant background information
#                   for your users here. For example some useful links to your EES
#                   publication, data sources and other relevant resources."),
#                 h3("Guidance sources (h3)"),
#                 p("For example, here we'll add some of the key resources we draw
#                   on to guide styling and vizualisation...")
#               )
#             )
#           )
#         )
#       )
#     )
#   )
# }
#
#
# dashboard_panel <- function() {
#   tabPanel(
#     value = "dashboard",
#     "Dashboard",
#
#     # Define UI for application that draws a histogram
#
#     # Sidebar with a slider input for number of bins
#     gov_main_layout(
#       gov_row(
#         column(
#           width = 12,
#           h1("Overall content title for this dashboard page (h1)"),
#         ),
#         column(
#           width = 12,
#           expandable(
#             inputId = "details", label = textOutput("dropdown_label"),
#             contents =
#               div(
#                 id = "div_a",
#                 # class = "well",
#                 # style = "min-height: 100%; height: 100%; overflow-y: visible",
#                 gov_row(
#                   column(
#                     width = 6,
#                     selectizeInput("selectPhase",
#                       "Select a school phase",
#                       choices = choicesPhase
#                     )
#                   ),
#                   column(
#                     width = 6,
#                     selectizeInput(
#                       inputId = "selectArea",
#                       label = "Choose an area:",
#                       choices = choicesAreas$area_name
#                     )
#                   ),
#                   column(
#                     width = 12,
#                     paste("Download the underlying data for this dashboard:"),
#                     br(),
#                     downloadButton(
#                       outputId = "download_data",
#                       label = "Download data",
#                       icon = shiny::icon("download"),
#                       class = "downloadButton"
#                     )
#                   )
#                 )
#               )
#           ),
#         ),
#         column(
#           width = 12,
#           tabsetPanel(
#             id = "tabsetpanels",
#             tabPanel(
#               "Valuebox example",
#               fluidRow(
#                 column(
#                   width = 12,
#                   h2("Examples of producing value boxes in R-Shiny (h2)"),
#                   fluidRow(
#                     column(
#                       width = 12,
#                       valueBoxOutput("boxavgRevBal_small", width = 6),
#                       valueBoxOutput("boxpcRevBal_small", width = 6)
#                     )
#                   ),
#                   fluidRow(
#                     column(
#                       width = 12,
#                       valueBoxOutput("boxavgRevBal", width = 6),
#                       valueBoxOutput("boxpcRevBal", width = 6)
#                     )
#                   ),
#                   fluidRow(
#                     column(
#                       width = 12,
#                       valueBoxOutput("boxavgRevBal_large", width = 6),
#                       valueBoxOutput("boxpcRevBal_large", width = 6)
#                     )
#                   )
#                 )
#               )
#             ),
#             tabPanel(
#               "Line chart example",
#               fluidRow(
#                 column(
#                   width = 12,
#                   h2("An example line chart using ggplot and ggiraph (h2)"),
#                   girafeOutput("lineRevBal", width = "100%", height = "100%")
#                 )
#               )
#             ),
#             tabPanel(
#               "Benchmarking example",
#               fluidRow(
#                 column(
#                   width = 12,
#                   h2("An example bar chart using ggplot and ggiraph (h2)"),
#                   p("This is the standard paragraph style for adding guiding
#                     info around data content."),
#                   column(
#                     width = 6,
#                     girafeOutput("colBenchmark",
#                       width = "100%", height = "100%"
#                     )
#                   ),
#                   column(
#                     width = 6,
#                     div(
#                       class = "well",
#                       style = "min-height: 100%; height: 100%; overflow-y:
#                       visible",
#                       fluidRow(
#                         column(
#                           width = 12,
#                           selectizeInput("selectBenchLAs",
#                             "Select benchmark LAs",
#                             choices = choicesLAs$area_name,
#                             multiple = TRUE,
#                             options = list(maxItems = 3)
#                           )
#                         )
#                       )
#                     ),
#                     dataTableOutput("tabBenchmark")
#                   )
#                 )
#               )
#             )
#           )
#         )
#         # add box to show user input
#       )
#     )
#   )
# }
#
#
#
#
#
#
