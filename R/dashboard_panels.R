# message("Sourcing dashboard panels")

data_upload_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Data upload",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Student data upload")
        )
      ),
      gov_row(
        column(
          width = 12,
          p("Please upload a .csv file containing the pupil level data for use in the 2019 Ready Reckoner"),
          fileInput("upload", NULL, buttonLabel = "Browse", accept = c(".csv")),
          br(),
          h2("User data preview:"),
          numericInput("a", "Number of rows to preview", value = 5, min = 1, step = 1),
          DTOutput("input_preview")
        )
      ),
      br(),
      br()
    )
  )
}


data_checking_panel <- function() {
  tabPanel(
    value = "dashboard",
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
      br(),
      gov_row(
        column(
          width = 6,
          h2("1. Exam cohort check:"),
          p("This check confirms the exam cohort code and exam cohort name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("cohort_check_table")
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
          h2("2. Qualification check:"),
          p("This check confirms the qualification code and qualification name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("qualification_check_table")
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
          h2("3. Subject check:"),
          p("This check confirms the subject code and subject name match as expected."),
          p("The table below will reveal any discrepancies and the additional download will provide more details."),
          DTOutput("subject_check_table")
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
      ) # ,
      # gov_row(
      #   column(
      #     width = 6,
      #     h2("Qualification identifier checks:"),
      #     p("The qualification identifier is determined from the exam cohort code, qualification code, subject code and size."),
      #     p("The following qualification identifiers found in the user data do not match the lookup."),
      #     p("They will be updated to match the lookup value."),
      #     numericInput("b", "Number of rows to preview", value = 20, min = 1, step = 1),
      #     DTOutput("qual_id_check")
      #   )
      # )
    )
  )
}


student_va_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Student value added",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Value Added scores for each student")
        )
      ),
      gov_row(
        column(
          width = 12,
          numericInput("n", "Number of rows to preview", value = 5, min = 1, step = 1),
          DTOutput("student_va_scores")
        )
      )
    )
  )
}


national_chart_panel <- function() {
  tabPanel(
    value = "dashboard",
    "National comparison",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("2019 Ready Reckoner: National subject level comparison"),
          br(),
          br(),
        ),
        column(
          width = 12,
          h2("A comparison between prior attainment and outcome attainment using national data and optional institution data"),
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
                    choices = c("National data only", "National and User Institution data")
                  )
                )
              )
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
              valueBoxOutput("entries", width = NULL),
              # valueBoxOutput("entries", width = 2),
              br(),
              br()
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
