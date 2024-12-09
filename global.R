# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent
# across users whereas the server and UI files are constantly interacting and
# responsive to user input.
#
# ---------------------------------------------------------
# message("Sourcing global")


# Library calls ----------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(stringr))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinytitle))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(DT))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))
shhh(library(shinytest2))
shhh(library(rstudioapi))
shhh(library(bslib))
shhh(library(dfeshiny))
shhh(library(ggiraph))
shhh(library(readxl))
# shhh(library(openxlsx))
shhh(library(snakecase))
shhh(library(tidyr))
shhh(library(shinycssloaders))
# shhh(library(shinya11y))

# Functions --------------------------------------------------------------------

# This line enables bookmarking such that input choices are shown in the url.
enableBookmarking("url")



# Rounding -------------------------------------
round2 <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^n
  z * posneg
}






# Source scripts ---------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets
# to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

site_title <- "16-18 Ready Reckoner"
# the following two links are for the template. can update to ours if they are needed/set up. will need to uncomment out the beta banner code in the ui script
site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"
# We can add further mirrors where necessary. Each one can generally handle
# about 2,500 users simultaneously
sites_list <- c(site_primary, site_overflow)
# Update this with your parent
# publication name (e.g. the EES publication)
ees_pub_name <- "Statistical publication"
# Update with parent publication link
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/"
google_analytics_key <- "Z967JJVQQX"


source("R/read_data.R")



# -----------------------------------------------------------------------------------------------------------------------------
# ---- read in the ready reckoner data from the Excel spreadsheet ----
# -----------------------------------------------------------------------------------------------------------------------------
data <- func_read_multiplesheets("data/2019_shadow_l3va_step5_outputs.xlsx")



# -----------------------------------------------------------------------------------------------------------------------------
# ---- tidy RR data and sort data types ----
# -----------------------------------------------------------------------------------------------------------------------------

colnames(data$national_bands) <- to_snake_case(colnames(data$national_bands))
colnames(data$subject_variance) <- to_snake_case(colnames(data$subject_variance))
colnames(data$qualification_variance) <- to_snake_case(colnames(data$qualification_variance))
colnames(data$points_lookup) <- to_snake_case(colnames(data$points_lookup))
colnames(data$subject_chart) <- to_snake_case(colnames(data$subject_chart))
colnames(data$qualid_lookup) <- to_snake_case(colnames(data$qualid_lookup))
colnames(data$gnumber_lookup) <- to_snake_case(colnames(data$gnumber_lookup))
colnames(data$disadvantaged_variance) <- to_snake_case(colnames(data$disadvantaged_variance))

data$national_bands <- data$national_bands %>%
  mutate(
    qual_id = as.character(qual_id)
  ) %>%
  rename(
    avg_prior_x_0 = prior_min,
    avg_prior_x_21 = prior_max,
    avg_outcome_y_0 = outcome_min,
    avg_outcome_y_21 = outcome_max
  )

colnames(data$national_bands) <- gsub(".*prior_", "", colnames(data$national_bands))
colnames(data$national_bands) <- gsub(".*outcome_", "", colnames(data$national_bands))

data$subject_variance <- data$subject_variance %>% mutate(
  qual_id = as.character(qual_id),
  qual_co_id = as.character(qual_co_id),
  sublevno = as.character(sublevno),
  subject_code = as.character(subject_code),
  exam_cohort = as.character(exam_cohort)
)
data$qualification_variance <- data$qualification_variance %>% mutate(
  qual_co_id = as.character(qual_co_id),
  sublevno = as.character(sublevno)
)
data$points_lookup <- data$points_lookup %>% mutate(
  qualification_code = as.character(qualification_code)
)
data$subject_chart <- data$subject_chart %>% mutate(
  qual_id = as.character(qual_id),
  exam_cohort = as.character(exam_cohort),
  sublevno = as.character(sublevno),
  subject_code = as.character(subject_code)
)
data$qualid_lookup <- data$qualid_lookup %>% mutate(
  qual_id = as.character(qual_id),
  qualification_code = as.character(qualification_code),
  subject_code = as.character(subject_code),
  size = as.character(size)
)
data$gnumber_lookup <- data$gnumber_lookup %>% mutate(
  qual_id = as.character(qual_id),
  qualification_number = as.character(qualification_number),
  qualification_code = as.character(qualification_code),
  subject_code = as.character(subject_code)
)
data$disadvantaged_variance <- data$disadvantaged_variance %>% mutate(
  qual_id = as.character(qual_id),
  exam_cohort = as.character(exam_cohort),
  sublevno = as.character(sublevno)
)

expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details", id = inputId,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}
