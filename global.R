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


# Function defined for not in -------------------------------------
`%not_in%` <- purrr::negate(`%in%`)





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
# site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"

# We can add further mirrors where necessary. Each one can generally handle
# about 2,500 users simultaneously
sites_list <- c(site_primary)
# sites_list <- c(site_primary, site_overflow)

# Update this with your parent
# publication name (e.g. the EES publication)
ees_pub_name <- "A level and other 16 to 18 results"
# Update with parent publication link
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results"
google_analytics_key <- "Z967JJVQQX"


source("R/read_data.R")



# -----------------------------------------------------------------------------------------------------------------------------
# ---- read in the ready reckoner data from the Excel spreadsheet ----
# -----------------------------------------------------------------------------------------------------------------------------

data <- func_read_multiplesheets("data/2019A_l3va_step5_outputs_Rversion.xlsx")

template_data <- read.csv("data/pupil_upload_template.csv", check.names = FALSE)





data$qualid_lookup <- data$qualid_lookup %>%
  mutate(cohort_code = as.character(cohort_code))

# -----------------------------------------------------------------------------------------------------------------------------
# ---- other ----
# -----------------------------------------------------------------------------------------------------------------------------

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
