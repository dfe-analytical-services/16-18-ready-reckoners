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

# Core shiny and R packages
shhh(library(shiny))
shhh(library(bslib))
shhh(library(rstudioapi))

# Custom packages
shhh(library(dfeR))
shhh(library(dfeshiny))
shhh(library(shinyGovstyle))

# Creating charts and tables
shhh(library(ggplot2))
shhh(library(DT))
shhh(library(sf))
shhh(library(leaflet))
shhh(library(htmltools))
shhh(library(reactable))
shhh(library(svglite))
shhh(library(afcharts))
shhh(library(ggrepel))
shhh(library(showtext))
shhh(library(openxlsx))
shhh(library(shinycssloaders))

# Data and string manipulation
shhh(library(dplyr))
shhh(library(stringr))
shhh(library(ggiraph))
shhh(library(purrr))
shhh(library(readr))

# Shiny extensions
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(shinytitle))
shhh(library(xfun))
shhh(library(metathis))

# Dependencies needed for testing or CI but not for the app -------------------
# Including them here keeps them in renv but avoids the app needlessly loading
# them, saving on load time.
if (FALSE) {
  shhh(library(shinytest2))
  shhh(library(chromote))
  shhh(library(testthat))
  shhh(library(devtools))
  shhh(library(shinya11y))
}

shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyalert))
shhh(library(readxl))
shhh(library(snakecase))
shhh(library(tidyr))

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
  right: 0;S
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
google_analytics_key <- "72QXVY0V75"


source("R/read_data.R")


# -----------------------------------------------------------------------------------------------------------------------------
# ---- read in the ready reckoner data from the Excel spreadsheet ----
# -----------------------------------------------------------------------------------------------------------------------------

## expand this list at the begining of each cycle
data_2025F <- func_read_multiplesheets("data/2025F_l3va_step5_outputs_Rversion.xlsx", 2025)
data_2024F <- func_read_multiplesheets("data/2024F_l3va_step5_outputs_Rversion_redacted.xlsx", 2024)


# get the sheet names - would need to update this each year with latest year data
# relies on sheet names not changing between runs
sheet_names <- names(data_2025F)

# combine corresponding sheets
# would also need to expand each year
full_data <- lapply(sheet_names, function(sheet) {
  bind_rows(
    data_2025F[[sheet]],
    data_2024F[[sheet]]
  )
})

# fix sheet names
names(full_data) <- sheet_names


template_data <- read.csv("data/pupil_upload_template.csv", check.names = FALSE)


full_data$qualid_lookup <- full_data$qualid_lookup %>%
  mutate(cohort_code = as.character(cohort_code))

# -----------------------------------------------------------------------------------------------------------------------------
# ---- other ----
# -----------------------------------------------------------------------------------------------------------------------------

# expandable <- function(inputId, label, contents) {
#   govDetails <- shiny::tags$details(
#     class = "govuk-details", id = inputId,
#     shiny::tags$summary(
#       class = "govuk-details__summary",
#       shiny::tags$span(
#         class = "govuk-details__summary-text",
#         label
#       )
#     ),
#     shiny::tags$div(contents)
#   )
# }

# Fonts for charts ------------------------------------------------------------
font_add("dejavu", "www/fonts/DejaVuSans.ttf")
register_font(
  "dejavu",
  plain = "www/fonts/DejaVuSans.ttf",
  bold = "www/fonts/DejaVuSans-Bold.ttf",
  italic = "www/fonts/DejaVuSans-Oblique.ttf",
  bolditalic = "www/fonts/DejaVuSans-BoldOblique.ttf"
)
showtext_auto()
