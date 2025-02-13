# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define
# where they are placed. Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to
# decide what goes in. However, every element should meet accessibility
# requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date
# selections, multiple choice dropdowns etc. Use the shiny cheatsheet to explore
# more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to
# use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#

ui <- function(input, output, session) {
  fluidPage(
    # use_tota11y(),
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      # Add title for browser tabs
      tags$title("16-18 Ready Reckoner")
    ),
    use_shiny_title(),
    tags$html(lang = "en"),
    # Add meta description for search engines
    meta() %>%
      meta_general(
        application_name = "16-18 Ready Reckoner",
        description = "16-18 Ready Reckoner",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "stats development",
        rating = "General",
        referrer = "no-referrer"
      ),
    shinyjs::useShinyjs(),
    useShinydashboard(),
    dfeshiny::custom_disconnect_message(dashboard_title = site_title),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    dfeshiny::dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(
      name = site_title
    ),
    dfeshiny::header(
      header = site_title
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This Dashboard is in beta phase and we are still reviewing performance and reliability."
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      homepage_panel(),
      data_upload_panel(),
      data_checking_panel(),
      student_va_panel(),
      subject_va_panel(),
      cohort_va_panel(),
      shiny::tabPanel(
        value = "accessibility",
        "Accessibility",
        dfeshiny::a11y_panel(
          dashboard_title = site_title,
          dashboard_url = site_primary,
          date_tested = "17/12/2024",
          date_prepared = "17/12/2024",
          date_reviewed = "17/12/2024",
          issues_contact = "attainment.statistics@education.gov.uk",
          non_accessible_components = c(
            "Some navigation elements are not announced correctly by screen readers",
            "Focus highlighting is limited within the dashboard",
            "Heading image and link are not labelled appropriately "
          ),
          specific_issues = c(
            "The navigation panel has been flagged as an accessibility issue and we will look for an alternative way to navigate around the panels on the app",
            "Focus styling is missing which means that features on the app do not change colour to indicate they have been selected.",
            "Heading image and link are not labelled appropriately"
          )
        )
      ),
      shiny::tabPanel(
        value = "support_panel",
        "Support and feedback",
        support_panel(
          team_email = "attainment.statistics@education.gov.uk",
          repo_name = "https://github.com/dfe-analytical-services/16-18-ready-reckoners",
          publication_name = "A level and other 16 to 18 results",
          publication_slug = "a-level-and-other-16-to-18-results",
          form_url = "https://forms.office.com/e/Sa4ULADzx4"
        )
      ),
      shiny::tabPanel(
        value = "cookies_panel_ui",
        "Cookies",
        dfeshiny::cookies_panel_ui(google_analytics_key = google_analytics_key)
      )
    ),
    tags$script(
      src = "script.js"
    ),
    footer(full = TRUE)
  )
}
