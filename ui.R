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
  bslib::page_fluid(
    # shinya11y::use_tota11y(),
    tags$head(HTML(paste0("<title>", site_title, "</title>"))),
    tags$head(tags$link(rel = "shortcut icon", href = "dfefavicon.png")),
    use_shiny_title(),
    useShinyjs(),
    tags$html(lang = "en"),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "gov_table_style.css"
      )
    ),
    # Add meta description for search engines
    meta() %>%
      meta_general(
        application_name = site_title,
        description = site_title,
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "16 to 18 statistics",
        rating = "General",
        referrer = "no-referrer"
      ),

    # Load javascript dependencies --------------------------------------------
    shinyjs::useShinyjs(),
    shinyGovstyle::full_width_overrides(),
    # useShinydashboard(),

    # Custom disconnect function ----------------------------------------------
    # Variables used here are set in the global.R file
    dfeshiny::custom_disconnect_message(
      links = sites_list,
      publication_name = ees_pub_name,
      publication_link = ees_publication
    ),

    # Google analytics --------------------------------------------------------
    tags$head(includeHTML(("google-analytics.html"))),

    # Cookies -----------------------------------------------------------------
    # Setting up cookie consent based on a cookie recording the consent:
    dfeshiny::dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(name = site_title),

    # Skip_to_main -------------------------------------------------------------
    # Add a 'Skip to main content' link for keyboard users to bypass navigation.
    # It stays hidden unless focussed via tabbing.
    shinyGovstyle::skip_to_main(),

    # Header ------------------------------------------------------------------
    shinyGovstyle::header(
      org_name = "",
      service_name = site_title,
    ),

    # Beta banner -------------------------------------------------------------
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This Dashboard is in beta phase and we are still reviewing performance and reliability."
      )
    ),
    gov_main_layout(
      bslib::navset_hidden(
        id = "pages",
        nav_panel(
          "dashboard",
          ## Main dashboard ---------------------------------------------------
          # Nav panels --------------------------------------------------------------
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
            cohort_va_panel()
          )
        ),
        nav_panel(
          value = "accessibility_statement",
          "Accessibility",
          layout_columns(
            col_widths = c(-2, 8, -2),

            # Add in back link
            actionLink(
              class = "govuk-back-link",
              style = "margin: 0",
              "accessibility_to_dashboard",
              "Back to dashboard"
            ),
            dfeshiny::a11y_panel(
              dashboard_title = site_title,
              dashboard_url = site_primary,
              date_tested = "17th December 2024",
              date_prepared = "17th December 2024",
              date_reviewed = "17th December 2024",
              issues_contact = "attainment.statistics@education.gov.uk",
              publication_name = "A level and other 16 to 18 results",
              publication_slug = "a-level-and-other-16-to-18-results",
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
          )
        ),
        nav_panel(
          value = "cookies_statement",
          "Cookies",
          layout_columns(
            col_widths = c(-2, 8, -2),

            # Add backlink
            actionLink(
              class = "govuk-back-link",
              style = "margin: 0",
              "cookies_to_dashboard",
              "Back to dashboard"
            ),
            cookies_panel_ui(google_analytics_key = google_analytics_key)
          )
        ),
        nav_panel(
          value = "support",
          "Support and feedback",
          # Set up column layout to center it -----------------------------------------
          layout_columns(
            col_widths = c(-2, 8, -2),

            # Add in back link
            actionLink(
              class = "govuk-back-link",
              style = "margin: 0",
              "support_to_dashboard",
              "Back to dashboard"
            ),
            support_panel(
              team_email = "attainment.statistics@education.gov.uk",
              repo_name = "https://github.com/dfe-analytical-services/16-18-ready-reckoners",
              publication_name = ees_pub_name,
              # publication_slug = "a-level-and-other-16-to-18-results",
              form_url = "https://forms.office.com/e/Sa4ULADzx4"
            )
          )
        )
      )
    ),

    # Footer ------------------------------------------------------------------
    shinyGovstyle::footer(
      full = TRUE,
      links = c(
        "Support",
        "Accessibility statement",
        "Cookies statement"
      )
    )
  )
}
