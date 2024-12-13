library(shinytest2)
library(diffviewer)

app <- AppDriver$new(name = "basic_load", height = 846, width = 1445, load_timeout = 45 * 1000, timeout = 20 * 1000, wait = TRUE)

# Wait until Shiny is not busy for 500ms
app$wait_for_idle(500)

# Screenshots are left on for this script to help with troubleshooting
# They will not cause any failures if there's changes

inputs <- c(
  "navlistPanel", "tabsetpanels",
  "data_source", "dropdown_cohort", "dropdown_qualifications", "dropdown_sizes",
  "dropdown_subjects",
  "a", "n",
  "upload"
)

outputs <- c(
  "input_preview",
  "cohort_check_table", "cohort_infobox",
  "prioratt_check_table", "prioratt_infobox",
  "qualid_check_table", "qualid_infobox",
  "qualification_check_table", "qualification_infobox",
  "removed_check_table", "removed_infobox"
)

test_that("App loads", {
  # Capture initial values
  app$expect_values(
    input = inputs,
    output = outputs
  )
})

app$set_inputs(navlistPanel = "data_upload_dashboard")
test_that("Data upload panel opens", {
  # Capture initial values
  app$expect_values(
    input = inputs,
    output = outputs
  )
})

app$set_inputs(navlistPanel = "data_check_dashboard")
test_that("Data check panel opens", {
  # Capture initial values
  app$expect_values(
    input = inputs,
    output = outputs
  )
})

app$set_inputs(navlistPanel = "va_student_dashboard")
test_that("VA student panel opens", {
  # Capture initial values
  app$expect_values()
})

app$set_inputs(navlistPanel = "va_subject_dashboard")
test_that("VA subject panel opens", {
  # Capture initial values
  app$expect_values()
})

app$set_inputs(navlistPanel = "va_cohort_dashboard")
test_that("VA cohort panel opens", {
  # Capture initial values
  app$expect_values()
})
