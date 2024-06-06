if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    checkboxGroupInput('in1', 'Check some letters', choices = head(LETTERS)),
    selectizeInput('in2', 'Select a state', choices = c("", state.name)),
    plotOutput('plot')
  )
  
  server <- function(input, output) {
    output$plot <- renderPlot({
      validate(
        need(input$in1, 'Check at least one letter!'),
        need(input$in2 != '', 'Please choose a state.')
      )
      plot(1:10, main = paste(c(input$in1, input$in2), collapse = ', '))
    })
  }
  
  shinyApp(ui, server)
  
}





test_data <- read.delim("clipboard")

test_data <- test_data %>%
  mutate(qual_id = as.character(qual_id))

# join on pava bands


minpositive = function(x) min(x[x >= 0])





### good version

pupil_pava_bands <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id") %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_") %>%
  mutate(difference_prior_x = prior_attainment - x) 

## 1.b. in some instances there may be multiple lower bands (all with the same value) so we want to select the highest possible band to be lower_band
## 1.c. we can then define upper_band to be lower_band plus 1
pupil_pava_bands_flags <- pupil_pava_bands %>%
  group_by(unique_identifier) %>%
  mutate(smallest_positive_difference = difference_prior_x == minpositive(difference_prior_x)) %>%
  filter(smallest_positive_difference == TRUE) %>%
  slice_max(band) %>%
  select(unique_identifier, lower_band = band) %>%
  mutate(upper_band = as.character(as.numeric(lower_band) + 1)) %>%
  ungroup()

## 1.d. 
pupil_pava_bands_filtered <-  pupil_pava_bands %>%
  inner_join(pupil_pava_bands_flags, by = "unique_identifier") %>%
  filter(band == lower_band | band == upper_band) %>%
  mutate(band = as.numeric(band)) %>%
  arrange(unique_identifier, band)



expected_column_names <- c(
  "unique_identifier", "forename", "surname", "gender", "forvus_id", 
  "cohort_code", "cohort_name", 
  "qualification_code", "qualification_name", 
  "subject_code", "subject_name", "size", "qual_id", 
  "prior_attainment", "actual_points", "disadvantaged_status")

## calculating estimate points
pupil_va <- pupil_pava_bands_filtered %>%
  group_by(unique_identifier) %>%
  mutate(numbering = row_number()) %>%
  mutate(band_position = case_when(numbering == 1 ~ "lower",
                                   numbering == 2 ~ "upper",
                                   TRUE ~ "error")) %>%
  select(-c(band, numbering)) %>%
  pivot_wider(names_from = band_position,
              values_from = c(x, y, difference_prior_x)) %>%
  ungroup() %>%
  mutate(delta_x = x_upper - x_lower,
         delta_y = y_upper - y_lower,
         estimated_points = (delta_y / delta_x) * (prior_attainment - x_lower) + y_lower,
         value_added = actual_points - estimated_points) %>%
  select(all_of(expected_column_names), estimated_points, value_added) %>%
  left_join(data$subject_variance %>% select(qual_id, subj_weighting, weighting), by = "qual_id") %>%
  mutate(value_added_subj_weight = value_added * (subj_weighting / size),
         value_added_qual_weight = value_added * (weighting / size))

subject_va <- pupil_va %>%
  group_by(cohort_code, cohort_name, qualification_code, qualification_name, 
           subject_code, subject_name, size, qual_id) %>%
  summarise(student_count = n(),
            subject_va_pt1 = mean(value_added)) %>%
  left_join(data$subject_variance %>% select(qual_id, sd_suqu), by = "qual_id") %>%
  mutate(subject_va_grade= subject_va_pt1/10/size,
         standard_error = sd_suqu / sqrt(student_count),
         lower_confidence_interval = subject_va_grade - (1.96 * standard_error),
         upper_confidence_interval = subject_va_grade + (1.96 * standard_error))
  
  






pupil_pava_bands_filtered_wide %>% filter(band_position == "error")

 








with_pava_lower <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id") %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_") %>%
  filter(prior_attainment >= x) %>%
  group_by(forvus_id, qual_id) %>%
  slice_max(band, n=1) %>%
  arrange(forvus_id, qual_id)

with_pava_upper <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id") %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_") %>%
  filter(prior_attainment < x) %>%
  group_by(forvus_id, qual_id) %>%
  slice_min(band, n=1) %>%
  arrange(forvus_id, qual_id)



join_columns <- with_pava_lower[, !names(with_pava_lower) %in% c("band", "x", "y")] %>% colnames()



combined <- with_pava_lower %>%
  full_join(with_pava_upper, by = join_columns)

















pivot_longer(cols = starts_with("x"),
             names_to = "x_band",
             names_prefix = "x_",
             values_to = "x_value",
             values_drop_na = TRUE)




billboard
billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )








