test_data <- read.csv("data/data.csv")

test_data <- test_data %>%
  rename_with(., ~ to_snake_case(.x)) %>%
  rename(
    cohort_name = cohort,
    actual_points = actual_points_new_points,
    estimated_points_user_input = estimated_points_new_points,
    value_added_score_user_input = value_added_score
  ) %>%
  mutate(
    qualification_code = as.character(qualification_code),
    subject_code = as.character(subject_code),
    qual_id = as.character(qual_id),
    size = as.character(size),
    disadvantaged_status = as.integer(disadvantaged_status),
    row_id = row_number(),
    cohort_code = case_when(
      cohort_name == "A level" ~ "1",
      cohort_name == "Academic" ~ "2",
      cohort_name == "Applied general" ~ "3",
      qualification_code == "699" ~ "4",
      cohort_name == "Tech level" ~ "5",
      cohort_name == "Technical certificate" ~ "6",
      TRUE ~ "unknown"
    )
  )





minpositive <- function(x) min(x[x >= 0])

pupil_pava_bands_full <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id") %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_"
  ) %>%
  mutate(difference_prior_x = prior_attainment - x)




pupil_pava_bands <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id")



## 1.c. identify which band has the smallest positive difference and set to TRUE
## 1.d. in some instances there may be multiple lower bands (all with the same value) so we want to select the highest possible band to be lower_band
## 1.e. we can then define upper_band to be lower_band plus 1




pupil_pava_bands_flags_pt1 <- pupil_pava_bands %>%
  filter(prior_attainment == x_21) %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_"
  ) %>%
  mutate(difference_prior_x = prior_attainment - x) %>%
  group_by(row_id) %>%
  mutate(smallest_positive_difference = difference_prior_x == minpositive(difference_prior_x)) %>%
  filter(smallest_positive_difference == TRUE) %>%
  slice_min(as.numeric(band)) %>%
  select(row_id, upper_band = band) %>%
  mutate(lower_band = as.character(as.numeric(upper_band) - 1)) %>%
  ungroup()




pupil_pava_bands_flags_pt2 <- pupil_pava_bands %>%
  filter(prior_attainment != x_21) %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_"
  ) %>%
  mutate(difference_prior_x = prior_attainment - x) %>%
  group_by(row_id) %>%
  mutate(smallest_positive_difference = difference_prior_x == minpositive(difference_prior_x)) %>%
  filter(smallest_positive_difference == TRUE) %>%
  slice_max(as.numeric(band)) %>%
  select(row_id, lower_band = band) %>%
  mutate(upper_band = as.character(as.numeric(lower_band) + 1)) %>%
  ungroup()





pupil_pava_bands_flags <- bind_rows(pupil_pava_bands_flags_pt1, pupil_pava_bands_flags_pt2)



## 1.f. join the two tables and filter band to only include lower and upper band values
## 1.g. this should leave two rows per pupil, with the lower and upper x and y values from the pava


pupil_pava_bands_filtered <- pupil_pava_bands_full %>%
  inner_join(pupil_pava_bands_flags, by = "row_id") %>%
  filter(band == lower_band | band == upper_band) %>%
  mutate(band = as.numeric(band)) %>%
  arrange(row_id, band)






pupil_pava_bands %>% distinct(unique_identifier)






expected_column_names <- c(
  "unique_identifier", "forename", "surname", "gender", "forvus_id",
  "cohort_code", "cohort_name",
  "qualification_code", "qualification_name",
  "subject_code", "subject_name", "size", "qual_id",
  "prior_attainment", "actual_points", "disadvantaged_status"
)

## calculating estimate points
pupil_va <- pupil_pava_bands_filtered %>%
  group_by(unique_identifier) %>%
  mutate(numbering = row_number()) %>%
  mutate(band_position = case_when(
    numbering == 1 ~ "lower",
    numbering == 2 ~ "upper",
    TRUE ~ "error"
  )) %>%
  select(-c(band, numbering)) %>%
  pivot_wider(
    names_from = band_position,
    values_from = c(x, y, difference_prior_x)
  ) %>%
  ungroup() %>%
  mutate(
    delta_x = x_upper - x_lower,
    delta_y = y_upper - y_lower,
    estimated_points = (delta_y / delta_x) * (prior_attainment - x_lower) + y_lower,
    value_added = actual_points - estimated_points
  ) %>%
  select(all_of(expected_column_names), estimated_points, value_added) %>%
  left_join(data$subject_variance %>% select(qual_id, subj_weighting, weighting), by = "qual_id") %>%
  mutate(
    value_added_subj_weight = value_added * (subj_weighting / size),
    value_added_qual_weight = value_added * (weighting / size)
  )

subject_va <- pupil_va %>%
  group_by(
    cohort_code, cohort_name, qualification_code, qualification_name,
    subject_code, subject_name, size, qual_id
  ) %>%
  summarise(
    student_count = n(),
    subject_va_pt1 = mean(value_added)
  ) %>%
  left_join(data$subject_variance %>% select(qual_id, sd_suqu), by = "qual_id") %>%
  mutate(
    subject_va_grade = subject_va_pt1 / 10 / size,
    standard_error = sd_suqu / sqrt(student_count),
    lower_confidence_interval = subject_va_grade - (1.96 * standard_error),
    upper_confidence_interval = subject_va_grade + (1.96 * standard_error)
  ) %>%
  ungroup()

subject_va %>%
  filter(qual_id == "1111110101") %>%
  select(student_count) %>%
  pull()

pupil_pava_bands_filtered_wide %>% filter(band_position == "error")


dog <- "The quick brown dog"
str_to_upper(dog)
str_to_lower(dog)
str_to_title(dog)
str_to_sentence("the quick brown dog")


data$qualid_lookup %>%
  select(cohort_name) %>%
  distinct()

qualification_va_disadvantaged <- pupil_va_disadvantaged %>%
  group_by(qual_co_id, cohort_code, cohort_name, qualification_code, qualification_name, size) %>%
  summarise(
    qual_student_count = n(),
    qual_va_numerator = sum(value_added_qual_weight, na.rm = TRUE),
    qual_va_denominator = sum(weighting, na.rm = TRUE)
  ) %>%
  group_by(
    qual_co_id, cohort_code, cohort_name, qualification_code, qualification_name, size,
    qual_student_count, qual_va_numerator, qual_va_denominator
  ) %>%
  left_join(
    subject_va_disadvantaged %>%
      select(qual_co_id, student_count_subj, subject_standard_error),
    by = "qual_co_id"
  ) %>%
  mutate(qual_standard_error_pt1 = (subject_standard_error * student_count_subj / qual_student_count)^2) %>%
  summarise(qual_standard_error_pt2 = sum(qual_standard_error_pt1, na.rm = TRUE)) %>%
  mutate(
    qual_va_grade = qual_va_numerator / qual_va_denominator / 10,
    qual_standard_error = sqrt(qual_standard_error_pt2),
    lower_confidence_interval = qual_va_grade - (1.96 * qual_standard_error),
    upper_confidence_interval = qual_va_grade + (1.96 * qual_standard_error)
  ) %>%
  ungroup()


with_pava_lower <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id") %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_"
  ) %>%
  filter(prior_attainment >= x) %>%
  group_by(forvus_id, qual_id) %>%
  slice_max(band, n = 1) %>%
  arrange(forvus_id, qual_id)

with_pava_upper <- test_data %>%
  select(-c(qualification_name, subject_name, cohort_name)) %>%
  left_join(data$national_bands, by = "qual_id") %>%
  pivot_longer(
    cols = starts_with(c("x", "y")),
    cols_vary = "slowest",
    names_to = c(".value", "band"),
    names_sep = "_"
  ) %>%
  filter(prior_attainment < x) %>%
  group_by(forvus_id, qual_id) %>%
  slice_min(band, n = 1) %>%
  arrange(forvus_id, qual_id)



join_columns <- with_pava_lower[, !names(with_pava_lower) %in% c("band", "x", "y")] %>% colnames()



combined <- with_pava_lower %>%
  full_join(with_pava_upper, by = join_columns)
