# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.


## function to read in a workbook with multiple sheets
func_read_multiplesheets <- function(workbook, year_to_add) {
  # get the sheet names from within the workbook
  sheet_names <- excel_sheets(workbook)
  # read in each of the sheets from the workbook
  tibble <- lapply(sheet_names, function(x) {
    read_excel(workbook, sheet = x) %>%
      mutate(year = year_to_add)
  })
  data_frame <- lapply(tibble, as.data.frame)

  # assigning names to data frames
  names(data_frame) <- sheet_names

  # print(data_frame)
  return(data_frame)
}


# func_read_multiplesheets <- function(workbook) {
#   # get the sheet names from within the workbook
#   sheet_names <- getSheetNames(workbook)
#   # read in each of the sheets from the workbook
#   data_frame <- lapply(sheet_names, function(sheet) {
#     read.xlsx(workbook, sheet = sheet)
#   })
#
#   # assigning names to data frames
#   names(data_frame) <- sheet_names
#
#   print(data_frame)
# }
