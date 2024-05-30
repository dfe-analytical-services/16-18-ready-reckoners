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
func_read_multiplesheets <- function(workbook) {
  # get the sheet names from within the workbook
  sheets <- excel_sheets(workbook)
  # read in each of the sheets from the workbook
  tibble <- lapply(sheets, function(x) read_excel(workbook, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)

  # assigning names to data frames
  names(data_frame) <- sheets

  # print data frame
  print(data_frame)
}
