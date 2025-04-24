<h1 align="center">
  <br>
16-18 Ready Reckoner app
  <br>
</h1>

---

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 

The 16-18 ready reckoner is a tool that can be used to both understand the 
value added model better, and to manipulate student data and inform target setting.

Links to where each version of the RR app is deployed can be found below:

- Production - https://department-for-education.shinyapps.io/ks5-ready-reckoner/
- Development - https://department-for-education.shinyapps.io/dev-ks5-ready-reckoner/


---

## Requirements

You should list out the software and programming skills needed, as well as any access requirements e.g.


### i. Software requirements (for running locally)

- Installation of R Studio 2024.12.1.563 or higher

- Installation of R 4.4.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)
  
---

## How to use

### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest2 that test the app loads. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function run_tests_locally() is created in the Rprofile script and is available in the RStudio console at all times to run both the unit and ui tests.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.


---

## How to contribute

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only members of the 16-18 accountability data and development team can merge pull requests. Add katie-aisling, cjrace, and rmbielby as requested reviewers, and the team will review before merging.

---

## Contact

Email
Attainment.STATISTICS@education.gov.uk
