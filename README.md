# Project-2

# Purpose of App
The purpose of this app is to give users a simple and efficient way to look at the nutritional facts regarding various fruits. Users are able to make comparisons surrounding their food using many graphical and numerical summaries.

# Required Packages
1. tidyverse
2. shiny
3. httr
4. jsonlite
5. dplyr

# Package Installation Code
packages <- c("tidyverse", "shiny", "httr", "jsonlite", "dplyr")

installed <- packages %in% installed.packages()
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)

# Code for running app from GitHub
shiny::runGitHub(repo = "Project-2", username = "egshipp")

