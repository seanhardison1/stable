# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
# Engineering

## Downloading the template
download.file("https://www.w3schools.com/w3css/tryw3css_templates_dark_portfolio.htm", "inst/app/www/template.html")
download.file("https://golemverse.org/img/golem-hex_250.png", "inst/app/www/golem-hex_250.png")

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shinipsum" )
usethis::use_package( "dplyr" )
usethis::use_package(" tidyr ")
usethis::use_package( "plotly" )
usethis::use_package( "DT" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "data_processing", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "va_demersal_fish", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation
## Vignette ----
usethis::use_vignette("stable")
devtools::build_vignettes()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

rstudioapi::navigateToFile("dev/03_deploy.R")
