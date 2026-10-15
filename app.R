# app.R
# 1. Force R to load all local package code, data, and namespaces
pkgload::load_all(path = ".", export_all = TRUE)


# 1. Explicitly load UI and package dependencies
#library(shiny)
#library(bslib)
#library(dplyr)  

#options(shiny.autoload.r = FALSE)

# 2. Force the server to scan and source all your package R files
r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]$")
sapply(r_files, source)

# 3. Launch the application environment
run_app()

