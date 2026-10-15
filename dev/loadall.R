library(shiny)
runApp("app.R", launch.browser = TRUE)

# Step 1: Install developer configuration dependencies
install.packages(c("devtools", "roxygen2"))
library(devtools)
library(roxygen2)

# Step 2: Auto-generate documentation infrastructure mapping
devtools::document()

# Step 3: Check package validity flags
devtools::check()

# Step 4: Clean install the application directly into your library
devtools::install()

# Step 5: Test launch your application suite from anywhere!
library(pksimapp)

run_app() |> shiny::runApp(launch.browser = TRUE)

# everytime I make the changes to package
devtools::load_all()
run_app()

Sys.setenv(R_CONFIG_ACTIVE = "production")

config::get()
