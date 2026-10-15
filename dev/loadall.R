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



### connect to git hub
usethis::use_git()
#generate the token
usethis::create_github_token()
# copy the token
# copy and paste token
gitcreds::gitcreds_set()

# activate the project
#The package will automatically create a brand new, private or public repository on your GitHub account matching your R project's name.
usethis::use_github()

# alternative method
# git add .
# git commit -m "Initial project commit"
# git remote add origin https://github.com
# git branch -M main
# git push -u origin main


# Publish to git hub
# 1. Update and generate all documentation files from roxygen comments
devtools::document()

# 2. Run your test suite (if you have one setup via testthat)
devtools::test()

# 3. Run a comprehensive suite of checks simulating CRAN standards
devtools::check()

# Initialize Git tracking locally
usethis::use_git()

# Push it to a brand new repository on your GitHub profile
usethis::use_github()


# others can install this package using
devtools::install_github("your-github-username/your-package-name")

