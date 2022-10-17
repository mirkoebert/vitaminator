library(testthat)

test_dir(
  "./vitaminatorApp/testthat",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)

