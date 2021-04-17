context("Running example simulation scripts")
test_that("Running Scripts",{
  # Saving the current working directory
  ttdir = getwd()
  # Changing the working directory to the 
  # tempdir
  setwd(tempdir())
  
  # Copying the sources to the current folder
  fr = workshop_fetch("Simulation", overwrite = TRUE)
  #--------------------
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code("analysis_single.r")[["isgood"]],           "analysis_single.r")
    expect_true(check_code("analysis_multiple.r")[["isgood"]],         "analysis_multiple.r")
    expect_true(check_code("analysis_multiple_file.r")[["isgood"]],    "analysis_multiple_file.r")
  }
  #--------------------
})