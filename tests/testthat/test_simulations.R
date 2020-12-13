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
  # Single subje t example
  test_file("analysis_single.r")
  test_file("analysis_multiple.r")
  test_file("analysis_multiple_file.r")
  #--------------------
})