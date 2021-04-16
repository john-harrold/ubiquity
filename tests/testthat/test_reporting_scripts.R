context("Running example reporting scripts")
test_that("Running Scripts",{
  # Saving the current working directory
  ttdir = getwd()
  # Changing the working directory to the 
  # tempdir
  setwd(tempdir())
  
  # Copying the sources to the current folder
  fr = workshop_fetch("Reporting", overwrite = TRUE)
  #--------------------
  test_file('make_report_PowerPoint.R')
  test_file('make_report_Word.R')
  #--------------------
  ttdir = getwd()
})