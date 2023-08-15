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
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "make_report_PowerPoint.R"))[["isgood"]], "PowerPoint")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "make_report_Word.R"))[["isgood"]],       "Word")
  }
  #--------------------
  ttdir = getwd()
})
