context("Running example NCA scripts")
test_that("Running Scripts",{
  # Saving the current working directory
  ttdir = getwd()
  # Changing the working directory to the 
  # tempdir
  setwd(tempdir())
  
  # Copying the sources to the current folder
  fr = workshop_fetch("NCA", overwrite = TRUE)
  #--------------------
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_nca_sd.R"))[["isgood"]],     "NCA Single")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_nca_md.R"))[["isgood"]],     "NCA Multiple")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_nca_sparse.R"))[["isgood"]], "NCA Sparse")
  }
  #--------------------
})
