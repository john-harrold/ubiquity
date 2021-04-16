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
  test_file('analysis_nca_sd.R')
  test_file('analysis_nca_md.R')
  test_file('analysis_nca_sparse.R')
  #--------------------
})