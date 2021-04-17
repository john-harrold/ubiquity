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
    expect_true(check_code('analysis_nca_sd.R')[["isgood"]],     'analysis_nca_sd.R')
    expect_true(check_code('analysis_nca_md.R')[["isgood"]],     'analysis_nca_md.R')
    expect_true(check_code('analysis_nca_sparse.R')[["isgood"]], 'analysis_nca_sparse.R')
  }
  #--------------------
})