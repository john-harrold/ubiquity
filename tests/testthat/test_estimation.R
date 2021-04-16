context("Running example estimation scripts")
test_that("Running Scripts",{
  # Saving the current working directory
  ttdir = getwd()
  # Changing the working directory to the 
  # tempdir
  setwd(tempdir())
  
  # Copying the sources to the current folder
  fr = workshop_fetch("Estimation", overwrite = TRUE)
  #--------------------
  test_file("analysis_parent.r")
  test_file("analysis_parent_metabolite.r")       
  test_file("analysis_parent_metabolite_global.r")
  test_file("analysis_parent_metabolite_nm_data.r")
  #--------------------
})