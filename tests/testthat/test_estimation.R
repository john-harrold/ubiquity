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
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_parent.r"))                   [["isgood"]], "analysis_parent.r")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_parent_metabolite.r"))        [["isgood"]], "analysis_parent_metabolite.r")       
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_parent_metabolite_global.r")) [["isgood"]], "analysis_parent_metabolite_global.r")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_parent_metabolite_nm_data.r"))[["isgood"]], "analysis_parent_metabolite_nm_data.r")
  }
  #--------------------
})
