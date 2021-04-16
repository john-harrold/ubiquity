context("Running example titration scripts")
test_that("Running scripts", {
  # Saving the current working directory
  ttdir = getwd()
  # Changing the working directory to the 
  # tempdir
  setwd(tempdir())
  
  # Copying the sources to the current folder
  fr = workshop_fetch("Titration", overwrite = TRUE)
  #--------------------
  # Single subje t example
  test_file("analysis_repeat_dosing.r")                    
  test_file("analysis_repeat_infusion.r")
  test_file("analysis_state_reset.r")                      
  test_file("analysis_visit_dosing_titration.r")           
  test_file("analysis_visit_dosing_titration_stochastic.r")
  test_file("analysis_visit_infusion_dosing.r")  
  #--------------------
})


