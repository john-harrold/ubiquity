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
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code("analysis_repeat_dosing.r")[["isgood"]],                        "analysis_repeat_dosing.r")                     
    expect_true(check_code("analysis_repeat_infusion.r")[["isgood"]],                      "analysis_repeat_infusion.r")
    expect_true(check_code("analysis_state_reset.r")[["isgood"]],                          "analysis_state_reset.r")                      
    expect_true(check_code("analysis_visit_dosing_titration.r")[["isgood"]],               "analysis_visit_dosing_titration.r")           
    expect_true(check_code("analysis_visit_dosing_titration_stochastic.r")[["isgood"]],    "analysis_visit_dosing_titration_stochastic.r")
    expect_true(check_code("analysis_visit_infusion_dosing.r")[["isgood"]],                "analysis_visit_infusion_dosing.r")  
  }
  #--------------------
})


