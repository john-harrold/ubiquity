context("Steady-state evaluation")
test_that("Steady-state Check",{
  setwd(tempdir())
  sys_cmpt = file.path("system_cmpt.txt")
  fr=system_new(
    file_name   = sys_cmpt,
    system_file = "mab_pk",
    overwrite   = TRUE)
  
  cfg_cmpt = build_system(sys_cmpt)
  ssres = system_check_steady_state(cfg_cmpt)
  expect_true(ssres$steady_state)
})

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
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_single.r"))       [["isgood"]],    "analysis_single.r")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_multiple.r"))     [["isgood"]],    "analysis_multiple.r")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_multiple_file.r"))[["isgood"]],    "analysis_multiple_file.r")
  }
  #--------------------
})

