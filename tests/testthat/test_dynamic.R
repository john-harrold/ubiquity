context("Testing the simulation dynamic option.")
test_that("Enable/disable C and R Script", {

  #-----------------------------------
  is_at_ss = function(cfg, som){
    is_ss = TRUE
    for(sname in names(cfg[["options"]][["mi"]][["states"]])){
      diff =  som[["simout"]][[sname]][1] - som[["simout"]][[sname]][ nrow(som[["simout"]]) ]
      if(diff != 0){
        is_ss = FALSE
      }
    }
  is_ss}
  #-----------------------------------
  tmpsf = paste0("system_", digest::digest(rnorm(1), algo=c("md5")), ".txt")
  fr = suppressWarnings(suppressMessages( system_new(system_file="mab_pk", file_name=tmpsf, output_directory=tempdir()) ))
  expect_true(fr)
  if(fr){
    setwd(tempdir())
    cfg = suppressWarnings(suppressMessages( build_system(tmpsf) ))
    params = system_fetch_parameters(cfg)
    
    #-----------------------------------
    # C, not steady state
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "integrate_with",
                          value  = "c-file")
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "dynamic",
                          value  = TRUE)
    som = suppressWarnings(suppressMessages( run_simulation_ubiquity(params, cfg) ))
    #-----------------------------------
    # R, not steady state
    expect_false(is_at_ss(cfg, som))
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "integrate_with",
                          value  = "r-file")
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "dynamic",
                          value  = TRUE)
    som = suppressWarnings(suppressMessages( run_simulation_ubiquity(params, cfg) ))
    expect_false(is_at_ss(cfg, som))
    #-----------------------------------
    # C, steady state
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "integrate_with",
                          value  = "c-file")
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "dynamic",
                          value  = FALSE)
    som = suppressWarnings(suppressMessages( run_simulation_ubiquity(params, cfg) ))
    expect_true(is_at_ss(cfg, som))
    #-----------------------------------
    # r, steady state
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "integrate_with",
                          value  = "r-file")
    cfg=system_set_option(cfg, group  = "simulation", 
                          option = "dynamic",
                          value  = FALSE)
    som = suppressWarnings(suppressMessages( run_simulation_ubiquity(params, cfg) ))
    expect_true(is_at_ss(cfg, som))
    #-----------------------------------
  }
  
  
})

context("Running example in vitro example scripts")
test_that("Running Scripts",{
  # Saving the current working directory
  ttdir = getwd()
  # Changing the working directory to the 
  # tempdir
  setwd(tempdir())
  
  # Copying the sources to the current folder
  fr = workshop_fetch("In Vitro", overwrite = TRUE)
  #--------------------
  if (Sys.getenv("USER") != "travis") {
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "mk_data_in_vitro.R"))[["isgood"]],  "mk_data_in_vitro.R")
    expect_true(check_code(system.file(package="ubiquity","ubinc","scripts", "analysis_in_vitro.R"))[["isgood"]], "analysis_in_vitro.R")
  }
  setwd(ttdir)
})

