context("Testing enabling and disabling of ODES with the dynamic option.")
test_that("System examples", {

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


