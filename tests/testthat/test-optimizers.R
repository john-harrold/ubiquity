library(ubiquity)
library(parallel)
library(testthat)

tmp_wd = file.path(tempdir(), "test_optimizers")
if(dir.exists(tmp_wd)){
  unlink(tmp_wd, recursive=TRUE, force=TRUE)
}
dir.create(tmp_wd, recursive=TRUE, showWarnings=FALSE)

test_wd = getwd()
setwd(tmp_wd)
on.exit(setwd(test_wd))


invisible(suppressMessages(suppressWarnings({
res = workshop_fetch(section="Estimation")


analysis_name = 'parent_metabolite_global';
flowctl = 'estimate';
archive_results = TRUE

# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# set name                  | Description
# -------------------------------------------------------
# default                   | Original Estimates


# The following will estimate a subset of the parameters:
pnames = c('Vp',
           'Vt',
           'Vm',
           'CLp',
           'CLm',
           'Q',
           'slope_parent',
           'slope_metabolite');

cfg = system_select_set(cfg, "default", pnames)


# Specify the output times used for smooth profiles
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "output_times", 
                           seq(0,100,1))

# Loading Datasets
#
cfg = system_load_data(cfg, dsname     = "pm_data", 
                            data_file  = "pm_data.csv")


# Defining the cohorts
#
# Clearing all of the cohorts
cfg = system_clear_cohorts(cfg);

#----------------------------------------------------------
# 30 mpk cohort
cohort = list(
  name         = "dose_10",
  cf           = list(DOSE      = c(10)),
  inputs       = NULL,
  outputs      = NULL,
  dataset      = "pm_data")


# Bolus inputs for the cohort
cohort[["inputs"]][["bolus"]] = list()
cohort[["inputs"]][["bolus"]][["Mpb"]] = list(TIME=NULL, AMT=NULL)
cohort[["inputs"]][["bolus"]][["Mpb"]][["TIME"]] = c( 0) # hours 
cohort[["inputs"]][["bolus"]][["Mpb"]][["AMT"]]  = c(10) # mpk 


# Defining Parent output
cohort[["outputs"]][["Parent"]] = list()

# Mapping to data set
cohort[["outputs"]][["Parent"]][["obs"]] = list(
         time           = "TIME",
         value          = "PT",
         missing        = -1)

# Mapping to system file
cohort[["outputs"]][["Parent"]][["model"]] = list(
         time           = "hours",       
         value          = "Cpblood",   
         variance       = "slope_parent*PRED^2")

# Plot formatting
cohort[["outputs"]][["Parent"]][["options"]] = list(
         marker_color   = "black",
         marker_shape   = 1,
         marker_line    = 1 )

# Defining Metabolite output
cohort[["outputs"]][["Metabolite"]] = list()

# Mapping to data set
cohort[["outputs"]][["Metabolite"]][["obs"]] = list(
         time           = "TIME",
         value          = "MT",
         missing        = -1)

# Mapping to system file
cohort[["outputs"]][["Metabolite"]][["model"]] = list(
         time           = "hours",       
         value          = "Cmblood",   
         variance       = "slope_metabolite*PRED^2")

# Plot formatting
cohort[["outputs"]][["Metabolite"]][["options"]] = list(
         marker_color   = "blue",
         marker_shape   = 1,
         marker_line    = 1 )

cfg = system_define_cohort(cfg, cohort)
#----------------------------------------------------------
# 30 mpk cohort
cohort = list(
  name         = "dose_30",
  cf           = list(DOSE      = c(30)),
  inputs       = NULL,
  outputs      = NULL,
  dataset      = "pm_data")


# Bolus inputs for the cohort
cohort[["inputs"]][["bolus"]] = list()
cohort[["inputs"]][["bolus"]][["Mpb"]] = list(TIME=NULL, AMT=NULL)
cohort[["inputs"]][["bolus"]][["Mpb"]][["TIME"]] = c( 0) # hours 
cohort[["inputs"]][["bolus"]][["Mpb"]][["AMT"]]  = c(30) # mpk 


# Defining Parent output
cohort[["outputs"]][["Parent"]] = list()

# Mapping to data set
cohort[["outputs"]][["Parent"]][["obs"]] = list(
         time           = "TIME",
         value          = "PT",
         missing        = -1)

# Mapping to system file
cohort[["outputs"]][["Parent"]][["model"]] = list(
         time           = "hours",       
         value          = "Cpblood",   
         variance       = "slope_parent*PRED^2")

# Plot formatting
cohort[["outputs"]][["Parent"]][["options"]] = list(
         marker_color   = "green",
         marker_shape   = 1,
         marker_line    = 1 )

# Defining Metabolite output
cohort[["outputs"]][["Metabolite"]] = list()

# Mapping to data set
cohort[["outputs"]][["Metabolite"]][["obs"]] = list(
         time           = "TIME",
         value          = "MT",
         missing        = -1)

# Mapping to system file
cohort[["outputs"]][["Metabolite"]][["model"]] = list(
         time           = "hours",       
         value          = "Cmblood",   
         variance       = "slope_metabolite*PRED^2")

# Plot formatting
cohort[["outputs"]][["Metabolite"]][["options"]] = list(
         marker_color   = "red",
         marker_shape   = 1,
         marker_line    = 1 )

cfg = system_define_cohort(cfg, cohort)



# here are the different optimizers to test:
opt_test = list()
opt_test[["pso"]] = list(
  pkgs     = "pso",
  opimizer = "pso",
  method = "psoptim",
  control = list(
     trace  = TRUE,
     maxit  = 20,
     REPORT = 10)
)
opt_test[["ppso"]] = list(
  pkgs     = "ppso",
  opimizer = "ppso",
  method = "optim_pso",
  control = list(
    max_number_function_calls = 500,
    max_number_of_iterations  = 50,
    number_of_particles       = 5))

  
opt_test[["parallelPSO"]] = list(
  pkgs     = c("parallelPSO", "doParallel"),
  opimizer = "parallelpso",
  method   = "pso",
  control = list(
    max_number_iterations  = 50,
    number_of_partiples    = 40,
    parallel = TRUE))
 
opt_test[["GA"]] = list(
  pkgs     = c("GA"),
  opimizer = "ga",
  method   = "ga",
  control = list(
    monitor   = FALSE, 
    maxiter   = 10, 
    optimArgs = list(method  = "Nelder-Mead",
                     maxiter = 10)))

opt_test[["FME"]] = list(
  pkgs     = c("FME"),
  opimizer = "fme",
  method   = "modMCMC",
  control = list( 
      burninlength              = 10, 
      niter                     = 500))


cfg=system_set_option(cfg, group  = "simulation",
                           option = "compute_cores", 
                           value  = detectCores() - 1)
 


})))

testthat::test_that("Testing Optimizers",{
  for(opt_name in names(opt_test)){
    opt_info = opt_test[[opt_name]]
    tmp_test_opt = TRUE
    if(!is.null(opt_info[["pkgs"]])){

      # Making sure the packages are loaded
      invisible( suppressMessages(suppressWarnings({
      lapply(opt_info[["pkgs"]], library, character.only = TRUE)
      })))

      ubiquity:::system_req(opt_info[["pkgs"]])
      cd_res = check_deps(opt_info[["pkgs"]]) 
      if(!cd_res$isgood){
        tmp_test_opt = FALSE
        message(paste0("Skipping optimizer test '", opt_name, 
          "' since package(s) '",  
          paste(cd_res[["pkgs_missing"]], collapse=", "), "' is(are) not installed."))
      }
    }
  
    if(tmp_test_opt){
      tmp_test_msg = paste0("Testing optimizer ", opt_name, " method ", opt_info[["method"]])
      cfg = system_set_option(cfg, group  = "estimation",
                                   option = "optimizer", 
                                   value  = opt_info[["opimizer"]])
      
      cfg = system_set_option(cfg, group  = "estimation",
                                   option = "method",
                                   value  = opt_info[["method"]])
      
      cfg = system_set_option(cfg, group  = "estimation",
                                   option = "control", 
                                   value  = opt_info[["control"]])
      
      testthat::expect_no_error(
        invisible( suppressMessages(suppressWarnings({
          pest = system_estimate_parameters(cfg, 
                                            flowctl         = flowctl, 
                                            analysis_name   = paste0(analysis_name,"_", opt_name), 
                                            archive_results = archive_results)
        })))
     )
    }
  }
})


setwd(test_wd)