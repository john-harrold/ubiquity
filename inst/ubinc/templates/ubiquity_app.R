#clearing the workspace
rm(list=ls())

# Set to TRUE if you wish to deploy the gui on a shiny server
deploying = FALSE

library("shiny")

if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path('library', 'r_general', 'ubiquity.R')) }


if(deploying){
  mywd = getwd()
} else{
  mywd  <-  dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(mywd) }



# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(ubiquity_app=TRUE)


# Start Default:
# The App Default values are set here, feel free to modify them 
# 

# Alter the parameter set at startup here:
cfg              = system_select_set(cfg, 'default')

# If you want to use the system paramters in the App defaults
# below uncomment the following line
# parameters = system_fetch_parameters(cfg) 

cfg$gui$outputs  = names(cfg$options$mi$outputs[1])

cfg$gui$text_bolus_frequency =  1
cfg$gui$text_bolus_number    =  2
cfg$gui$text_nsub            = 10
cfg$gui$text_ci              = 95
cfg$gui$text_save_sim        = "Analysis Name"

cfg$gui$admin_mode        = FALSE 

cfg$gui$check_grid        = FALSE
cfg$gui$check_log         = FALSE
cfg$gui$check_repeatdoses = FALSE
cfg$gui$check_timestamp   = FALSE
cfg$gui$check_variability = FALSE

# Number of lines to display in the user log
cfg$gui$user_log_length = 10

# minimum step size in the output times to simulate
# as a fraction of the total time interval
cfg$gui$minstep = 0.005
# Output time interval (units are the system time units)
# cfg$options$misc$output_times = 'c(0, 100)'

# Normally simulations will begin at time zero, however some systems with
# nonzero steadystate will require a lead-in stabilization time. Set this 
# to a negative value equal in magnitude to the required time to reach 
# steady-state:
cfg$gui$sim_starttime = 0
# If you want to sample less frequently before the start time 
# you can change it here:
# cfg$gui$ss_minstep    = 0.1

# Default colors for lines (solid) and confidence intervals (region)
cfg$gui$colors$solid      = c("darkblue",    "firebrick1", "darkgreen",  "darkorange4" )
cfg$gui$colors$region     = c("cadetblue1",  "pink"      , "olivedrab2", "darkorange"  )



# Set options here:
# If we're not deploying the app we enable debugging
cfg=system_set_option(cfg,group = "logging", 
                         option = "debug", 
                         value  = !deploying)
#  cfg=system_set_option(cfg,group = "simulation", 
#                           option = "solver", 
#                           value = "lsoda")
#
# Force the use of the r-file for integrating
# cfg=system_set_option(cfg, group  = "simulation", 
#                            option = "integrate_with",
#                            value  = "r-file")
# 
# To overwrite solver options use the following:
# cfg=system_set_option(cfg,group  = "solver",
#                           option = "atol",   
#                           value  = 1e-10)
# 
# cfg=system_set_option(cfg,group  = "solver",
#                           option = "rtol",   
#                           value  = 1e-10)

# You can create up to 5 model reports. These are indiviudal RMarkdown
# documents that appear as tabs next to the timecourse portion of the gui. 
# While these are called reports they can be any kind of document used to
# convey detailed information to the user. For example this functionality can
# be used to provide more detailed documentation of how the model is used. 
#
# To create a model report create a template:
# system_fetch_template(cfg, template="Shiny Rmd Report")
# 
# This should create the two files. An RMarkdown report (system_report.Rmd)
# and a script to help with debugging the report (test_system_report.R) in the 
# current working directory. To use this report simply uncomment the following
# lines

#cfg$gui$modelreport_files$R1$title = "Tab Title"
#cfg$gui$modelreport_files$R1$file  = "system_report.Rmd"

# To create more reports just:
#
#  1 Create another Rmd file
#  2 Copy/paste the report lines above
#  3 Increment R1 to R2
#  4 Change the values for the title and file name
#
# You can do this for values up to R5. If you have an Rmd file that does not
# depend on what the user enters into the App. For example, if you simply want
# to document the model and this will not change as the user interacts with
# the App, you may not want to render the html each time the user clicks on the
# report tab. For these cases you can render the report to the html file and simply use that
# as the file name. For the example abvoe, you could do the following:
#
#cfg$gui$modelreport_files$R1$title = "Tab Title"
#cfg$gui$modelreport_files$R1$file  = "system_report.html"
#
# And it can speed things up. 


# To more finely control the gui it may be necessary to define your own user functions. 
# These can be placed in a user library and included here
# cfg$gui$functions$user_def = 'mylibs.r'


# Next you need to tell the gui which functions to use for which task the
# following functions may be specified
#
#  Possible functions:
#     sim_ind - Running individual simulation, the expected format of the output
#               is the same as run_simulation_ubiquity. If you also write your 
#               own plotting functions, then you can have any output format you 
#               wish. You have the following possible inputs: parameters and 
#               cfg.
#               
#     sim_var - Running simulations with variability, the expected format of the output
#               is the same as simulate_subjects. If you also write your own
#               plotting functions, then you can have any output format you
#               wish. You have the following possible inputs: parameters and
#               cfg.
#     
#     plot_ind - Funcion called when plotting individual simulations. You can
#               have parameters, cfg and som as an input. The format of som will be
#               the same as the output of run_simulation_ubiquity unless sim_ind
#               above is returning a custom output.
#     
#     plot_var - Funcion called when plotting simulations with variabilitiy. You can
#               have parameters, cfg and som as an input. The format of som will be
#               the same as the output of simulate_subjects unless sim_ind above
#               is returning a custom output.
#     
#   Possible inputs:
#   parameters - vector of parameters for the currently selected parameter set with 
#                user specified values overwritten
#   cfg -        system configuration variable 
#     
#   som - with variability this has the format of the simulate_subjects output,
#          for individual simulations this has the format of run_simulation_ubiquity
#     

cfg$gui$functions$sim_ind         = 'run_simulation_ubiquity(parameters, cfg)'
cfg$gui$functions$sim_var         = 'simulate_subjects(parameters, cfg)'
#cfg$gui$functions$plot_ind       = 'function_name(cfg, parameters,som)'
#cfg$gui$functions$plot_var       = 'function_name(cfg, parameters,som)'


# Save options, set to FALSE to prevent saving
cfg$gui$save$system_txt  = TRUE
cfg$gui$save$user_log    = TRUE

cfg$gui$dims$timecourse$width  = '850px'
cfg$gui$dims$timecourse$height = '450px'

cfg$gui$dims$paramdist$width  = '850px'
cfg$gui$dims$paramdist$height = '450px'

cfg$gui$dims$modeldiagram$width  = '850px'
cfg$gui$dims$modeldiagram$height = '450px'


#
# To enable/disable elements of the UI you can change the display list elements. By
# default these values are true but if you set them to FALSE they will not be
# visible in the App.
#

# Point selection on the gui
cfg$gui$display$selectpoint      = TRUE

# X-axis controls
cfg$gui$display$autoxaxis        = TRUE

# Y-axis controls
cfg$gui$display$autoyaxis        = TRUE

# Check grid      
cfg$gui$display$checkgrid        = TRUE

# Check log       
cfg$gui$display$checklog         = TRUE

# outputs list
cfg$gui$display$outputs          = TRUE

# variability input tab 
# If the system file has iiv specified and this 
# variable is TRUE it will show the tab
cfg$gui$display$iivtab           = TRUE

#
# Do not make changes below this piont
# Stop: Default

cfg = system_set_option(cfg, 'logging', 'enable', 'yes')
# These contain the overwrites from the defaults
# by default they are null and these will be updated 
# as information is entered by the user in the gui
cfg$gui$parameters               = list()
cfg$gui$covariates               = list()
cfg$gui$iiv                      = list()
cfg$gui$infusion_rates           = list()
cfg$gui$bolus                    = list()
cfg$gui$save_simulation          = list()
cfg$gui$sim_status               = 'Initialization'
cfg$gui$pset_change              = FALSE
                                 
cfg$gui$good_text_autox          = TRUE
cfg$gui$good_text_autoy          = TRUE
                                 
cfg$gui$check_autox              = TRUE
cfg$gui$check_autoy              = TRUE
cfg$gui$text_autox               = c(0,1)
cfg$gui$text_autoy               = c(0,1)

cfg$gui$table_parameters_touched = TRUE
cfg$gui$table_iiv_touched        = TRUE
cfg$gui$table_covariates_touched = TRUE

# The App is setup to run out of transient/rgui
# So the following copies files into that directory 
#
# initializing the working directory

# Setting the working directory
cfg$gui$wd                   = mywd

# Setting deployment option
cfg$gui$deployed = deploying

# Clearing out the old working directory
if(dir.exists(file.path(cfg$options$misc$temp_directory, 'rgui'))){
  unlink(file.path(cfg$options$misc$temp_directory, 'rgui'), recursive=TRUE) }

# Temporary files will be stored in transient/rgui
dir.create(file.path(cfg$options$misc$temp_directory, 'rgui'), recursive=TRUE, showWarnings=FALSE) 


#
# Initializng the inputs to the model defaults
# 
#Bolus 
if(!is.null(cfg$options$inputs$bolus$times$values)){
  # times 
  cfg$gui$bolus$times = cfg$options$inputs$bolus$times$values
  # each dosing compartment
  for(name in names(cfg$options$inputs$bolus$species)){
    cfg$gui$bolus$states[[name]] = cfg$options$inputs$bolus$species[[name]]$values
  }
}

# Infusion rates
if(length(names(cfg$options$inputs$infusion_rates)) > 0){
  for(name in names(cfg$options$inputs$infusion_rates)){
    cfg$gui$infusion_rates[[name]]$times  = cfg$options$inputs$infusion_rates[[name]]$times$values  
    cfg$gui$infusion_rates[[name]]$values = cfg$options$inputs$infusion_rates[[name]]$levels$values 
  }
}

# Covariates
if(length(names(cfg$options$inputs$covariates)) > 0){
  for(name in names(cfg$options$inputs$covariates)){
    cfg$gui$covariates[[name]]$times  = cfg$options$inputs$covariates[[name]]$times$values  
    cfg$gui$covariates[[name]]$values = cfg$options$inputs$covariates[[name]]$values$values 
  }
}

# finding the model diagram
cfg$gui$modeldiagram_file   = NULL
if( file.exists(sprintf('%s%ssystem.png', mywd, .Platform$file.sep))){
  cfg$gui$modeldiagram_file  = sprintf('%s%ssystem.png', mywd, .Platform$file.sep)
}

# Lastly we identify to the underlying functions 
# that we're running things through the App
cfg$options$misc$operating_environment = "gui"

# If no timescale has been set by the user
# we set the time scale to the timescale of the 
# simulation if that one has been defined
if(is.null(cfg$options$misc$TS)){
  for(tsname in names(cfg$options$time_scales)){
    if(cfg$options$time_scales[[tsname]] == 1){
      cfg$options$misc$TS = tsname
    }
  }
}

  
# Saving the system information/state to a file
save(cfg, file=file.path(cfg$options$misc$temp_directory, "rgui", "gui_state.RData"))
# saving the deploying flag to a file in the temporary directory:
# transient/app_base/DEPLOYING
fileConn<-file(file.path(cfg$options$misc$temp_directory,"DEPLOYING"))
if(deploying){
  writeLines(c("TRUE"), fileConn)
} else {
  writeLines(c("FALSE"), fileConn)
}
close(fileConn)

# Lastly if we're not deploying we run the app 
if(!deploying){
  runApp(mywd)} 
