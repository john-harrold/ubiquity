#clearing the workspace
rm(list=ls())

# Set to TRUE if you wish to deploy the gui on a shiny server
deploying = FALSE

require(shiny)
require(shinydashboard)
require(deSolve)
require(ggplot2)
require(foreach)
require(doParallel)
require(rhandsontable)
require(rstudioapi)

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
# Stop Default:
#----------------------------------------
# Do not make changes below this piont
cfg$gui$wd                       = mywd

# Setting deployment option
cfg$gui$deployed                 = deploying

# Default app status
cfg$gui$app_status               = 'Initialization'

cfg$gui$user_log_length          = 20

# The App is setup to run out of transient/rgui
# So the following copies files into that directory 
#
# initializing the working directory

# Clearing out the old working directory
if(dir.exists(file.path(cfg$options$misc$temp_directory, 'rgui'))){
  unlink(file.path(cfg$options$misc$temp_directory, 'rgui'), recursive=TRUE) }

# Temporary files will be stored in transient/rgui
dir.create(file.path(cfg$options$misc$temp_directory, 'rgui'), recursive=TRUE, showWarnings=FALSE) 

#----------------------------------------
# Saving the system inforamtion/state to a file
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

if(!deploying){
  runApp(mywd)} 
