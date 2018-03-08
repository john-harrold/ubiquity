#clearing the workspace
rm(list=ls())

source("library/r_general/ubiquity.r");
if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
  source(file.path('library', 'r_general', 'ubiquity.R'))
} else { 
  library(ubiquity) }

# Use this script to debug the system_report.Rmd file
# You need to run the model in an undeployed fashion. This 
# will generate the cfg and som variables that can then be
# loaded here:


# this creates cfg:
load(file=sprintf('transient%srgui%sdefault%sgui_state.RData', .Platform$file.sep, .Platform$file.sep, .Platform$file.sep))

# this creates the som:
load(file=sprintf('transient%srgui%sdefault%sgui_som.RData', .Platform$file.sep, .Platform$file.sep, .Platform$file.sep))

# Next we define all of the functions that will be available:
source(sprintf("library%sr_general%subiquity.r", .Platform$file.sep, .Platform$file.sep))
source(sprintf('transient%sauto_rcomponents.r', .Platform$file.sep))

# Storing the som and cfg variables to be 
# passed to the render function
params = c()
params$som = som
params$cfg = cfg

# Rendering the report
rmarkdown::render("system_report.Rmd", params = params)
