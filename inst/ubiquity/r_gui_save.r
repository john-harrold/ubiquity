#clearing the workspace
rm(list=ls())
# Uncomment to set the script directory as the working directory
# setwd(dirname(sys.frame(tail(grep('source',sys.calls()),n=1))$ofile))
graphics.off()
library("deSolve")
library("ggplot2")
require("gdata")
library("foreach")
library("doParallel")
library("doRNG")

<user_def>

# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv



# --------------------------------------------------------------
# By default the system uses the stand-alone libraries generated
# by the GUI during export:
source("<libfile>");
# --------------------------------------------------------------
# # Uncomment this block to rebuild the system and incorporate
# # modifications to the system.txt into the simulation results
# source("library/r_general/ubiquity.r");
# # Rebuilding the system (R scripts and compiling C code)
# build_system()
# # loading the different functions
# source("transient/auto_rcomponents.r");
# --------------------------------------------------------------

# Loading the system information
cfg = system_fetch_cfg()
cfg = system_select_set(cfg, "<pset>")

<parameters>
# The previous statement sets 'parameters' to the values 
# in the currently selected parameter set. To overwrite 
# a specific parameter uncomment the following statement 
# and replace PNAME with the name of the parameter 
# and VALUE with the desired value:
#
# parameters$PNAME = VALUE;

# Setting simulation options: 
<options>

# Forcing the r-script to be used
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "integrate_with",
                           value  = "r-file")

<bheader><bolus><rheader><infusion_rates><cheader><covariates><iheader><iiv>

# To view the system state before the simulations are run
# uncomment the following line:
# cat(system_view(cfg))

# --------------------------------------------------------------
# Running simulation(s)
<sim>

<save_csv>
# --------------------------------------------------------------


# --------------------------------------------------------------
# plotting timecourse
<plot_timecourse><plot_paramdist>
# --------------------------------------------------------------
