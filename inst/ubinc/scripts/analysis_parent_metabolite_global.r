#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

# If we are in a stand alone ubiquity distribution we run 
# from there otherwise we try to load the package
if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
  source(file.path('library', 'r_general', 'ubiquity.R'))
} else { 
  library(ubiquity) }
  
analysis_name = 'parent_metabolite';
# flowctl = 'previous estimate as guess';
# flowctl = 'plot guess';
# flowctl = 'plot previous estimate';
  flowctl = 'estimate';
archive_results = TRUE

# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system()

# set name                  | Description
# -------------------------------------------------------
# default                   | Original Estimates


# The following will estimate a subset of the parameters:
pnames = c('Vp',
           'Vt',
           'CLp',
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

#
# particle swarm
#
 
  library("pso")
 
  cfg = system_set_option(cfg, group  = "estimation",
                               option = "optimizer", 
                               value  = "pso")
  
  cfg = system_set_option(cfg, group  = "estimation",
                               option = "method",
                               value  = "psoptim")
  

#
# genetic algorithm 
#
# library(GA)
#
# cfg = system_set_option(cfg, group  = "estimation",
#                              option = "optimizer", 
#                              value  = "ga")
# 
# cfg = system_set_option(cfg, group  = "estimation",
#                              option = "method",
#                              value  = "ga")
# 
# cfg = system_set_option(cfg, group  = "estimation",
#                              option = "control", 
#                              value  = list(maxiter   = 10000, 
#                                            optimArgs = list(method  = "Nelder-Mead",
#                                                             maxiter = 1000)))


# Defining the cohorts
#
# Clearing all of the cohorts
cfg = system_clear_cohorts(cfg);
 
# One entry for each cohort:
# For more information type:
#
# help system_define_cohort
#
# It is necessary to replace the following compontents:
#
# CHNAME    - cohort name
# COLNAME   - column name in dataset
# ONAME     - output name
# TIMECOL   - column name in dataset with the observation times
# TS        - model timescale corresponding to TIMECOL
# OBSCOL    - column name in dataset with the observation values
# MODOUTPUT - model output corresponding to OBSCOL
#
# Only specify bolus and infusion inputs that are non-zero. Simply ignore
# those that don't exist for the given cohort. Covariates should be specified
# to overwrite the default covariate values
cohort = c()
cohort$name                                      = 'dose_10'
cohort$cf$DOSE                                   = c(10)
cohort$dataset                                   = 'pm_data'
                                                 
cohort$inputs$bolus$Mpb$TIME                     = c(0)  # hours 
cohort$inputs$bolus$Mpb$AMT                      = c(10) # mpk 

cohort$outputs$parent$obs$time                   = 'TIME' 
cohort$outputs$parent$obs$value                  = 'PT'    
cohort$outputs$parent$obs$missing                = -1;
cohort$outputs$parent$model$time                 = 'hours'        
cohort$outputs$parent$model$value                = 'Cpblood'
cohort$outputs$parent$model$variance             = 'slope_parent*PRED^2'
cohort$outputs$parent$options$marker_color       = 'black'
cohort$outputs$parent$options$marker_shape       = 1
cohort$outputs$parent$options$marker_line        = 1 

cohort$outputs$metabolite$obs$time               = 'TIME' 
cohort$outputs$metabolite$obs$value              = 'MT'    
cohort$outputs$metabolite$obs$missing            = -1;
cohort$outputs$metabolite$model$time             = 'hours'        
cohort$outputs$metabolite$model$value            = 'Cmblood'
cohort$outputs$metabolite$model$variance         = 'slope_metabolite*PRED^2'
cohort$outputs$metabolite$options$marker_color   = 'blue'
cohort$outputs$metabolite$options$marker_shape   = 1
cohort$outputs$metabolite$options$marker_line    = 1 
cfg = system_define_cohort(cfg, cohort)
#----------------------------------------------------------

cohort = c()
cohort$name                                      = 'dose_30'
cohort$cf$DOSE                                   = c(30)
cohort$dataset                                   = 'pm_data'
                                                 
cohort$inputs$bolus$Mpb$TIME                     = c(0)  # hours 
cohort$inputs$bolus$Mpb$AMT                      = c(30) # mpk 

cohort$outputs$parent$obs$time                   = 'TIME' 
cohort$outputs$parent$obs$value                  = 'PT'    
cohort$outputs$parent$obs$missing                = -1;
cohort$outputs$parent$model$time                 = 'hours'        
cohort$outputs$parent$model$value                = 'Cpblood'
cohort$outputs$parent$model$variance             = 'slope_parent*PRED^2'
cohort$outputs$parent$options$marker_color       = 'green'
cohort$outputs$parent$options$marker_shape       = 1
cohort$outputs$parent$options$marker_line        = 1 

cohort$outputs$metabolite$obs$time               = 'TIME' 
cohort$outputs$metabolite$obs$value              = 'MT'    
cohort$outputs$metabolite$obs$missing            = -1;
cohort$outputs$metabolite$model$time             = 'hours'        
cohort$outputs$metabolite$model$value            = 'Cmblood'
cohort$outputs$metabolite$model$variance         = 'slope_metabolite*PRED^2'
cohort$outputs$metabolite$options$marker_color   = 'red'
cohort$outputs$metabolite$options$marker_shape   = 1
cohort$outputs$metabolite$options$marker_line    = 1 
cfg = system_define_cohort(cfg, cohort)

#----------------------------------------------------------
# performing estimation or loading guess/previous results
pest = system_estimate_parameters(cfg, 
                                  flowctl         = flowctl, 
                                  analysis_name   = analysis_name, 
                                  archive_results = archive_results)

# Simulating the system at the estimates
erp = system_simulate_estimation_results(pest = pest, cfg = cfg) 

plot_opts = c()

plot_opts$outputs$metabolite$yscale   = 'log'   
plot_opts$outputs$metabolite$ylabel   = 'Metabolite'
plot_opts$outputs$metabolite$ylim     = c(1, 100)
plot_opts$outputs$metabolite$xlabel   = 'Time (hours)'

plot_opts$outputs$parent$yscale        = 'log'
plot_opts$outputs$parent$ylabel        = 'Parent'
plot_opts$outputs$parent$xlabel        = 'Time (hours)'


# Plotting the simulated results at the estimates 
# These figures will be placed in output/
system_plot_cohorts(erp, plot_opts, cfg, prefix=analysis_name)
