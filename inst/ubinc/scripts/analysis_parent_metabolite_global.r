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

analysis_name = 'parent_metabolite_global';
# flowctl = 'previous estimate as guess';
# flowctl = 'plot guess';
# flowctl = 'plot previous estimate';
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

# Global optimization with the particle swarm optimizer:
#
library("pso")

cfg = system_set_option(cfg, group  = "estimation",
                             option = "optimizer", 
                             value  = "pso")

cfg = system_set_option(cfg, group  = "estimation",
                             option = "method",
                             value  = "psoptim")

cfg = system_set_option(cfg, group  = "estimation",
                             option = "control", 
                             value  = list(trace  = TRUE,
                                           maxit  = 200,
                                           REPORT = 10))

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

#----------------------------------------------------------
# performing estimation or loading guess/previous results
pest = system_estimate_parameters(cfg, 
                                  flowctl         = flowctl, 
                                  analysis_name   = analysis_name, 
                                  archive_results = archive_results)


# Simulating the system at the estimates
erp = system_simulate_estimation_results(pest = pest, cfg = cfg) 

plot_opts = c()

plot_opts$outputs$Metabolite$yscale   = 'log'   
plot_opts$outputs$Metabolite$ylabel   = 'Metabolite'
plot_opts$outputs$Metabolite$ylim     = c(1, 100)
plot_opts$outputs$Metabolite$xlabel   = 'Time (hours)'

plot_opts$outputs$Parent$yscale        = 'log'
plot_opts$outputs$Parent$ylabel        = 'Parent'
plot_opts$outputs$Parent$xlabel        = 'Time (hours)'


# Plotting the simulated results at the estimates 
# These figures will be placed in output/
system_plot_cohorts(erp, plot_opts, cfg, analysis_name=analysis_name)
#-------------------------------------------------------
# Writing the results to a PowerPoint report
  cfg = system_rpt_read_template(cfg, template="PowerPoint")
  cfg = system_rpt_estimation(cfg=cfg, analysis_name=analysis_name)
  system_rpt_save_report(cfg=cfg, output_file=file.path("output",paste(analysis_name, "-report.pptx", sep="")))
#-------------------------------------------------------
# Writing the results to a Word report
  cfg = system_rpt_read_template(cfg, template="Word")
  cfg = system_rpt_estimation(cfg=cfg, analysis_name=analysis_name)
  system_rpt_save_report(cfg=cfg, output_file=file.path("output",paste(analysis_name, "-report.docx", sep="")))
#-------------------------------------------------------

