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

cohort$outputs$Parent$obs$time                   = 'TIME' 
cohort$outputs$Parent$obs$value                  = 'PT'    
cohort$outputs$Parent$obs$missing                = -1;
cohort$outputs$Parent$model$time                 = 'hours'        
cohort$outputs$Parent$model$value                = 'Cpblood'
cohort$outputs$Parent$model$variance             = 'slope_parent*PRED^2'
cohort$outputs$Parent$options$marker_color       = 'black'
cohort$outputs$Parent$options$marker_shape       = 1
cohort$outputs$Parent$options$marker_line        = 1 

cohort$outputs$Metabolite$obs$time               = 'TIME' 
cohort$outputs$Metabolite$obs$value              = 'MT'    
cohort$outputs$Metabolite$obs$missing            = -1;
cohort$outputs$Metabolite$model$time             = 'hours'        
cohort$outputs$Metabolite$model$value            = 'Cmblood'
cohort$outputs$Metabolite$model$variance         = 'slope_metabolite*PRED^2'
cohort$outputs$Metabolite$options$marker_color   = 'blue'
cohort$outputs$Metabolite$options$marker_shape   = 1
cohort$outputs$Metabolite$options$marker_line    = 1 
cfg = system_define_cohort(cfg, cohort)
#----------------------------------------------------------

cohort = c()
cohort$name                                      = 'dose_30'
cohort$cf$DOSE                                   = c(30)
cohort$dataset                                   = 'pm_data'
                                                 
cohort$inputs$bolus$Mpb$TIME                     = c(0)  # hours 
cohort$inputs$bolus$Mpb$AMT                      = c(30) # mpk 

cohort$outputs$Parent$obs$time                   = 'TIME' 
cohort$outputs$Parent$obs$value                  = 'PT'    
cohort$outputs$Parent$obs$missing                = -1;
cohort$outputs$Parent$model$time                 = 'hours'        
cohort$outputs$Parent$model$value                = 'Cpblood'
cohort$outputs$Parent$model$variance             = 'slope_parent*PRED^2'
cohort$outputs$Parent$options$marker_color       = 'green'
cohort$outputs$Parent$options$marker_shape       = 1
cohort$outputs$Parent$options$marker_line        = 1 

cohort$outputs$Metabolite$obs$time               = 'TIME' 
cohort$outputs$Metabolite$obs$value              = 'MT'    
cohort$outputs$Metabolite$obs$missing            = -1;
cohort$outputs$Metabolite$model$time             = 'hours'        
cohort$outputs$Metabolite$model$value            = 'Cmblood'
cohort$outputs$Metabolite$model$variance         = 'slope_metabolite*PRED^2'
cohort$outputs$Metabolite$options$marker_color   = 'red'
cohort$outputs$Metabolite$options$marker_shape   = 1
cohort$outputs$Metabolite$options$marker_line    = 1 
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

# Writing the results to a PowerPoint report
# cfg = system_report_init(cfg)
# cfg = system_report_estimation(cfg=cfg, analysis_name=analysis_name)
# cfg = system_report_save(cfg=cfg, output_file=file.path("output",paste(analysis_name, "-report.pptx", sep="")))

