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

# flowctl = 'previous estimate as guess';
# flowctl = 'plot guess';
# flowctl = 'plot previous estimate';
  flowctl = 'estimate';
archive_results = TRUE

analysis_name = 'parent_metabolite_nm_data';


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
                           linspace(0,50,100))

# Loading Datasets
#
cfg = system_load_data(cfg, dsname     = "nm_pm_data", 
                            data_file  = "nm_data.csv")


# Defining the cohorts
#
# Clearing all of the cohorts
cfg = system_clear_cohorts(cfg);
 


# Only including the 10 and 30 mpk doses 
filter = list()
filter$DOSE = c(10, 30)


# Mapping information: 
OBSMAP = list()
OBSMAP$PT = list(variance     = 'slope_parent*PRED^2',
                 CMT          =  1,
                 output       = 'Cpblood', 
                 missing      =  -1 )

OBSMAP$MT = list(variance     = 'slope_metabolite*PRED^2',
                 CMT          =  2,
                 output       = 'Cmblood', 
                 missing      =  -1 )

INPUTMAP = list()
INPUTMAP$bolus$Mpb$CMT_NUM             =  1

cfg = system_define_cohorts_nm(cfg, 
                               DS       = 'nm_pm_data',
                               col_ID   = 'ID',
                               col_CMT  = 'CMT',
                               col_DV   = 'DV',
                               col_TIME = 'TIME',
                               col_AMT  = 'AMT',
                               col_RATE = 'RATE',
                               col_EVID = 'EVID',
                               col_GROUP= 'DOSE',
                               filter   =  filter, 
                               INPUTS   =  INPUTMAP,
                               OBS      =  OBSMAP)
                                


#  #----------------------------------------------------------
#  # performing estimation or loading guess/previous results
pest = system_estimate_parameters(cfg, 
                                  flowctl         = flowctl, 
                                  analysis_name   = analysis_name, 
                                  archive_results = archive_results)


# Simulating the system at the estimates
erp = system_simulate_estimation_results(pest = pest, cfg = cfg) 

plot_opts = c()

plot_opts$outputs$MT$yscale   = 'log'   
plot_opts$outputs$MT$ylabel   = 'Metabolite'
#plot_opts$outputs$MT$ylim     = c(1, 100)
plot_opts$outputs$MT$xlabel   = 'Time (hours)'

plot_opts$outputs$PT$yscale   = 'log'
plot_opts$outputs$PT$ylabel   = 'Parent'
plot_opts$outputs$PT$xlabel   = 'Time (hours)'


# Plotting the simulated results at the estimates 
# These figures will be placed in output/
system_plot_cohorts(erp, plot_opts, cfg, analysis_name=analysis_name)

# Writing the results to a PowerPoint report
# cfg = system_report_init(cfg)
# cfg = system_report_estimation(cfg=cfg, analysis_name=analysis_name)
# cfg = system_report_save(cfg=cfg, output_file=file.path("output",paste(analysis_name, "-report.pptx", sep="")))



