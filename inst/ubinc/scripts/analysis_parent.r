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

analysis_name = 'parent_d1030'
# flowctl = 'previous estimate as guess'
# flowctl = 'plot previous estimate'
# flowctl = 'plot guess'
  flowctl = 'estimate'
archive_results = TRUE



# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# set name                  | Description
# -------------------------------------------------------
# default                   | TMDD: Membrane bound target

pnames = c('Vp', 'Vt', 'CLp', 'Q')
cfg = system_select_set(cfg, "default", pnames)

# #
# # Simulation options 
# #
# 
# Specify the output times used for smooth profiles
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "output_times", 
                           seq(0,100,1))

cfg = system_load_data(cfg, dsname     = "pm_data", 
                            data_file  = "pm_data.csv")

# cat(system_view(cfg, 'datasets'))

# Defining the cohorts
#
# Clearing all of the cohorts
cfg = system_clear_cohorts(cfg)

#-------------------------------------------------------
# 10 mpk cohort

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
         variance       = "1")

# Plot formatting
cohort[["outputs"]][["Parent"]][["options"]] = list(
         marker_color   = "black",
         marker_shape   = 1,
         marker_line    = 2 )

cfg = system_define_cohort(cfg, cohort)
#-------------------------------------------------------

#-------------------------------------------------------
# 30 mpk cohort
cohort = list(
  name         = "dose_30",
  cf           = list(DOSE      = c(30)),
  dataset      = "pm_data",
  inputs       = NULL,
  outputs      = NULL)

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
         variance       = "1")

# Plot formatting
cohort[["outputs"]][["Parent"]][["options"]] = list(
         marker_color   = "red",
         marker_shape   = 2,
         marker_line    = 2 )

cfg = system_define_cohort(cfg, cohort)
#-------------------------------------------------------

#cat(system_view(cfg, field="datasets"))
#cat(system_view(cfg, field="cohorts"))
#cat(system_view(cfg, field="estimation"))
#cat(system_view(cfg, field="simulation"))


# cfg = system_set_option(cfg, group="estimation",
#                              option="method",
#                              value = "BFGS")
# 
# 
# cfg = system_set_option(cfg, group="estimation",
#                              option="optimizer", 
#                              value = "optimx")
# 
# 

# performing estimation or loading guess/previous results
pest = system_estimate_parameters(cfg, 
                                  flowctl         = flowctl, 
                                  analysis_name   = analysis_name, 
                                  archive_results = archive_results)

# Simulating the system at the estimate
erp = system_simulate_estimation_results(pest = pest, cfg = cfg) 

plot_opts = c()

plot_opts$outputs$Parent$yscale       = 'log'
plot_opts$outputs$Metabolite$yscale   = 'linear'

# plot_opts$outputs$ONAME$yscale   = 'linear' # 'linear' or 'log'
# plot_opts$outputs$ONAME$ylim     = c(0,1) # NULL
# plot_opts$outputs$ONAME$xlim     = c(0,1) # NULL

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
