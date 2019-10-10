#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)
require("ggplot")
require("doParallel")

# If we are in a stand alone ubiquity distribution we run 
# from there otherwise we try to load the package
if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
  source(file.path('library', 'r_general', 'ubiquity.R'))
} else { 
  library(ubiquity) }


# Creating the mAb system file
system_new(file_name="system.txt", system_file="mab_pk", overwrite = TRUE)

# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system()

# set name                  | Description
# -------------------------------------------------------
# default                   | TMDD: Membrane bound target

cfg = system_select_set(cfg, "default")

# fetching the parameter values
parameters = system_fetch_parameters(cfg)


# The following applies to both individual and stochastic simulations:
# Define the solver to use
cfg=system_set_option(cfg,group = "simulation", option = "solver", value = "lsoda")
# Specify the output times 
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "output_times", 
                           seq(0,28*7*4,5))

# -------------------------------------------------------------------------
# Enabling titration 
cfg=system_set_option(cfg,
                      group       = "titration",
                      option      = "titrate",   
                      value       = TRUE)

# Creating a titration rule
cfg=system_new_tt_rule(cfg,   
                       name       = "ivdose",
                       times      = c(0, 6, 12, 18, 24),   
                       timescale  = "months")
                                  
# Adding conditions to that rule
cfg=system_set_tt_cond(cfg,       
                       name       = "ivdose",
                       cond       = "Cc < 900",   
                       action     = "SI_TT_BOLUS[state='At', values=700, times=0, repdose='last', number=11, interval=14]",
                       value      = "700")

cfg=system_set_tt_cond(cfg,       
                       name       = "ivdose",
                       cond       = "Cc > 900",   
                       action     = "SI_TT_BOLUS[state='At', values=600, times=0, repdose='last', number=11, interval=14]",
                       value      = "600")

# Uncomment the following to parallelize the simulations
# cfg=system_set_option(cfg, group  = "simulation",
#                            option = "parallel",    
#                            value  = "multicore")
# 
# cfg=system_set_option(cfg, group  = "simulation",
#                            option = "compute_cores", 
#                            value  = detectCores() - 1)
                           

# Setting the number of subjects
cfg = system_set_option(cfg, group="stochastic", option="nsub",    value=20)

# Simulating the subjects
som= simulate_subjects(parameters, cfg)

# Converting the structured output into a flat format (data frame)
sdf = som_to_df(cfg, som)

# Pulling the subject IDs of the individuals which can support a lower dose
Low_Dose = unique(sdf$ID[sdf$tt.ivdose.value==600])
