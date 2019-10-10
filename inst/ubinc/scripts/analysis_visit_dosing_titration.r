#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)
require("ggplot2")

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
                           seq(0,28*7*4,1))


# -------------------------------------------------------------------------


# Same using titration
# Enabling titration 
cfg=system_set_option(cfg,
                      group       = "titration",
                      option      = "titrate",   
                      value       = TRUE)

# Creating a titration event            
cfg=system_new_tt_rule(cfg,   
                       name       = "ivdose",
                       times      = c(0, 6, 12, 18, 24),   
                       timescale  = "months")
                                  
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

                           

som_tt = run_simulation_titrate(parameters, cfg)

myfig = ggplot() + 
        geom_line(data=som_tt$simout, aes(x=ts.weeks,   y=Cc), color="blue")  


myfig = prepare_figure('present', myfig) 

print(myfig)

ggsave(sprintf('output%svisit_titration.png', .Platform$file.sep), width=8, height=3.4, plot=myfig)

