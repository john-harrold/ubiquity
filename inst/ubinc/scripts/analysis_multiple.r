#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

# if we are in a stand alone distribution we run from there
# otherwise we try to load the package
if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
  source(file.path('library', 'r_general', 'ubiquity.R'))
} else { 
  library(ubiquity) }

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))


# set name   | Description
# -------------------------------------------------------
# default    | mAb in Humans
# Selecting the default paraemter set
cfg = system_select_set(cfg, 'default')

# Fetching the parameter values
parameters = system_fetch_parameters(cfg)

# To overwrite the default dosing uncomment
cfg = system_zero_inputs(cfg)
cfg = system_set_bolus(cfg, state = "At",
                           times  = c(  0.0,  14.0,  28.0),
                           values = c(400.0, 400.0, 400.0))


# The following applies to both individual and stochastic simulations:
# Define the solver to use
cfg=system_set_option(cfg,group   = "simulation",
                          option  = "solver",
                          value   = "lsoda")

# Specify the output times 
cfg=system_set_option(cfg, group  = "simulation",
                           option = "output_times",
                           value  = seq(0,80,.1))
# -------------------------------------------------------------------------
# # Stochastic Simulation:
cfg=system_set_option(cfg, group  = "stochastic",
                           option = "nsub",
                           value  = 200)

# Uncomment the following to parallelize the simulations
# library(doParallel)
# cfg=system_set_option(cfg, group  = "simulation",
#                            option = "parallel",    
#                            value  = "multicore")
# 
# cfg=system_set_option(cfg, group  = "simulation",
#                            option = "compute_cores", 
#                            value  = detectCores() - 1)
# 
som  = simulate_subjects(parameters, cfg)

graphics.off()
library(ggplot2)
myfig = ggplot(som$tcsummary, aes(x=ts.days, y=o.C_ng_ml.mean)) +
               geom_ribbon(aes(ymin=o.C_ng_ml.lb_ci, 
                               ymax=o.C_ng_ml.ub_ci), 
                               fill="lightblue", 
                               alpha=0.6) +
               geom_line(linetype="solid", size=0.7, color="blue")  +
               geom_line(aes(x=ts.days, y=o.C_ng_ml.ub_ci), linetype="dashed", size=0.2, color="blue")  +
               geom_line(aes(x=ts.days, y=o.C_ng_ml.lb_ci), linetype="dashed", size=0.2, color="blue")  +
               xlab("Time (days)")+
               ylab("C (ng/ml) (units)")+
               guides(fill="none") 


myfig = gg_log10_yaxis(myfig , ylim_min=1e3, ylim_max=3e5)
myfig = prepare_figure("print", myfig)
print(myfig)

ggsave(sprintf('output%smultiple.png', .Platform$file.sep), width=8, height=3.4, plot=myfig)

# sdf = som_to_df(cfg, som)
# -------------------------------------------------------------------------
