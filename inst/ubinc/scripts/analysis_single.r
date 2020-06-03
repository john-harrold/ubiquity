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
# Individual Simulation:
som = run_simulation_ubiquity(parameters, cfg)

library(ggplot2)
myfig = ggplot() + 
        geom_line(data=som$simout, aes(x=ts.days,   y=C_ng_ml), color="red")  +
        xlab("Time (days)")+
        ylab("C (ng/ml)")
myfig = gg_log10_yaxis(myfig, ylim_min=1e3, ylim_max=3e5)
myfig = prepare_figure("print", myfig)
print(myfig)
# -------------------------------------------------------------------------

ggsave(sprintf('output%ssingle.png', .Platform$file.sep), width=8, height=3.4, plot=myfig)

