#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

# If we cannot load the ubiquity package we try the stand alone distribution
if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path("library", "r_general", "ubiquity.R")) }

# For documentation explaining how to modify the commands below see 
# the simulation vignette:
# vignette(package="ubiquity", topic="Simulation")
# Or the simulation tutorial at the bottom of this page:
# http://r.ubiquity.tools/


# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system-in_vitro.txt",
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# set name                  | Description
# -------------------------------------------------------
# default                   | default

cfg = system_select_set(cfg, "default")

# fetching the parameter values
parameters = system_fetch_parameters(cfg)

# The following applies to both individual and stochastic simulations:
# Define the solver to use
cfg=system_set_option(cfg,group = "simulation", option = "solver", value = "lsoda")
 
# To overwrite solver options use the following:
# cfg=system_set_option(cfg,group  = "solver",
#                           option = "atol",   
#                           value  = 1e-10)
# cfg=system_set_option(cfg,group  = "solver",
#                           option = "rtol",   
#                           value  = 1e-10)

# By default, important times will be included in the simulation output
# e.g., bolus times, sampling before and after rate switches, etc.
# uncomment to only evaluate at the output times specified above
# cfg=system_set_option(cfg, group  = "simulation", 
#                            option = "include_important_output_times", 
#                            value  = "no")
# Uncomment to specify ode file to use
# cfg=system_set_option(cfg, group  = "simulation", 
#                            option = "integrate_with",
#                            value  = "r-file")

# To overwrite the default dosing uncomment the following
# Setting all dosing to zero
# cfg = system_set_bolus(cfg, state   ="Cp_A", 
#                             times   = c(0),  #  hours
#                             values  = c(1.0))  #  mg
# cfg = system_set_bolus(cfg, state   ="Cp_B", 
#                             times   = c(0),  #  hours
#                             values  = c(1.0))  #  mg


# We're doing an in vitro analysis so we set dynamic to 
# FALSE to turn off the ODES
cfg=system_set_option(cfg,group  = "simulation", 
                          option = "dynamic", 
                          value  = FALSE)

# The more output times the slower the simulation, so here we just specify two
# output times (you have to have a start and stop time):
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "output_times", 
                           seq(0,1, 1))

# Here we zero out the default inputs:
cfg = system_zero_inputs(cfg) 

# Here we are creating the different experimental conditions. I'm going to
# create a smooth range of A values and just 3 B values
C_A0s = logspace(-3,4,10)
C_B0s= c(.1,100, 500, 1000)


set.seed(5446)
er_data = NULL
for(C_A0 in C_A0s){
  for(C_B0 in C_B0s){
    # 3 samples per point
    for(nsam in c(1:3)){
      ptmp = parameters
      ptmp$C_A0 = C_A0
      ptmp$C_B0 = C_B0
      som = run_simulation_ubiquity(ptmp, cfg)
      # Adding a little noise to make it more realistic
      tmp_Effect =  som$simout$Effect[1]*exp(rnorm(1, mean=0, sd=.05))
      er_data = rbind(er_data, 
        data.frame(
           C_A0   = C_A0,
           C_B0   = C_B0,
           Effect = tmp_Effect
        )
      )
    }
  }
}

er_data = dplyr::mutate(er_data, C_B0 = as.factor(C_B0)) |>
  dplyr::group_by(C_A0, C_B0) |>
  dplyr::mutate(ave_eff = mean(Effect)) |>
  dplyr::mutate(treat = paste0("A_", C_A0, "_B_", C_B0)) |>
  dplyr::mutate(treat = stringr::str_replace_all(treat, "\\.", "_")) |>
  dplyr::mutate(samp_time  = 1)


readr::write_csv(er_data, file="in_vitro_er_data.csv")

library(ggplot2)
p = ggplot(data=er_data) +
    geom_point(aes(x=C_A0, y=Effect, group=C_B0, color=C_B0)) +
     geom_line(aes(x=C_A0, y=ave_eff, group=C_B0, color=C_B0)) + 
     scale_x_log10()

print(p)
