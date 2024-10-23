#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

# If we cannot load the ubiquity package we try the stand alone distribution
if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path("library", "r_general", "ubiquity.R")) }

# -------------------------------------------------------------------------

# flowctl       = "plot previous estimate"
# flowctl       = "previous estimate as guess"
  flowctl       = "estimate"
# flowctl       = "plot guess"
analysis_name   = "in_vitro_example"
archive_results = TRUE 

# For documentation explaining how to modify the commands below see
# the estimation vignette:
# vignette(package="ubiquity", topic="Estimation")
# Or the estimation tutorial at the bottom of this page:
# http://r.ubiquity.tools/

# -------------------------------------------------------------------------
# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system-in_vitro.txt",
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))


# Initializing the log file ./transient/ubiquity.log
cfg = system_log_init(cfg)

# To fix parameters, simply specify only those you want to estimate here:
pnames = c("PSI")
cfg = system_select_set(cfg, "default", pnames)

cfg = system_set_guess(cfg, pname="PSI",  value=1.0,   lb=NULL, ub=NULL) 
 
# -------------------------------------------------------------------------
# Setting options
# 
# Specify output times here using sparse sampling (large time steps) to make
# the estimation quick. See down below where you can specify the sampling to 
# generate smooth profiles when plotting. This will be the
# default output times unless overwritten at the cohort level:
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "output_times", 
                           seq(0,1,1))

# We're doing an in vitro analysis so we set dynamic to 
# FALSE to turn off the ODES
cfg=system_set_option(cfg,group  = "simulation", 
                          option = "dynamic", 
                          value  = FALSE)

# -------------------------------------------------------------------------
# Loading Datasets

# Below you'll see how we add a cohort for each treatment group. To do that I'm loading
# the dataset here so it will be available to in both this environment and in
# the estimation environment
er_data = readr::read_csv("in_vitro_er_data.csv")

# This loads the data for estimation
cfg = system_load_data(cfg, dsname     = "er_data",
                            data_file  = er_data)


# -------------------------------------------------------------------------
# Defining the cohorts
# Clearing all of the cohorts
cfg = system_clear_cohorts(cfg)


# Each treatment group in this dataset is a cohort. So I'm going 
# to process it group by group:
for(tmp_treat in unique(er_data$treat)){

  treat_recs = dplyr::filter(er_data, treat == tmp_treat)

  # Here  we just assign the cohort name to the treatment name. And then we
  # setup the cohort filter (cf) to only include records with the current
  # treatment group. We need to add the cohort parameters (cp) field here as
  # well. This will fix the specified parameters here at the value for the
  # current treatment group. 
  cohort = list(
    name         = tmp_treat,
    cf           = list(
                     treat     = c(tmp_treat)),
    cp           = list(
                     C_A0 = treat_recs$C_A0[1],
                     C_B0 = treat_recs$C_B0[1]),
    inputs       = NULL,
    outputs      = NULL,
    dataset      = "er_data")
  
  # There are no inputs so we just leave them as NULL above and we just need
  # to define the output Effect.
  cohort[["outputs"]][["Effect"]] = list()
  
  # Here samp_time is just an arbitrary value because we are analyzing a
  # static system. It only has to be within the simulated output times defined
  # above.
  cohort[["outputs"]][["Effect"]][["obs"]] = list(
           time           = "samp_time",
           value          = "Effect",
           missing        = -1)
     
  # Again timescale here is unimportant because of the static nature. You just
  # have to define a legitimate timescale in the model
  cohort[["outputs"]][["Effect"]][["model"]] = list(
           time           = "hours",       
           value          = "Effect",
           variance       = "PRED^2")
  
  cfg = system_define_cohort(cfg, cohort)
} 
# -------------------------------------------------------------------------
# performing estimation or loading guess/previous results
pest = system_estimate_parameters(cfg, 
                                  flowctl         = flowctl, 
                                  analysis_name   = analysis_name, 
                                  archive_results = archive_results)

# -------------------------------------------------------------------------

# Simulating the system at the estimates
erp = system_simulate_estimation_results(pest = pest, cfg = cfg) 
# -------------------------------------------------------------------------


# This will merge the simulated results at the estimate (erp) with the
# observation dataset (er_data) to make some plots:
df_orig = er_data |>
  dplyr::select(C_A0, C_B0, ave_eff, treat) |>
  dplyr::distinct()

df_est = erp$pred |>
  dplyr::filter(!SMOOTH) |>
  dplyr::rename(treat = COHORT)

df_plot = dplyr::full_join(df_est, df_orig, by="treat") |>
  dplyr::mutate(C_B0 = as.factor(C_B0))

library(ggplot2)
p = ggplot(data=df_plot) +
    geom_point(aes(x=C_A0, y=OBS,  group=C_B0, color=C_B0)) +
     geom_line(aes(x=C_A0, y=PRED, group=C_B0, color=C_B0)) + 
     scale_x_log10()
p
