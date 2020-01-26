#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

# If we cannot load the ubiquity package we try the stand alone distribution
if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path('library', 'r_general', 'ubiquity.R')) }

# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system-mab_pk.txt",
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# set name                  | Description
# -------------------------------------------------------
# default                   | mAb in Humans

cfg = system_select_set(cfg, "default")

# fetching the parameter values
parameters = system_fetch_parameters(cfg)

# The following applies to both individual and stochastic simulations:
# Define the solver to use
cfg=system_set_option(cfg,group = "simulation", option = "solver", value = "lsoda")


# cfg=system_set_option(cfg, group  = "simulation", 
#                            option = "include_important_output_times", 
#                            value  = "no")
# 
# Sample times in hours
# the grps will be used to split up the data set for sparse sampling
st_grp1         = c(1,  24, 336)
st_grp2         = c(4,  72, 504)
st_grp3         = c(8, 168, 671.9999)
sample_times_sd = sort(c(st_grp1, st_grp2, st_grp3))
sample_times_md = sort(c(st_grp1, st_grp2, st_grp3))

ndose = 6
28*24*(ndose-1)

# Cmax and Ctrough times for the intermediate doses
tmax_iv         = 1:(ndose-2)*28*24+1
tmax_sc         = 1:(ndose-2)*28*24+168
ttrough         = 1:(ndose-2)*28*24+671.99
sample_times_md = sort(unique(c(sample_times_sd, sample_times_sd+3360, tmax_iv, tmax_sc, ttrough)))



# To overwrite the default dosing uncomment the following
# Setting all dosing to zero

doses  = c(30,   120,  300)
dcmpts = c("Cc", "At", "Cc")
routes = c("iv bolus", "extra-vascular", "iv bolus")


simfull_sd    =  NULL
simsparse_sd  =  NULL

simfull_md    =  NULL
simsparse_md  =  NULL

sparse_idx = 1
sub_idx = 1
dose_idx = 1
for(dose in doses){
  dcmpt = dcmpts[dose_idx]
  route = routes[dose_idx]
  dose_idx = dose_idx + 1
  # Single dose simulations
  ot_sd = sort(unique(c(linspace(0,28,100), sample_times_sd)))
  cfg=system_set_option(cfg, group  = "simulation", 
                             option = "output_times", 
                             linspace(0,28,100))
  cfg = system_zero_inputs(cfg) 
  cfg = system_set_bolus(cfg, state   = dcmpt, 
                              times   = c( 0.0 ),  #  day
                              values  = c( dose))  #  mg
  cfg = system_set_option(cfg, group="stochastic", option="nsub",    value=18)

  som_sd = simulate_subjects(parameters, cfg)

  # Multiple dose simulations
  # Q4W for 6 months 
  ot_md =sort(unique(c(linspace(0,28*6,100),sample_times_md)))
  cfg=system_set_option(cfg, group  = "simulation", 
                             option = "output_times", 
                             ot_md)
  cfg = system_zero_inputs(cfg) 
  cfg = system_set_bolus(cfg, state   = dcmpt, 
                              times   = c( 0:(ndose-1)*28),  #  day
                              values  = rep(dose, times=ndose))  #  mg
  cfg = system_set_option(cfg, group="stochastic", option="nsub",    value=18)
  som_md = simulate_subjects(parameters, cfg)

  for(tmpidx in 1:length(som_sd$subjects$parameters[,1])){
    
    # -------------------------------------------------------------------------
    # Making the full dataset
    # Single dose
    C_ng_ml = approx(x=som_sd$times$ts.hours, y=som_sd$outputs$C_ng_ml[tmpidx, ], xout=sample_times_sd)$y

    tmpdf = data.frame( ID       = rep(sub_idx, length(sample_times_sd)),
                        TIME_HR  =  sample_times_sd,
                        C_ng_ml  = C_ng_ml,
                        DOSENUM = floor(sample_times_sd/24/28 +1),
                        DOSE     = dose)

    tmpdf$ROUTE   = route
  
    if(is.null(simfull_sd)){
      simfull_sd = tmpdf
    } else {
      simfull_sd = rbind(simfull_sd, tmpdf)
    }

    # Multiple dose 
    C_ng_ml = approx(x=som_md$times$ts.hours, y=som_md$outputs$C_ng_ml[tmpidx, ], xout=sample_times_md)$y

    tmpdf = data.frame( ID       = rep(sub_idx, length(sample_times_md)),
                        TIME_HR  =  sample_times_md,
                        C_ng_ml  = C_ng_ml,
                        DOSENUM = floor(sample_times_md/24/28 +1),
                        DOSE     = dose)
    tmpdf$ROUTE   = route
    if(is.null(simfull_md)){
      simfull_md = tmpdf
    } else {
      simfull_md = rbind(simfull_md, tmpdf)
    }
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    # Now making the sparse dataset
    if(sparse_idx == 1){
      st_grp = st_grp1
    } else if (sparse_idx == 2){
      st_grp = st_grp2
    } else if (sparse_idx == 3){
      st_grp = st_grp3
    }

    C_ng_ml_grp = approx(x=som_sd$times$ts.hours, y=som_sd$outputs$C_ng_ml[tmpidx, ], xout=st_grp)$y

    tmpdf = data.frame( ID       = rep(sub_idx, length(st_grp)),
                        TIME_HR  =  st_grp,
                        C_ng_ml  = C_ng_ml_grp,
                        DOSENUM = floor(st_grp/24/28 +1),
                        DOSE     = dose)

    tmpdf$ROUTE   = route
    if(is.null(simsparse_sd)){
      simsparse_sd = tmpdf
    } else {
      simsparse_sd = rbind(simsparse_sd, tmpdf)
    }

    sparse_idx  = sparse_idx  + 1
    # if we get past 3 we go back to 1
    if(sparse_idx > 3){
      sparse_idx  = 1
    }
    # -------------------------------------------------------------------------

    # incrementing the subject id
    sub_idx = sub_idx + 1
  }

}



#Adding the nominal time to the multiple dose dataset
simfull_md$NTIME_HR = simfull_md$TIME_HR - (simfull_md$DOSENUM-1)*28*24

#Adding the extrapolation column
simfull_md$EXTRAP = -1

# For even numbed subjects we set the number of extrapolation points to 3
simfull_md$EXTRAP[(simfull_md$ID %% 2) ==0]=3
# For odd we set it to 4
simfull_md$EXTRAP[(simfull_md$ID %% 2) !=0]=4


write.csv(simfull_sd,   file="pk_all_sd.csv",    quote=FALSE, row.names=FALSE)
write.csv(simfull_md,   file="pk_all_md.csv",    quote=FALSE, row.names=FALSE)
write.csv(simsparse_sd, file="pk_sparse_sd.csv", quote=FALSE, row.names=FALSE)

# All of the data
# calculating the average
simfull_sd$DOSE = as.factor(simfull_sd$DOSE)
simfull_sd$POOL = interaction(simfull_sd$TIME_HR, simfull_sd$DOSE)
simfull_sd$C_ng_ml_ave = -1

simfull_md$DOSE = as.factor(simfull_md$DOSE)
simfull_md$POOL = interaction(simfull_md$TIME_HR, simfull_md$DOSE)
simfull_md$C_ng_ml_ave = -1

for(POOL in simfull_sd$POOL){
   simfull_sd[simfull_sd$POOL == POOL, ]$C_ng_ml_ave = mean(simfull_sd[simfull_sd$POOL == POOL, ]$C_ng_ml)
}

for(POOL in simfull_md$POOL){
   simfull_md[simfull_md$POOL == POOL, ]$C_ng_ml_ave = mean(simfull_md[simfull_md$POOL == POOL, ]$C_ng_ml)
}

# Single dose 
p = ggplot() + 
    geom_point(data=simfull_sd, aes(x=TIME_HR, y=C_ng_ml, color=DOSE)) + 
     geom_line(data=simfull_sd, aes(x=TIME_HR, y=C_ng_ml_ave, color=DOSE)) +
     facet_wrap(.~ID)

p = gg_log10_yaxis(fo=p)
p = prepare_figure(fo=p, purpose="print")
p = p + xlab("Time (hours)") + ylab("Serum (ng/ml)")

ggsave(file.path("output","pk_full_sd.png"), plot=p, units="in", dpi=300, height=10, width=12 )

# Multiple dose 
p = ggplot() + 
    geom_point(data=simfull_md, aes(x=TIME_HR, y=C_ng_ml, color=DOSE)) + 
     geom_line(data=simfull_md, aes(x=TIME_HR, y=C_ng_ml_ave, color=DOSE)) +
     facet_wrap(.~ID)

p = gg_log10_yaxis(fo=p)
p = prepare_figure(fo=p, purpose="print")
p = p + xlab("Time (hours)") + ylab("Serum (ng/ml)")

ggsave(file.path("output","pk_full_md.png"), plot=p, units="in", dpi=300, height=10, width=12 )

# Sparse data
simsparse_sd$DOSE = as.factor(simsparse_sd$DOSE)
simsparse_sd$POOL = interaction(simsparse_sd$TIME_HR, simsparse_sd$DOSE)
simsparse_sd$C_ng_ml_ave = -1

for(POOL in simsparse_sd$POOL){
   simsparse_sd[simsparse_sd$POOL == POOL, ]$C_ng_ml_ave = mean(simsparse_sd[simsparse_sd$POOL == POOL, ]$C_ng_ml)
}

p = ggplot() + 
    geom_point(data=simsparse_sd, aes(x=TIME_HR, y=C_ng_ml, color=DOSE)) + 
     geom_line(data=simsparse_sd, aes(x=TIME_HR, y=C_ng_ml_ave, color=DOSE))  +
     facet_wrap(.~ID)

p = gg_log10_yaxis(fo=p)
p = prepare_figure(fo=p, purpose="print")
p = p + xlab("Time (hours)") + ylab("Serum (ng/ml)")

ggsave(file.path("output","pk_sparse_sd.png"), plot=p, units="in", dpi=300, height=10, width=12 )

