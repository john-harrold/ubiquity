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

# For documentation explaining how to modify the commands below
# See the "R Workflow" section at the link below:
# http://presentation.ubiquity.grok.tv

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="<SYSTEM_FILE>")

<PSETS>
cfg = system_select_set(cfg, "default")

# fetching the parameter values
parameters = system_fetch_parameters(cfg)

# The previous statement sets 'parameters' to the values 
# in the currently selected parameter set. To overwrite 
# a specific parameter uncomment the following statement 
# and replace PNAME with the name of the parameter 
# and VALUE with the desired value:
#
# parameters$PNAME = VALUE;

<OUTPUT_TIMES>

# The following applies to both individual and stochastic simulations:
# Define the solver to use
cfg=system_set_option(cfg,group = "simulation", option = "solver", value = "lsoda")
#
# To overwrite solver options use the following:
# cfg=system_set_option(cfg,group  = "solver",
#                           option = "atol",   
#                           value  = 1e-10)
# cfg=system_set_option(cfg,group  = "solver",
#                           option = "rtol",   
#                           value  = 1e-10)

# Specify the output times 
cfg=system_set_option(cfg, group  = "simulation", 
                           option = "output_times", 
                           seq(0,100,1))
# By default, important times will be included in the simulation output
# e.g., bolus times, sampling before and after rate switches, etc.
# uncomment to only evalutate at the output times specified above
# cfg=system_set_option(cfg, group  = "simulation", 
#                            option = "include_important_output_times", 
#                            value  = "no")
# Uncomment to specify ode file to use
# cfg=system_set_option(cfg, group  = "simulation", 
#                            option = "integrate_with",
#                            value  = "r-file")

<BOLUS>
<INFUSION_RATES>
<COVARIATES>

# -------------------------------------------------------------------------
# Individual Simulation:
som = run_simulation_ubiquity(parameters, cfg)
# # replace TS     with a timescale (i.e. days) and 
# #         OUTPUT with a named output  (i.e. Cp)
#plot(som$simout$TS,        som$simout$OUTPUT)
# p = ggplot() + 
#         geom_line(data=som$simout, aes(x=ts.TS,   y=OUTPUT), color="red") 
# 
# # This adds a log10 scale to the y axis
# p = gg_log10_yaxis(p)
# 
# # This pretties up the default figure layout for ggplot
# p = prepare_figure('present', p)              
#
# # Dump the figure to a file:
# png(file.path('output', 'simulation.png'), width=20,  height=14, units="cm", res=300)
# print(p)
# dev.off()
# 
# ggsave(plot=p, units='cm', width=20, height=14, filename=file.path('output', 'simulation.png'))
# 
# 
# # To combine figures you can use gridExtra
# library("grid", "gridExtra")
# 
# pdf("output/myfigure.pdf", height=7.0, width=13.5)
# grid.arrange(p_PCT, p_NUM, ncol=2,
#              bottom = textGrob("X",        gp=gpar(fontsize=15)), 
#              right  = textGrob("Y-Right",  gp=gpar(fontsize=15), rot=90), 
#              left   = textGrob("Y-Left",   gp=gpar(fontsize=15), rot=90))
# 
# dev.off()
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# # Stochastic Simulation:
# # To use this you need to have specified variability 
# # in your model using the <IIV:?>, <IIV:?:?>, and <IIVCOR:?:?> 
# # delimiters see examples/system-iiv.txt
#
# cfg = system_set_option(cfg, group="stochastic", option="nsub",    value=100)
# cfg = system_set_option(cfg, group="stochastic", option="ci",      value=95 )
# cfg = system_set_option(cfg, group="stochastic", option="seed",    value=8675309)
# cfg = system_set_option(cfg, group="stochastic", option="ponly",   value=FALSE)
# cfg = system_set_option(cfg, group="stochastic", option="states",  value=list())
# cfg = system_set_option(cfg, group="stochastic", option="outputs", value=c('OP1', 'OP2'))
#
# # To pull subject parameters and covariates from a file first
# # Load the file then tell simulate_subjects to use that file
# cfg = system_load_data(cfg, dsname     = "SUBS", 
#                             data_file  = "subjects.csv")
# 
# cfg=system_set_option(cfg, group  = "stochastic",
#                            option = "sub_file",
#                            value  = "SUBS")
# 
# # Control how subjects are sampled from the file here:
# cfg=system_set_option(cfg, group  = "stochastic",
#                            option = "sub_file_sample",
#                            value  = "with replacement")
#
# # To parallelize the simulations uncomment the following:
#  cfg=system_set_option(cfg, group  = "simulation",
#                             option = "parallel",    
#                             value  = "multicore")
#  
#  cfg=system_set_option(cfg, group  = "simulation",
#                             option = "compute_cores", 
#                             value  = detectCores() - 1)
#  
# som   = simulate_subjects(parameters, cfg)
# 
# #
# # replace TS     with a timescale (i.e. days) and 
# #         OUTPUT with a named output  (i.e. Cp)
#
# p = ggplot() 
# p = p + geom_ribbon(data=som$tcsummary, aes(x=ts.TS, ymin=o.OUTPUT.lb_ci, ymax=o.OUTPUT.ub_ci), fill='cadetblue1', alpha=0.6)
# p = p + geom_line(  data=som$tcsummary, aes(x=ts.TS, y=o.OUTPUT.median, color='OUTPUT'), linetype='solid', size=0.9) 
# p = p + xlab('Time (TS)')                     
# p = p + ylab('Output')                        
# p = prepare_figure('present', p)              
# p = p + scale_colour_manual(values=c("OUTPUT"="darkblue"))   
# p = p + theme(legend.title = element_blank()) 
# p = p + theme(legend.position = 'bottom') 
#
# print(p)
# 
# 
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Reporting
# cfg = system_report_init(cfg)
# 
# system_report_slide_title(cfg,
#         title                  = "Presentation Title",      
#         sub_title              = "Sub title")
# 
# tcontent = list()
# tcontent$table = data.frame(col1 = c(1), col2 = c(2))
# 
# cfg = system_report_slide_content(cfg,
#         rptname      = "test",    
#         title        = "Title Example",
#         sub_title    = "Sub Title",
#         content_type = "table", 
#         content      = tcontent)
# -------------------------------------------------------------------------
