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
cfg = build_system(system_file="<SYSTEM_FILE>",
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

<PSETS>
cfg = system_select_set(cfg, "default")

# fetching the parameter values
parameters = system_fetch_parameters(cfg)

# The previous statement sets "parameters" to the values 
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
# uncomment to only evaluate at the output times specified above
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
# p = prepare_figure("present", p)              
#
# # Dump the figure to a file:
# png(file.path("output", "simulation.png"), width=20,  height=14, units="cm", res=300)
# print(p)
# dev.off()
# 
# ggsave(plot=p, units="cm", width=20, height=14, filename=file.path("output", "simulation.png"))
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
# cfg = system_set_option(cfg, group="stochastic", option="ssp",     value=list())
# cfg = system_set_option(cfg, group="stochastic", option="states",  value=list())
# cfg = system_set_option(cfg, group="stochastic", option="outputs", value=c("OP1", "OP2"))
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
#  library(doParallel)
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
# p = p + geom_ribbon(data=som$tcsummary, aes(x=ts.TS, ymin=o.OUTPUT.lb_ci, ymax=o.OUTPUT.ub_ci), fill="cadetblue1", alpha=0.6)
# p = p + geom_line(  data=som$tcsummary, aes(x=ts.TS, y=o.OUTPUT.median, color="OUTPUT"), linetype="solid", size=0.9) 
# p = p + xlab("Time (TS)")                     
# p = p + ylab("Output")                        
# p = prepare_figure("present", p)              
# # Plot customizaton
# p = p + scale_colour_manual(values=c("OUTPUT"="darkblue"))   # manually specifying colors
# p = p + guides(color=guide_legend(title="Color Title"))      # Alter legend title
# p = p + theme(legend.title = element_blank())                # Remove all legend titles
# p = p + guides(linetype = FALSE)                             # Remove just the linetype legend
# p = p + theme(legend.position="none")                        # Remove all legends
# p = p + theme(legend.position = "bottom")                    # Set the legend location
# p = p + theme(legend.position = c(0.8, 0.2))                 # Manually  specify legend position
#
# print(p)
# 
# 
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Reporting
# See Reporting vignette for more information
# # PowerPoint
# cfg = system_report_init(cfg, rpttype="PowerPoint")
# 
# cfg = system_report_slide_title(cfg,
#         title                  = "Presentation Title",      
#         sub_title              = "Sub title")
# 
# tcontent = list()
# tcontent$table = data.frame(col1 = c(1), col2 = c(2))
# 
# cfg = system_report_slide_content(cfg,
#         title        = "Title Example",
#         sub_title    = "Sub Title",
#         content_type = "table", 
#         content      = tcontent)
#
# system_report_save(cfg, output_file=file.path("output", "report.pptx"))
#
# # Word
# cfg = system_report_init(cfg, rpttype="Word")
#
# cfg = system_report_doc_add_content(cfg, 
#   content_type  = "text",
#   content       = list(style   = "normal",
#                        text    = "This is a Word report"))
# 
# system_report_save(cfg, output_file=file.path("output", "report.pptx"))
# -------------------------------------------------------------------------
