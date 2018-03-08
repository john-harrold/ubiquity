#clearing the workspace
rm(list=ls())
# Turning on more verbose error reporting
options(error=traceback)
options(show.error.locations = TRUE)
# Uncomment to set the script directory as the working directory
# This works when calling this file as a script:
# R -e "source('thisfile.r')"
# setwd(dirname(sys.frame(tail(grep('source',sys.calls()),n=1))$ofile))
graphics.off()

# if we are in a stand alone distribution we run from there
# otherwise we try to load the package
if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
  source(file.path('library', 'r_general', 'ubiquity.R'))
} else { 
  library(ubiquity) }

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system.txt")

# Loading the system information
cfg = system_fetch_cfg()
# set name                  | Description
# -------------------------------------------------------
# default                   | TMDD: Membrane bound target

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

# To overwrite the default dosing uncomment the following
# Setting all dosing to zero
# cfg = system_zero_inputs(cfg) 
# cfg = system_set_bolus(cfg, state   ="Cp", 
#                             times   = c(  0  ),  #  weeks
#                             values  = c( 30  ))  #  mpk




# -------------------------------------------------------------------------
# Individual Simulation:
som = run_simulation_ubiquity(parameters, cfg)
# # replace TS     with a timescale (i.e. days) and 
# #         OUTPUT with a named output  (i.e. Cp)
#plot(som$simout$TS,        som$simout$OUTPUT)
# p = ggplot() + 
#         geom_line(data=som$simout, aes(x=ts.TS,   y=OUTPUT), color="red") 
# p = gg_log10_yaxis(p)
# print(p)
# png(file.path('output', 'simulation.png'), width=20,  height=14, units="cm", res=300)
# print(p)
# dev.off()
# ggsave(plot=p, units='cm', width=20, height=14, filename=file.path('output', 'simulation.png'))
# -------------------------------------------------------------------------





                               


                               
                               
                               


#
# Starting a report called test
#
cfg = system_report_init(cfg, 
                         template = file.path("library", "templates", "report.pptx"))
#
#  use system_report_view_layout to get the 
#
system_report_view_layout(cfg, 
                          output_file   = "layout.pptx")
#
# use system_report_slide_content to create slides with title and a single
# large body of information:
#

system_report_slide_title(cfg,
                          title                  = "Presentation Title",      
                          sub_title              = "Sub title")


# For displaying only text use the following:
cfg = system_report_slide_content(cfg,
                            content_type = "text", 
                            content      = "Text")
#
# To display bulleted text make a vector with 
# pairs of indent level and text:
#
lcontent = c(1, "First major item",
             2, "first sub bullet",
             2, "second sub bullet",
             3, "sub sub bullet",
             1, "Second major item",
             2, "first sub bullet",
             2, "second sub bullet")

cfg = system_report_slide_content(cfg,
                                  title        = "Title",
                                  sub_title    = "Sub Title",
                                  content_type = "list", 
                                  content      = lcontent)


tcontent = list()
tcontent$table = data.frame(Parameters = c("Vp", "Cl", "Q", "Vt"),
                            Values     = 1:4,
                            Units      = c("L", "L/hr", "L/hr", "L") )

# disabling the header here:
tcontent$header    = FALSE
tcontent$first_row = FALSE
cfg = system_report_slide_content(cfg,
                                  title        = "Title Example",
                                  sub_title    = "Sub Title",
                                  content_type = "table", 
                                  content      = tcontent)
#
# To add an image file the dimensions should be 
#
# units = inches, height = 5.09, width = 9.45
p = ggplot() + annotate("text", x=0, y=0, label = "picture example")
imgfile = tempfile(pattern="image", fileext=".png")
ggsave(filename=imgfile, plot=p, height=5.15, width=9, units="in")

cfg = system_report_slide_content(cfg,
                                  title        = "Image example",
                                  sub_title    = "Image file",
                                  content_type = "imagefile", 
                                  content      = imgfile)
# 
# Or you can add a ggplot image
# 
cfg = system_report_slide_content(cfg,
       title        = "Image example",
       sub_title    = "ggplot",    
       content_type = "ggplot", 
       content      = p)

cfg = system_report_slide_title(cfg,
       title     = "Information in Two Column",      
       sub_title = "Text, pictures and bullets")

#
# Two columns of content
#

cfg = system_report_slide_two_col(cfg,
        title                  = "Two columns of plain text",      
        sub_title              = NULL, 
        content_type           = "text", 
        left_content           = "Left Side",
        right_content          = "Right Side")

cfg = system_report_slide_two_col(cfg,
       title                  = "Two columns of lists",      
       sub_title              = NULL, 
       content_type           = "list", 
       left_content           = lcontent,
       right_content          = lcontent)


cfg = system_report_slide_two_col(cfg,
       title                  = "Two columns: image and table",      
       sub_title              = NULL, 
       content_type           = "text", 
       left_content_type      = "ggplot",
       left_content           = p,
       right_content_type     = "table", 
       right_content          = tcontent)


cfg = system_report_slide_two_col(cfg,
       title                  = "Two columns: plain text and image with header",      
       sub_title              = "With a header",
       content_type           = "text", 
       right_content          = "Right Side",
       left_content_type      = "ggplot",
       left_content           = p,
       left_content_header    =  "Header text",  
       right_content_header   =  NULL)



# Example using flex tables
tcf = list()
tcf$table = data.frame(Parameters = c("Vp", "Cl", "Q", "Vt"),
                       Values     = 1:4,
                       Units      = c("L", "L/hr", "L/hr", "L") )
# tcf$header_top   = 
#      list(Parameters     = "Name", 
#           Values         = "Value",
#           Units          = "Units")
# 
tcf$cwidth        = 0.8 
tcf$table_autofit = TRUE
tcf$table_theme   ='theme_zebra'

cfg = system_report_slide_two_col(cfg,
       title                       = "Two columns: flex table and image with header",      
       sub_title                   = "With a header",
       content_type                = "list",    
       right_content_type          = "flextable",
       right_content               = tcf,
       left_content_type           = "ggplot",
       left_content                = p,
       left_content_header         =  "Header text",  
       right_content_header_type   =  "ggplot",
       right_content_header        =  p)


# Pulling the report
rpt = system_report_fetch(cfg)

# you can make changes to rpt directly using the 
# functions from officer here

# then you can reassociate it with the named report:
 cfg = system_report_set(cfg, 
     rpt     = rpt)  

# Lastly you can save reports to files:
system_report_save(cfg, output_file = "example.pptx")
