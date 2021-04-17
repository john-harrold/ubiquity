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

library(ggplot2)
library(officer)
# If we cannot load the ubiquity package we try the stand alone distribution
if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path("library", "r_general", "ubiquity.R")) }

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system.txt", 
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# Loading the system information
cfg = system_fetch_cfg()

#
# Starting a report
#
cfg = system_report_init(cfg)

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
                                  title        = "Title",
                                  sub_title    = "Sub Title",
                                  content_type = "text", 
                                  content      = "Body Text")
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

cfg = system_report_slide_section(cfg,
       title     = "Information in Two Column",      
       sub_title = "Text, pictures and bullets")
 
#
# Two columns of content
#


cfg = system_report_slide_two_col(cfg,
        title                  = "Two columns of plain text",      
        sub_title              = "Subtitle", 
        content_type           = "text", 
        left_content           = "Left Side",
        right_content          = "Right Side")

cfg = system_report_slide_two_col(cfg,
       title                  = "Two columns of lists",      
       sub_title              = "Subtitle", 
       content_type           = "list", 
       left_content           = lcontent,
       right_content          = lcontent)
  
cfg = system_report_slide_two_col(cfg,
       title                  = "Two Col Text w/Headers",      
       sub_title              = "Two columns of text with headers",
       content_type           = "text", 
       right_content          = "Right Text",
       left_content_type      = "text",
       left_content           = "Left Text",
       left_content_header    = "Left Header",
       right_content_header   = "Right Header")


cfg = system_report_slide_two_col(cfg,
       title                  = "Two Col Text w/Headers",      
       sub_title              = "Two columns of text with headers",
       content_type           = "list", 
       right_content          = c(1, "Right Text"),
       left_content_type      = "list",
       left_content           = c(1, "Left Text"),
       left_content_header    = "Left Header",
       right_content_type     = "list",
       right_content_header   = "Right Header")
  
cfg = system_report_slide_two_col(cfg,
       title                  = "Two columns: image and table",      
       sub_title              = "Subtitle", 
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



# Example usingflex tables
tcf = list()
tcf$table = data.frame(Parameters = c("Vp", "Cl", "Q", "Vt"),
                       Values     = 1:4,
                       Units      = c("L", "L/hr", "L/hr", "L") )
tcf$header_top   = 
     list(Parameters     = "Name", 
          Values         = "Value",
          Units          = "Units")

tcf$cwidth        = 0.8 
tcf$table_autofit = TRUE
tcf$table_theme   ='theme_zebra'

cfg = system_report_slide_two_col(cfg,
       title                       = "Two columns: flextable and image with header",      
       sub_title                   = "With a header",
       content_type                = "list",    
       right_content_type          = "flextable",
       right_content               = tcf,
       left_content_type           = "ggplot",
       left_content                = p,
       left_content_header_type    = "text",  
       left_content_header         = "Header text",  
       right_content_header_type   = "ggplot",
       right_content_header        =  p)

# Example using flextables explicitly 
library(magrittr)
library(flextable)

data = data.frame(property = c("mean",   "variance"),
                  Cmax     = c(2,         0.1),
                  AUCinf   = c(22,       0.21),
                  AUClast  = c(22,       0.21))

# This creates a flextable object:
ft = flextable::flextable(data)                      

cfg = system_report_slide_content(cfg,
       title        = "Userdefined Flextable",
       sub_title    = "flextable_object",    
       content_type = "flextable_object", 
       content      = ft)

# Pulling the report
rpt = system_fetch_report(cfg)

# you can make changes to rpt directly using the 
# functions from officer here

# then you can reassociate it with the named report:
 cfg = system_report_set(cfg, 
     rpt     = rpt)  

# Lastly you can save reports to files:
system_report_save(cfg, output_file = "example.pptx")
