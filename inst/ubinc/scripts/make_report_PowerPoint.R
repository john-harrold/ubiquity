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
# If we cannot load the ubiquity package we try the stand alone distribution
if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path("library", "r_general", "ubiquity.R")) }

# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system.txt", 
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# -------------------------------------------------------------------------
# Here were creating some content for the reporting examples below:
# This creates a ggplot object (p) and an image file (imgfile)
library(ggplot2)
p = ggplot() + annotate("text", x=0, y=0, label = "picture example")
imgfile = tempfile(pattern="image", fileext=".png")
ggsave(filename=imgfile, plot=p, height=5.15, width=9, units="in")

# A dataframe with some tabular data:
tdata =  data.frame(Parameters = c("Vp", "Cl", "Q", "Vt"),
                    Values     = 1:4,
                    Units      = c("L", "L/hr", "L/hr", "L") )

# This example can be used for a simple Office table:
tco  = list(table     = tdata,    # This element contains the table data
            header    = TRUE,     # These two lines control the header
            first_row = FALSE)

# This is an example of adding a flextable
tcf = list(table       = tdata,             # This element contains the table data
           header_top  = list(              # Defining the table heaaders
             Parameters = "Name",  
             Values     = "Value",
             Units      = "Units"),
           cwidth         = 0.8,            # Column width
           table_autofit  = TRUE,           # Making the tables automatically fit
           table_theme    = "theme_zebra",  # Selecting the table theme
           first_row = FALSE)

# This is a user-created flextable object
tfo = flextable::flextable(tdata)
tfo = flextable::autofit(tfo)

# To display bulleted text make a vector with 
# pairs of indent level and text:
lcontent = c(1, "First major item",
             2, "first sub bullet",
             2, "second sub bullet",
             3, "sub sub bullet",
             1, "Second major item",
             2, "first sub bullet",
             2, "second sub bullet")
# -------------------------------------------------------------------------
# This loads the default PowerPoint template for ubiquity. See the Reporting
# vignette for details on how to create an obrand template for your
# organziation
cfg = system_rpt_read_template(cfg, "PowerPoint")
# -------------------------------------------------------------------------
# To access the underlying officer object you can do the following:
# First extract the officerobject:
rpt = system_fetch_rpt_officer_object(cfg)

# Now you can use any officer commands you'd want on the rpt object

# Don't forget to stick it bac into the ubiquity object once you're done:
cfg  = system_set_rpt_officer_object(cfg, rpt)

# -------------------------------------------------------------------------
# To access the underlying onbrand object you can do the following:
# First extract the onbrand object:
obnd = system_fetch_rpt_onbrand_object(cfg)

# Now you can use any onbrand functions on the obnd object

# Don't forget to stick it back into the ubiquity object once you're done:
cfg  = system_set_rpt_onbrand_object(cfg, obnd)
# -------------------------------------------------------------------------
# Adding content to the report
cfg = system_rpt_add_slide(cfg, 
   template = "title_slide",
   elements = list(
      title=list(content = "Reporting in ubiquity",
                 type    = "text")))
cfg = system_rpt_add_slide(cfg, 
   template = "section_slide",
   elements = list(
      title=list(content = "Content Types",
                 type    = "text")))
# ---------
# Figures from ggplot objects
cfg = system_rpt_add_slide(cfg, 
   template = "content_text",
   elements = list(
      title=
        list(content = "Figures: ggplot object",
             type    = "text"),
      sub_title=
        list(content = "Using ggplot objects directly",
             type    = "text"),
      content_body=
        list(content = p,
             type    = "ggplot")))
# Figures from image files
cfg = system_rpt_add_slide(cfg, 
   template = "content_text",
   elements = list(
      title=
        list(content = "Figures: image file",
             type    = "text"),
      sub_title=
        list(content = "Inserting figures from files",
             type    = "text"),
      content_body=
        list(content = imgfile,
             type    = "imagefile")))
# ---------
# Lists
cfg = system_rpt_add_slide(cfg, 
   template = "content_list",
   elements = list(
      title=
        list(content = "Lists",
             type    = "text"),
      sub_title=
        list(content = "For placholders that contain lists.",
             type    = "text"),
      content_body=
        list(content = lcontent,
             type    = "list")))

# ---------
# Office tables 
cfg = system_rpt_add_slide(cfg, 
   template = "content_text",
   elements = list(
      title=
        list(content = "Tables: Office",
             type    = "text"),
      sub_title=
        list(content = "Table in native Office format",
             type    = "text"),
      content_body=
        list(content = tco,
             type    = "table")))

# Flextables usin the onbrand abstraction
cfg = system_rpt_add_slide(cfg, 
   template = "content_text",
   elements = list(
      title=
        list(content = "Tables: flextable",
             type    = "text"),
      sub_title=
        list(content = "Flextables using onbrand abstraction",
             type    = "text"),
      content_body=
        list(content = tcf,
             type    = "flextable")))

# Flextables usin the onbrand abstraction
cfg = system_rpt_add_slide(cfg, 
   template = "content_text",
   elements = list(
      title=
        list(content = "Tables: flextable object",
             type    = "text"),
      sub_title=
        list(content = "Flextables using a user-created flextable object",
             type    = "text"),
      content_body=
        list(content = tfo,
             type    = "flextable_object")))

# ---------
# Other available templates: 
cfg = system_rpt_add_slide(cfg, 
   template = "section_slide",
   elements = list(
      title=list(content = "Other Available Default Templates",
                 type    = "text")))
# ---------
cfg = system_rpt_add_slide(cfg, 
   template = "two_content_header_text",
   elements = list(
      title=
        list(content = "Two Columns (Text) w/Headers",
             type    = "text"),
      sub_title=
        list(content = "Two columns of text content with headers",
             type    = "text"),
      content_left_header=
        list(content = "Left Header",
             type    = "text"),
      content_left=
          list(content = "Left Text",
             type    = "text"),
      content_right_header=
        list(content = "Right Header",
             type    = "text"),
      content_right=
        list(content = "Right Text",
             type    = "text")))
# ---------
cfg = system_rpt_add_slide(cfg, 
   template = "two_content_text",
   elements = list(
      title=
        list(content = "Two Columns (Text)",
             type    = "text"),
      sub_title=
        list(content = "Two columns of text content",
             type    = "text"),
      content_left=
          list(content = "Left Text",
             type    = "text"),
      content_right=
        list(content = "Right Text",
             type    = "text")))
# ---------
cfg = system_rpt_add_slide(cfg, 
   template = "two_content_header_list",
   elements = list(
      title=
        list(content = "Two Columns (List) w/Headers",
             type    = "text"),
      sub_title=
        list(content = "Two columns of lists with headers",
             type    = "text"),
      content_left_header=
        list(content = "Left Header",
             type    = "text"),
      content_left=
          list(content = c(1, "Left Text"),
             type    = "list"),
      content_right_header=
        list(content = "Right Header",
             type    = "text"),
      content_right=
        list(content = c(1, "Right Text"),
             type    = "list")))
# ---------
cfg = system_rpt_add_slide(cfg, 
   template = "two_content_list",
   elements = list(
      title=
        list(content = "Two Columns (List)",
             type    = "text"),
      sub_title=
        list(content = "Two columns of lists",
             type    = "text"),
      content_left=
          list(content = c(1, "Left Text"),
             type    = "list"),
      content_right=
        list(content = c(1, "Right Text"),
             type    = "list")))

# This will pull out template mapping details
tmplt_deets = system_rpt_template_details(cfg)
# -------------------------------------------------------------------------
# Saving the report
system_rpt_save_report(cfg, output_file = "example.pptx")
