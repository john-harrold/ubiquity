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
# -------------------------------------------------------------------------
# Here were creating some content for the reporting examples below:
# This creates a ggplot object (p) and an image file (imgfile)
library(ggplot2)
p = ggplot() + annotate("text", x=0, y=0, label = "picture example")
imgfile = tempfile(pattern="image", fileext=".png")
ggsave(filename=imgfile, plot=p, height=5.15, width=9, units="in")


gpc = list(image   = p,
           caption = "This is an example of an image from a ggplot object.")

ifc  = list(image   = imgfile,
            caption = "This is an example of an image from a file.")

# A dataframe with some tabular data:
tdata =  data.frame(Parameters = c("Vp", "Cl", "Q", "Vt"),
                    Values     = 1:4,
                    Units      = c("L", "L/hr", "L/hr", "L") )

# This example can be used for a simple Office table:
tco  = list(table     = tdata,    # This element contains the table data
            header    = TRUE,     # These two lines control the header
            first_row = FALSE,
            caption   = "This creates a table using an Office theme/format.")

# This is an example of adding a flextable
tcf = list(table       = tdata,             # This element contains the table data
           caption_format = "md",
           caption     = "This creates a <ff:courier>flextable</ff> using the <ff:courier>onbrand</ff> abstraction",
           header_top  = list(              # Defining the table heaaders
             Parameters = "Name",  
             Values     = "Value",
             Units      = "Units"),
           cwidth         = 0.8,            # Column width
           table_autofit  = TRUE,           # Making the tables automatically fit
           table_theme    = "theme_zebra",  # Making the tables automatically fit
           first_row = FALSE)

# This is a user-created flextable object
tfo = flextable::flextable(tdata)
tfo = flextable::autofit(tfo)

tcfo = list(ft = tfo,
            caption  = "This inserts a flextable object created by the user")

plain_text_content = paste(rep("The quick brown fox jumped over the lazy dog.", 70), collapse= " ")
md_text_content    = paste(rep("The *quick* <color:brown>brown</color> fox **jumped** over the ~lazy dog~.", 70), collapse=" ")
fpar_text_content  = officer::fpar(
   officer::ftext("The quick ", prop=NULL),
   officer::ftext("brown", prop=officer::fp_text(color="brown")),
   officer::ftext(" fox jumped over the lazy dog.", prop=NULL))
# -------------------------------------------------------------------------
# This loads the default Word template for ubiquity. See the Reporting
# vignette for details on how to create an obrand template for your
# organziation
cfg = system_rpt_read_template(cfg, "Word")
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
cfg = system_rpt_add_doc_content(cfg,
  type     = "ph",
  content  = list(name     = "FOOTERLEFT",
                  value    = "Text Swapped with Footer Placeholder",
                  location = "footer"))

cfg = system_rpt_add_doc_content(cfg,
  type     = "ph",
  content  = list(name     = "HEADERLEFT",
                  value    = "Left Side",
                  location = "header"))

cfg = system_rpt_add_doc_content(cfg,
  type     = "ph",
  content  = list(name     = "HEADERRIGHT",
                  value    = "Right Side",
                  location = "header"))
# -------------------------------------------------------------------------
# Adding text content. 
cfg = system_rpt_add_doc_content(cfg, 
        type="text",
        content = list(
          style = "Heading_1",
          text  = "Formatting Text"))

cfg = system_rpt_add_doc_content(cfg, 
        type="text",
        content = list(
          text  = "Text can be added in three diffent formats: plain text, using the fpar command from officer, and as markdown."))

cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_2",
    text  = "Adding Plain Text"))

cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    text  = plain_text_content))

cfg = system_rpt_add_doc_content(cfg, 
        type="text",
        content = list(
          style = "Heading_2",
          text  = "Using Markdown"))

cfg = system_rpt_add_doc_content(cfg, 
        type="text",
        content = list(
          style  = "Normal",    
          format = "md",
          text   = md_text_content))

cfg = system_rpt_add_doc_content(cfg, 
        type="text",
        content = list(
          style  = "Heading_2",
          format = "md",
          text   = "Using <ff:courier>fpar</ff>"))

cfg = system_rpt_add_doc_content(cfg, 
        type="text",
        content = list(
          style  = "Normal",    
          format = "fpar",
          text   = fpar_text_content))
# ---------
cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_1",
    text  = "Adding Figures"))

cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_2",
    text  = "Figures from image files"))

cfg = system_rpt_add_doc_content(cfg,
  type     = "imagefile",
  content  = ifc)

cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_2",
    text  = "Figures from ggplot objects"))
cfg = system_rpt_add_doc_content(cfg,
  type     = "ggplot",
  content  = gpc)
# ---------
cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_1",
    text  = "Adding Tables"))

cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_2",
    text  = "Office tables"))
cfg = system_rpt_add_doc_content(cfg, 
  type     = "table",
  content  = tco)
cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_2",
    text  = "onbrand flextable abstraction"))
cfg = system_rpt_add_doc_content(cfg, 
  type     = "flextable",
  content  = tcf)
cfg = system_rpt_add_doc_content(cfg, 
  type="text",
  content = list(
    style = "Heading_2",
    text  = "User-defined flextable objects"))
cfg = system_rpt_add_doc_content(cfg, 
  type     = "flextable_object",
  content  = tcfo)
# ---------
# -------------------------------------------------------------------------
# This will pull out template mapping details
tmplt_deets = system_rpt_template_details(cfg)
# -------------------------------------------------------------------------
# Saving the report
system_rpt_save_report(cfg, output_file = "example.docx")
