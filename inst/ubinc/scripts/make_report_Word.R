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
# If we cannot load the ubiquity package we try the stand alone distribution
#if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
#{source(file.path("library", "r_general", "ubiquity.R")) }

source(file.path("library", "r_general", "ubiquity.R")) 
# Rebuilding the system (R scripts and compiling C code)
cfg = build_system(system_file="system.txt")

# Loading the system information
cfg = system_fetch_cfg()



#
# Starting a report
#
cfg = system_report_init(cfg, rpttype="Word")


#
# Dumping the layout of the  current report
#
system_report_view_layout(cfg, output_file="layout.docx")

#--------------------
# Generating a table of contents
cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h1",
                       text    = "Table of Contents"))
cfg = system_report_doc_add_content(cfg, 
  content_type  = "toc",
  content       = list(level=1))
#--------------------
# Figures
cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h1",
                       text    = "Adding figures to a word document"))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       text    = "There are two ways to add figures to word document the first uses the output of ggplot directly and the second reads figures from image files."))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h2",
                       text    = "Subsection"))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h3",
                       text    = "Subsubsection"))

# Creating an image to use in the examples below
p = ggplot() + annotate("text", x=0, y=0, label = "picture example")
imagefile = tempfile(pattern="image", fileext=".png")
ggsave(filename=imagefile, plot=p, height=5.15, width=9, units="in")

# Using ggplot objects
cfg = system_report_doc_add_content(cfg, 
  content_type  = "ggplot",
  content       = list(image   = p,
                       caption = "This is a ggplot image"))

# Using image file
cfg = system_report_doc_add_content(cfg, 
  content_type  = "imagefile",
  content       = list(image   = imagefile,
                       caption = "This is an image file"))



#--------------------
# Tables
#
cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h1",
                       text    = "Adding tables to a word document"))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       text    = "There are two ways to add tables to word document the first uses the native tables in word and the second uses the flextable package."))
# Table example using standard word tables
tc = list()
tc$table = data.frame(Parameters = c("Vp", "Cl", "Q", "Vt"),
                      Values     = 1:4,
                      Units      = c("L", "L/hr", "L/hr", "L") )
tc$header    = TRUE 
tc$first_row = TRUE 
tc$caption = "This is a table"

cfg = system_report_doc_add_content(cfg, 
  content_type  = "table",
  content       = tc)


# Table example using flextable
tcf = list()
tcf$caption = "This is a flextable"
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

cfg = system_report_doc_add_content(cfg, 
  content_type  = "flextable",
  content       = tcf)
#--------------------
cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h1",
                       text    = "Formatting Text"))

fpartext = fpar(
ftext("Formatted text can be created using the ", prop=NULL),
ftext("fpar ", prop=officer::fp_text(color="green")),
ftext("command from the officer package.", prop=NULL))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       format  = "fpar",
                       text    = fpartext))



mdtext = "Text can be specified in markdown format as well. You can specify
*bold text*, **italicized text**, ^superscripts^ and ~subscripts~. These can
be combined as well *H*~*2*~*0*.

You can change colors to  <color:red>red</color>, <color:blue>blue</color>, etc and
change the <shade:#33ff33>shading</shade>. Again _:::BODYPHEXAMPLE:::_ these can be combined
<shade:orange><color:green>both color and shading</color></shade>. You can also
change the font to things like <ff:symbol>*symbol*</ff>."

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style  = "normal",
                       format = "md",
                       text   = mdtext))

tmp = md_to_officer(mdtext)

#--------------------
# Placeholder content
cfg = system_report_doc_set_ph(cfg, 
      ph_content  = "Header Placeholder" ,
      ph_name     = "HeaderLeft", 
      ph_location = "header")

cfg = system_report_doc_set_ph(cfg, 
      ph_content  = "Placeholder text in Body" ,
      ph_name     = "BODYPHEXAMPLE", 
      ph_location = "body")

#--------------------
# Document formatting
#--------------------

# This sets the previous section to continuous 
cfg = system_report_doc_format_section(cfg, section_type="continuous")

# Next we add some content, say an image
p = ggplot() + annotate("text", x=0, y=0, label = "picture example")

cfg = system_report_doc_add_content(cfg, 
  content_type  = "ggplot",
  content       = list(image   = p,
                       height  = 2.5,
                       width   = 9,
                       caption = "This is a landscape figure"))
# Then we end this section in landscape mode 
cfg = system_report_doc_format_section(cfg, section_type="landscape", h=8, w=10)

# next we can add some text
cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       text    = paste(rep("some two column text", 300), collapse=" ")))

# Then we end this section in landscape mode 
cfg = system_report_doc_format_section(cfg, section_type="columns", widths=c(3,3))
                       
# Flextable formatting:
# https://stackoverflow.com/questions/52264208/how-to-insert-greek-letter-delta-∆-into-header-of-flextable-object
# https://davidgohel.github.io/flextable/articles/display.html


# system_report_doc_set_ph

# # Pulling the report
# rpt = system_report_fetch(cfg)
# 
# # you can make changes to rpt directly using the 
# # functions from officer here
# 
# # then you can reassociate it with the named report:
#  cfg = system_report_set(cfg, 
#      rpt     = rpt)  


# Lastly you can save reports to files:
system_report_save(cfg, output_file = "example.docx")
