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

# If you want more complicated tables you can create a flextable and use that:
# Example using flextables explicitly 
library(magrittr)
library(flextable)

data = data.frame(property = c("mean",   "variance"),
                  Cmax     = c(2,         0.1),
                  AUCinf   = c(22,       0.21),
                  AUClast  = c(22,       0.21))

header = list(property = c("",              ""),
              Cmax     = c("Cmax",          "ng/ml"),
              AUCinf   = c("AUCinf",        "mL/hr"),
              AUClast  = c("AUClast",       "mL/hr"))

# This creates a flextable object:
ft = flextable::flextable(data)                     %>% 
     flextable::delete_part(part = "header")        %>%
     flextable::add_header(values =as.list(header)) %>%
     flextable::theme_zebra()

tcfo = list(caption = "This is a flextable object",
            key     = "TAB_FTO",
            ft      = ft)

cfg = system_report_doc_add_content(cfg, 
  content_type  = "flextable_object",
  content       = tcfo)
#--------------------
cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h1",
                       format  = "md",
                       text    = "Formatting Text <color:red>Colors</color> and other ^formats^"))

fpartext = fpar(
ftext("Formatted text can be created using the ", prop=NULL),
ftext("fpar ", prop=officer::fp_text(color="green")),
ftext("command from the officer package.", prop=NULL))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       format  = "fpar",
                       text    = fpartext))


mdtext = "Text can be specified in markdown format as well. You just need to
use the format <ff:courier>'md'</ff> in the content options. You can specify
*bold text*, **italicized text**, ^superscripts^ and ~subscripts~. These can
also be combined *H*~*2*~*0*.

You can change colors to  <color:red>red</color>, <color:blue>blue</color>,
etc and change the <shade:#33ff33>shading</shade>. These can be used with
_===BODYPHEXAMPLE===_ and can be combined such as changing
<shade:orange><color:green>both color and shading</color></shade>. You can
also change the font to things like <ff:symbol>*symbol*</ff>."

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h2",
                       format  = "text",
                       text    = "Text Containing Markdown"))

cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style  = "normal",
                       format = "text",
                       text   = mdtext))


cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h2",
                       format  = "text",
                       text    = "Text With Markdown Rendered"))

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
      ph_content  = "placeholder text in the body" ,
      ph_name     = "BODYPHEXAMPLE", 
      ph_location = "body")


cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "h2",
                       text    = "Formatting Table Contents"))


cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       text    = 'You can put Markdown into the header definitions and have them interpreted when you create a flextable, all you need to do is set the header_format to "md"'))

tcf = list()

tcf$table  = data.frame(property = c("mean",   "variance"),
                  Cmax     = c(2,         0.1),
                  AUCinf   = c(22,       0.21),
                  AUClast  = c(22,       0.21))

tcf$header_top = list(property = c(""       ), 
                      Cmax     = c("C~max~"   ), 
                      AUCinf   = c("AUC~inf~" ), 
                      AUClast  = c("AUC~last~")) 

tcf$header_middle = list(property = c(""),
                         Cmax     = c("ng/ml"),
                         AUCinf   = c("mL\U00B7hr^-1^"),
                         AUClast  = c("mL\U00B7hr^-1^"))

tcf$header_format = "md"

tcf$caption = "This is a flextable with markdown formatting"

tcf$cwidth        = 0.8 
tcf$table_autofit = TRUE
tcf$table_theme   ='theme_zebra'


cfg = system_report_doc_add_content(cfg, 
  content_type  = "flextable",
  content       = tcf)


cfg = system_report_doc_add_content(cfg, 
  content_type  = "text",
  content       = list(style   = "normal",
                       text    = 'You can use the function md_to_oo within flextable calls to turn Markdown into the formatted objects that flextable expects. This can be used to modify both headers and text within the table. Note that you can use unicode for symbols like the dot multiplication is unicode format U+00B7 which can be witten as a backslash and removing the + to produce "\U00B7" '))

data = data.frame(property = c("mean",   "variance"),
                  Cmax     = c(2,         0.1),
                  AUCinf   = c(22,       0.21),
                  AUClast  = c(22,       0.21))

header = list(property = c("",              ""),
              Cmax     = c("Cmax",          "ng/ml"),
              AUCinf   = c("AUCinf",        "mL/hr"),
              AUClast  = c("AUClast",       "mL/hr"))


# To get the formatting correct we need to pull out the default format for
# tables (dft):
dft      = system_fetch_report_format(cfg, element="Table_Labels")$default_format
dft_body = system_fetch_report_format(cfg, element="Table")$default_format


# This creates a flextable object:
ftf = flextable::flextable(data)                                                                                  %>% 
      flextable::delete_part(part = "header")                                                                     %>%
      flextable::add_header(values =as.list(header))                                                              %>%
      flextable::compose(j    = "Cmax",                                                    
                        part  = "header",                                                          
                        value = c(md_to_oo("*C*~*max*~", dft)$oo, md_to_oo("*ng/ml*", dft)$oo))                   %>%
      flextable::compose(j    = "AUClast",                                                    
                        part  = "header",                                                          
                        value = c(md_to_oo("*AUC*~*last*~", dft)$oo, md_to_oo("*ml\U00B7hr*^*-1*^", dft)$oo))     %>%
      flextable::compose(j    = "AUCinf",                                                    
                        part  = "header",                                                          
                        value = c(md_to_oo("*AUC*~*inf*~", dft)$oo, md_to_oo("*ml\U00B7hr*^*-1*^", dft)$oo))      %>%
      flextable::compose(j     = "property",                                                         
                         i     = match("mean", data$property),                        
                         part  = "body",                                                          
                         value = c(md_to_oo("Mean (<ff:symbol>m</ff>)", dft_body)$oo))                            %>%
      flextable::compose(j     = "property",                                                                      
                         i     = match("variance", data$property),                                                
                         part  = "body",                                                                          
                         value = c(md_to_oo("Variance (<ff:symbol>s</ff>^2^)", dft_body)$oo))                     %>%
      flextable::autofit()                                                                                        %>%
      flextable::theme_zebra()

tcfo = list(caption = "This is a flextable object with Markdown formatting",
            key     = "TAB_FTO_FORMATTED",
            ft      = ftf)
cfg = system_report_doc_add_content(cfg, 
  content_type  = "flextable_object",
  content       = tcfo)










# To be added later:
#  cfg = system_report_doc_add_content(cfg, 
#    content_type  = "text",
#    content       = list(style   = "h1",
#                         text    = "Referencing Tables and Figures"))
#  
#  
#  refing_text = "When you create a table or figure the content list has a key
#  option. You can then use that key to reference that object in the text using
#  <REF:KEY_VALUE> to insert that object number. For example: Above Table 
#  <REF:TAB_FTO> shows how to insert a flextable object and Table 
#  <REF:TAB_FTO_FORMATTED> shows how to format flextable objects using markdown
#  notation. This is another reference to Table <REF:TAB_FTO>."
#  
#  cfg = system_report_doc_add_content(cfg, 
#    content_type  = "text",
#    content       = list(style  = "normal",
#                         format = "text",
#                         text   = refing_text))

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
