
# Use this file to define functions for the meta information for an organizations 
# PowerPoint or Word template file. 




org_pptx_meta = function(){
#--------------------------------------------------------------------
# First create a PowerPoint template as described in the Reporting vignette.
# If your organizational template is stored in myOrg.pptx you can do the
# following:
#
# First you will need a system file. This will create a minimal system file in
# the current working directory:
#
# system_new(system_file="empty")
#
# Next initialize a report using your organizational template:
#
# cfg = system_report_init(cfg, template="myOrg.pptx")
#
# Now generate an annotated layout of your template:
#
# system_report_view_layout(cfg, output_file="layout.pptx")
#
# Open the layout.pptx file in PowerPoint and fill in the information below
# for each of the slide masters.
#
# Once that is done you can use your organization template in the following
# way:
#   - source your myOrg.R file
#   - initialize the report with the correct template and meta functions
# 
# source(myOrg.R)
# cfg = system_report_init(cfg, 
#         meta     = org_pptx_meta(),
#         template = "myOrg.pptx")
# 
#--------------------------------------------------------------------
# default reporting options
# 
# Set sub_title fields to NULL if they do not exist in the template
#

meta = list(title          = list(),
            section        = list(),
            content        = list(),
            two_col        = list(),
            md_def         = list())

# this is the information the title slide
meta$title$layout$general                      = "title_slide"
meta$title$master$general                      = "Office Theme"             
meta$title$type$title                          = 'ctrTitle'
meta$title$type$sub_title                      = 'subTitle'
meta$title$indices$title                       = 3
meta$title$indices$sub_title                   = 4
meta$title$ph_labels$title                     = "Title 1"
meta$title$ph_labels$sub_title                 = "Subtitle 2" 

# this is the information the section title slide                         
meta$section$layout$general                    = "section_slide"
meta$section$master$general                    = "Office Theme"             
meta$section$type$title                        = 'ctrTitle'
meta$section$type$sub_title                    = 'subTitle'
meta$section$indices$title                     = 3
meta$section$indices$sub_title                 = 4
meta$section$ph_labels$title                   = "Title 1"
meta$section$ph_labels$sub_title               = "Subtitle 2"


# These contain the mapping information for content in the template
# The main dimensions are:
# units = inches, height = 5.0, width = 9.5                    
# Text content                                                   
meta$content$layout$general                    = "content_text"   
meta$content$master$general                    = "Office Theme"             
meta$content$indices$content_body              = 3 
meta$content$indices$content_sub_title         = 2 
meta$content$ph_labels$content_body            = "Content Placeholder 2"
meta$content$ph_labels$content_sub_title       = "Content Placeholder 10" 
                                                                
# List content                                                  
meta$content$layout$list                       = "content_list"
meta$content$master$list                       = "Office Theme"             
meta$content$indices$list_body                 = 2
meta$content$indices$list_sub_title            = 3 
meta$content$ph_labels$list_body               = "Content Placeholder 2"
meta$content$ph_labels$list_sub_title          = "Content Placeholder 10"


# Two column slide options (with headers)                       
# Each of the larger placeholders have dimensions of:           
# units = inches, height = 4.41, width = 4.65                   
# Two column list with headers                                  
meta$two_col$layout$list_head                  = "two_content_header_list"
meta$two_col$master$list_head                  = "Office Theme"             
meta$two_col$indices$list_head_sub_title       = 1
meta$two_col$indices$list_head_left_title      = 6 
meta$two_col$indices$list_head_left            = 5 
meta$two_col$indices$list_head_right_title     = 4 
meta$two_col$indices$list_head_right           = 3 
meta$two_col$ph_labels$list_head_sub_title     = "Content Placeholder 10"
meta$two_col$ph_labels$list_head_left_title    = "Text Placeholder 2" 
meta$two_col$ph_labels$list_head_left          = "Content Placeholder 2" 
meta$two_col$ph_labels$list_head_right_title   = "Text Placeholder 4"
meta$two_col$ph_labels$list_head_right         = "Content Placeholder 3"

# Two column text with headers
meta$two_col$layout$text_head                  = "two_content_header_text"
meta$two_col$master$text_head                  = "Office Theme"             
meta$two_col$indices$text_head_sub_title       = 6
meta$two_col$indices$text_head_left_title      = 1 
meta$two_col$indices$text_head_left            = 3 
meta$two_col$indices$text_head_right_title     = 2 
meta$two_col$indices$text_head_right           = 4 
meta$two_col$ph_labels$text_head_sub_title     = "Content Placeholder 10"
meta$two_col$ph_labels$text_head_left_title    = "Text Placeholder 2" 
meta$two_col$ph_labels$text_head_left          = "Content Placeholder 2" 
meta$two_col$ph_labels$text_head_right_title   = "Text Placeholder 4" 
meta$two_col$ph_labels$text_head_right         = "Content Placeholder 3" 



# Each place holder has dimensions of:
# units = inches, height = 5.08, width = 4.65
# Two column lists (no headers)
meta$two_col$layout$list                      = "two_content_list"
meta$two_col$master$list                      = "Office Theme"             
meta$two_col$indices$list_sub_title           = 2 
meta$two_col$indices$list_left                = 3 
meta$two_col$indices$list_right               = 4 
meta$two_col$ph_labels$list_sub_title         = "Content Placeholder 10" 
meta$two_col$ph_labels$list_left              = "Content Placeholder 2" 
meta$two_col$ph_labels$list_right             = "Content Placeholder 3" 
                                            
# Two column text (no headers)              
meta$two_col$layout$text                      = "two_content_text"
meta$two_col$master$text                      = "Office Theme"             
meta$two_col$indices$text_sub_title           = 4 
meta$two_col$indices$text_left                = 3 
meta$two_col$indices$text_right               = 1 
meta$two_col$ph_labels$text_sub_title         = "Content Placeholder 10" 
meta$two_col$ph_labels$text_left              = "Content Placeholder 2" 
meta$two_col$ph_labels$text_right             = "Content Placeholder 3" 


# When using markdown text you have to specify the default properties of text
# for the Table 
#
# Table_Labels refers to the footers and headers while Table is the defaults
# for the body of the table
meta[["md_def"]][["Table_Labels"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = TRUE, 
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Helvetica",
        vertical.align = "baseline",
        shading.color  = "transparent")


meta[["md_def"]][["Table"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = FALSE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Helvetica",
        vertical.align = "baseline",
        shading.color  = "transparent")


return(meta)}


org_docx_meta = function(){
#--------------------------------------------------------------------
# First create a Word template as described in the Reporting vignette. If your
# organizational template is stored in myOrg.docx you can do the following:
#
# First you will need a system file. This will create a minimal system file in
# the current working directory:
#
# system_new(system_file="empty")
#
# Next initialize a report using your organizational template:
#
# cfg = system_report_init(cfg, template="myOrg.docx")
#
# Now generate an annotated layout of your template:
#
# system_report_view_layout(cfg, output_file="layout.docx")
#
# Open the layout.docx file in Word and fill in the information below
# for each of the placeholders, styles, captions and markdown definitions
#
# Once that is done you can use your organization template in the following
# way:
#   - source your myOrg.R file
#   - initialize the report with the correct template and meta functions
# 
# source(myOrg.R)
# cfg = system_report_init(cfg, 
#         meta     = org_docx_meta(),
#         template = "myOrg.docx")

meta = list(ph_content = list(),
            styles     = list(),
            captions   = list(),
            md_def     = list())

# If your document contains placeholders you can put default values for those
# placeholders here. 
#
# The ph_content field should be the placeholder without the surrounding
# offset characters. So for example if you had ===HEADERLEFT=== in your
# doucment template you would use the following to replace that with empty
# content by default:
#
# For each placeholder there should be a location and a content element. The
# location can be either "header", "footer" or "body", and the content will be
# the default value. 
meta[["ph_content"]][["HEADERLEFT"]][["location"]]   = "header"
meta[["ph_content"]][["HEADERLEFT"]][["content"]]    = ""

# You'll need to create a template with the following styles defined.
meta[["styles"]][["Normal"]]                 = "Normal"
meta[["styles"]][["Code"]]                   = "Code"
meta[["styles"]][["TOC"]]                    = "TOC 1" 
meta[["styles"]][["Heading_1"]]              = "heading 1"
meta[["styles"]][["Heading_2"]]              = "heading 2"
meta[["styles"]][["Heading_3"]]              = "heading 3"
meta[["styles"]][["Table"]]                  = "Table Grid"
meta[["styles"]][["Table_Caption"]]          = "table title"
meta[["styles"]][["Figure_Caption"]]         = "graphic title" 

# Locations can be either top or bottom
meta[["styles"]][["Table_Caption_Location"]]    = "top" 
meta[["styles"]][["Figure_Caption_Location"]]   = "bottom" 

# This sets the default figure width and height in inches for the document:
meta[["styles"]][["Figure_Width"]]           = 6.0
meta[["styles"]][["Figure_Height"]]          = 5.0


# The pre_number and post_number values here wrap round the figure and table
# number. For figure N the label before the caption will be:
# "Figure N: " 
# Followed by the caption.
meta[["captions"]][["figure"]][["pre_number"]]          = "Figure "
meta[["captions"]][["figure"]][["post_number"]]         = ": "
meta[["captions"]][["table"]][["pre_number"]]           = "Table " 
meta[["captions"]][["table"]][["post_number"]]          = ": "



# When using markdown text you have to specify the default properties of text
# for each of the styles listed above:

meta[["md_def"]][["Normal"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = FALSE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Cambria (Body)",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Code"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = FALSE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Courier",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["TOC"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = FALSE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Cambria (Body)",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Heading_1"]] = list(
        color          = "black",
        font.size      = 16,
        bold           = TRUE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Calibri (Headings)",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Heading_2"]] = list(
        color          = "black",
        font.size      = 13,
        bold           = TRUE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Calibri (Headings)",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Heading_3"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = TRUE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Calibri (Headings)",
        vertical.align = "baseline",
        shading.color  = "transparent")

# Table_Labels refers to the footers and headers while Table is the defaults
# for the body of the table
meta[["md_def"]][["Table_Labels"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = TRUE, 
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Helvetica",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Table"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = FALSE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Helvetica",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Table_Caption"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = TRUE,
        italic         = TRUE,
        underlined     = FALSE,
        font.family    = "Cambria (Body)",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Figure"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = FALSE,
        italic         = FALSE,
        underlined     = FALSE,
        font.family    = "Cambria (Body)",
        vertical.align = "baseline",
        shading.color  = "transparent")

meta[["md_def"]][["Figure_Caption"]] = list(
        color          = "black",
        font.size      = 12,
        bold           = TRUE,
        italic         = TRUE,
        underlined     = FALSE,
        font.family    = "Cambria (Body)",
        vertical.align = "baseline",
        shading.color  = "transparent")


return(meta)}
