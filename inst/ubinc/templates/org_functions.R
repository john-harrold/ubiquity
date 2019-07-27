
# Use this function to define the meta information for an organizations 
# PowerPoint template file. First create a PowerPoint template as described in
# the Reporting vignette. If your organizational template is stored in
# myOrg.pptx you can do the following:
#
# First you will need a system file. This will create a minimal system file in
# the current working directory:
#
# system_new(system_file="empty")
#
# Next initialize a report using your organizational template:
#
# cfg = system_report_init(cfg, tempalte="myOrg.pptx")
#
# Now generate an annotated layout of your template:
#
# system_report_view_layout(cfg, output_file="layout.pptx")
#
# Open the layout.pptx file in PowerPoint and fill in the information below
# for each of the slide masters:

org_pptx_meta = function(){
#--------------------------------------------------------------------
# default reporting options
# 
# Set sub_title fields to NULL if they do not exist in the template
#

meta = list()
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

return(meta)}


