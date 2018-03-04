
# Use this function to define the meta information for an organizations 
# powerpoint template file
org_pptx_meta = function(){
#--------------------------------------------------------------------
# default reporting options
# 
# Set sub_title fields to NULL if they do not exist in the template
#
# this is the information the title slide
meta$title$layout$general                   = "Title Slide"
meta$title$master$general                   = "Office Theme"             
meta$title$type$title                       = 'ctrTitle'
meta$title$type$sub_title                   = 'subTitle'
meta$title$indices$title                    = NULL
meta$title$indices$sub_title                = NULL

# this is the information the title slide
meta$section$layout$general                 = "Section Header"
meta$section$master$general                 = "Office Theme"             
meta$section$type$title                     = 'ctrTitle'
meta$section$type$sub_title                 = 'subTitle'
meta$section$indices$title                  = NULL
meta$section$indices$sub_title              = NULL

# these contain the mapping information for content in the template
# units = inches, height = 5.09, width = 9.45
meta$content$indices$list_body              = 2
meta$content$indices$content_body           = 1 
meta$content$indices$list_sub_title         = 4 
meta$content$indices$content_sub_title      = 4 
meta$content$layout$list                    = "Title and Content List"
meta$content$master$list                    = "Office Theme"             
meta$content$layout$general                 = "Title and Content Text"
meta$content$master$general                 = "Office Theme"             

# Two column slide options
# Each place holder has dimensions of:
# units = inches, height = 5.08, width = 4.65
# No headers with text
meta$two_col$indices$text_sub_title         = 4 
meta$two_col$indices$text_left              = 5 
meta$two_col$indices$text_right             = 3 
meta$two_col$layout$text                    = "Two Content Text"
meta$two_col$master$text                    = "Office Theme"             

# No headers with lists
meta$two_col$indices$list_sub_title         = 1 
meta$two_col$indices$list_left              = 5 
meta$two_col$indices$list_right             = 4 
meta$two_col$layout$list                    = "Two Content List"
meta$two_col$master$list                    = "Office Theme"             

# Headers with text
# Each place holder has dimensions of:
# units = inches, height = 4.41, width = 4.65
meta$two_col$indices$text_head_sub_title    = 3
meta$two_col$indices$text_head_left_title   = 4 
meta$two_col$indices$text_head_left         = 6 
meta$two_col$indices$text_head_right_title  = 7 
meta$two_col$indices$text_head_right        = 2 
meta$two_col$layout$text_head               = "Two Content Header Text"
meta$two_col$master$text_head               = "Office Theme"             

# Headers with text
meta$two_col$indices$list_head_sub_title    = 6
meta$two_col$indices$list_head_left_title   = 2 
meta$two_col$indices$list_head_left         = 3 
meta$two_col$indices$list_head_right_title  = 5 
meta$two_col$indices$list_head_right        = 1 
meta$two_col$layout$list_head               = "Two Content Header List"
meta$two_col$master$list_head               = "Office Theme"             

return(meta)}


