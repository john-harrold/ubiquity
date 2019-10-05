rm(list=ls())
library(shiny)

if(file.exists("REBUILD")){
   source("ubiquity_app.R")
   file.remove("REBUILD")
}

#---------------------------------------------------------------------------
# Loading the system information
load(file=file.path(getwd(), "transient", "app_base", "rgui","gui_state.RData"))
#---------------------------------------------------------------------------


OUTPUTS = c()
# Creating selections for model outputs:
# by stripping out all of the QC outputs
# unless admin mode has been set.
for(OUTPUT  in names(cfg$options$mi$outputs)) {
  if((attr(regexpr('^QC_', OUTPUT), 'match.length') < 0) | 
      cfg$gui$admin_mode == TRUE){
    OUTPUTS[length(OUTPUTS)+1] =  OUTPUT }
 }


# Initializing the UI elements to 'empty'
uiele = list()
uiele$selectpoint = list()
uiele$autoxaxis   = list()
uiele$autoyaxis   = list()
uiele$checklog    = list()
uiele$checkgrid   = list()
uiele$outptus     = list()


# Adding those elements based on overwrites
if(cfg$gui$display$selectpoint){
  uiele$selectpoint = list( div(style="display:inline-block; max-width: 180px", "Point:"),
                            div(style="display:inline-block; max-width: 180px", textOutput("text_selectpoint")))
}

if(cfg$gui$display$autoxaxis){
  uiele$autoxaxis   = list( div(style="display:inline-block", checkboxInput("check_autox", "Auto X-axis",           value=cfg$gui$check_autox)), 
                            div(style="display:inline-block; max-width: 120px", textInput("text_autox", label=NULL, value="0,1")))
}

if(cfg$gui$display$autoyaxis){
  uiele$autoyaxis   = list( div(style="display:inline-block", checkboxInput("check_autoy", "Auto Y-axis",           value=cfg$gui$check_autoy)), 
                            div(style="display:inline-block; max-width: 120px", textInput("text_autoy", label=NULL, value="0,1")))
}


if(cfg$gui$display$checklog){
  uiele$checklog    = list( div(checkboxInput("check_log", "Log Scale ", value=cfg$gui$check_log)))
}

if(cfg$gui$display$checkgrid){
  uiele$checkgrid    = list( div(checkboxInput("check_grid", "Show Grid", value=cfg$gui$check_grid)))
}

if(cfg$gui$display$outputs){
  uiele$outputs = div(style="display:inline-block",  selectInput("select_outputs", multiple=TRUE, selected=cfg$gui$outputs, "", OUTPUTS))
}

ui = fluidPage(
       fluidRow(
         column(3,  
           uiOutput("my_parameter_tabs"),
           uiOutput("my_input_tabs")
         ),
         column(9
            ,fluidRow( 
               column(12,  
               uiOutput("my_plot_tabs")
               ) 
             )
            ,fluidRow( 
               column( 8,  
               actionButton("button_update", "Update Plot"), 
               uiele$outputs, 
               textOutput('text_status', inline = TRUE)
               ), 
               column( 1, '' 
               ),
               column( 3, align="center", br(), uiele$selectpoint)
             )
           ,fluidRow(
              column(2, uiele$autoxaxis 
              ),
              column(2, uiele$autoyaxis
              )
            , column(8, 
                uiele$checkgrid, 
                uiele$checklog   
              )
           )
          ,fluidRow(
             column(12, 
               downloadButton("button_download", label = "Save"),
               div(style="display:inline-block; max-width:200px", textInput('text_save_sim', label=NULL, value=cfg$gui$text_save_sim)),
               div(style="display:inline-block", checkboxInput('check_timestamp', 'Timestamp', value=cfg$gui$check_timestamp))
             )
           )
          ,fluidRow(
             column(12, verbatimTextOutput("text_user_log")  
             )
           )
         )
       )
     )


