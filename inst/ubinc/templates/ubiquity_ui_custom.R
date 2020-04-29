rm(list=ls())

require(shiny)
require(shinydashboard)

if(file.exists("REBUILD")){
   source("ubiquity_app.R")
   file.remove("REBUILD")
}

#---------------------------------------------------------------------------
# Loading the system information
load(file=file.path(getwd(), "transient", "app_base", "rgui","gui_state.RData"))
#---------------------------------------------------------------------------


# https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title="App Name"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Introduction",      tabName="introduction", icon=icon("archive")), 
       menuItem("Item 1",            tabName="item_1",       icon=icon("dashboard")), 
       menuItem("Item 2",            tabName="item_2",       icon=icon("dashboard")), 
       menuItem("Diagnostics",       tabName="diagnostics",  icon=icon("info-circle"))
     )
  ),
  dashboardBody(
    tabItems(                                
     tabItem(tabName="introduction",      "Intro text"),
     tabItem(tabName="item_1",            "Item 1 content"),
     tabItem(tabName="item_2",            "Item 2 content"),
     tabItem(tabName="diagnostics",       "User Log", verbatimTextOutput("text_user_log")
      )
    )
  )
)
