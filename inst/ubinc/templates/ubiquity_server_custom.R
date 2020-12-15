rm(list=ls())


require(shiny)
require(shinydashboard)
require(deSolve)
require(ggplot2)
require(foreach)
require(doParallel)
require(rhandsontable)

#
# If we're operating out of a "stand alone" directory we load the files from
# there. Otherwise we try to load the ubiquity package
#

if("ubiquity" %in% (.packages())){
  ubiquity_distribution = "package"
} else if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
  source(file.path('library', 'r_general', 'ubiquity.R'))
  ubiquity_distribution = "stand alone"
} else { 
  library(ubiquity) 
  ubiquity_distribution = "package"
}

source(file.path("transient", "app_base", "auto_rcomponents.R"))


#-----------------------------------------
user_log_update <- function(input, output, session) {
  cfg=gui_fetch_cfg(session)
  fileReaderData <- reactiveFileReader(500, session, cfg$gui$user_log_file, readLines)
  output$text_user_log <- renderText({
    # Read the text, and make it a consistent number of lines so
    # that the output box doesn't grow in height.
    text <- fileReaderData()
    length(text) <- cfg$gui$user_log_length
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })
}

#-----------------------------------------
# Puling up the app state
#
gui_fetch_cfg  <-function(session){
  cfg = NULL
  user_dir = find_user_dir(session)
  load(file=file.path(user_dir, "gui_state.RData"))
  return(cfg)}

#-----------------------------------------
# This function finds a place to store the user information for the current
# session. If the user variable is set, we use that to store the user session,
# otherwise we use the session token. If for some reason those fail we store
# them in 'default'
#
# If full is FALSE then we just get the name of the user specific directory.
# If full is TRUE we return the full path to that directory.
#
# To do this we need to figure out if the app is deployed. This is triggered
# by the looking at the file "DEPLOYING" in transient/app_base. If that file
# has the word TRUE, then we store everything in the session temporary
# directory. If the app isn't deployed then we store everything in the working
# directory to allow for debugging later.
find_user_dir <- function(session, full=TRUE) {

  # Trying to determine where to store the user information
  # First we see if the session$user exists, if not we look for the
  # session token, then we look for 
  if(!is.null(session$user)){
    user = session$user 
  } else if (!is.null(session$token)) {
    user = session$token 
  } else {
    user = 'default'  
  }

  if(full){
    # First we set deploying to false
    DEPLOYING = FALSE
    # Next we try to open the file that contains the deploying switch. It's
    # created at the end of the ubiquity_app.R file:
    DEPINFO_file = file.path(getwd(), "transient", "app_base" ,"DEPLOYING")
    if(file.exists(DEPINFO_file)){
      # This file should contain a single line with either TRUE if the app is
      # being deployed and false if it's not
      DEPLOYINGConn<-file(file.path(getwd(), "transient", "app_base" ,"DEPLOYING"))
      if(readLines(DEPINFO_file)[1] == "TRUE"){
        DEPLOYING = TRUE
      }
      close(DEPLOYINGConn)
    } 
    
    if(DEPLOYING){
        # If we're deploying we store the user files in the temporary
        # directory:
        user = file.path(tempdir(),"transient", "app_base", "rgui", user) 
      } else {
        # otherwise we store them locally 
        user = file.path(getwd(),"transient", "app_base", "rgui", user) 
      }
  }

  return(user)
}
#-----------------------------------------
initialize_session <- function(session) {

  # pulling the path to the user directory 
  user_dir = find_user_dir(session)

  # if the user directory exists we delete it
  if(dir.exists(user_dir)){
    unlink(user_dir, recursive=TRUE)}
  
  # now we create the user directory and copy the default 
  # cfg variable into it
  dir.create(user_dir, recursive=TRUE)

  file.copy(file.path(getwd(), "transient", "app_base", "rgui", "gui_state.RData"), file.path(user_dir, "gui_state.RData"))

  # loading the cfg variable
  cfg=gui_fetch_cfg(session)

  # setting the working directory
  cfg = system_set_option(cfg,
                          group  = "general"   ,
                          option = "output_directory",
                          value  =  file.path(user_dir, "output"))
  
  # initializing the general ubiquity log
  cfg = system_set_option(cfg, 
                        group="logging", 
                        option="file",
                        file.path(user_dir, "ubiquity_log.txt"))
  cfg = system_log_init(cfg)
  GUI_log_entry(cfg, "server.R startup")
  
  # initialzing the user log
  cfg$gui$user_log_file =  file.path(user_dir, "user_log.txt")
  file.create(cfg$gui$user_log_file)

  # Default to integrating with r scripts
  cfg$options$simulation_options$integrate_with  = "r-file"
  # If the dynamic library exists we try to load it
  if(file.exists(file.path("transient", paste("r_ode_model", .Platform$dynlib.ext, sep = "")))){
    GUI_log_entry(cfg, "Found dynamic library attempting to load")
    dyn.load(file.path("transient", paste("r_ode_model", .Platform$dynlib.ext, sep = ""))) }

  # If the library has been loaded we switch to C
  if(is.null(getLoadedDLLs()$r_ode_model) == FALSE){
    if(getLoadedDLLs()$r_ode_model[["dynamicLookup"]] == TRUE){
      GUI_log_entry(cfg, "Dynamic library seems to be loaded, setting")
      GUI_log_entry(cfg, "integration method to c-file")
      cfg$options$simulation_options$integrate_with  = "c-file"
    }
  }

  user_log_entry(cfg, "App Initialized")
  
  # saving the cfg variable
  gui_save_cfg(cfg, session)

}
#-----------------------------------------
gui_save_cfg = function(cfg, session){
  user_dir = find_user_dir(session)
  save(cfg, file=sprintf('%s%sgui_state.RData', user_dir, .Platform$file.sep))
}
#-----------------------------------------

#-----------------------------------------
ubiquity_app_state <- function(input, output, session){
  user_dir = find_user_dir(session)
  # Reading int the GUI state
  cfg=gui_fetch_cfg(session)
  sim_status = cfg$gui$sim_status;
}

#-----------------------------------------
user_log_entry = function(cfg, text, initialize=FALSE){
  logfile = cfg$gui$user_log_file

  # First we add it to the GUI log entry
  GUI_log_entry(cfg, text)

    # now we read in the contents of the gui log file
    current_file = file(logfile, open="r")
    lines = readLines(current_file)
    close(current_file)
    
    # appending the text to the log file
    lines = c(lines, text)
    
    # only pulling the last n lines
    lines = tail(lines, cfg$gui$user_log_length)
    
    # writing those back to the log file
    # Now we dump it to the log file:
    write(lines, file=logfile, append=FALSE)

}

#-----------------------------------------
server <- function(input, output, session) {

    # Initializing the the session for 
    # the current user
      initialize_session(session) 
      user_log_update(input, output, session)

      cfg=gui_fetch_cfg(session)

}
