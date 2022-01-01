rm(list=ls())


require(shiny)
require(deSolve)
require(ggplot2)
require(foreach)
require(doParallel)
require(rhandsontable)
require(grid)          
require(gridExtra)

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
str2list = function(cfg, str){
    val = c();

   tryCatch(
    { 
    val = eval(parse(text=sprintf('c(%s)',str )))
    },
     warning = function(w) {
     # place warning stuff here
    },
     error = function(e) {
       GUI_log_entry(cfg, sprintf('unable to parse string (%s)', str))
    })
  return(val)}
#-----------------------------------------
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
#-----------------------------------------
system_trim_som_initialization = function(cfg, som, variability=FALSE, sstime=0){


   if(cfg$gui$check_variability){

     # boolean vector of times to keep:
     t_to_keep = som$times$time >sstime

     # first we trim all the initialization values from
     # the state information
     for(sname  in names(som$states)){
       som$states[[sname]]=som$states[[sname]][,t_to_keep]
     }

     # Next we do the same for the outputs
     for(oname  in names(som$outputs)){
       som$outputs[[oname]]=som$outputs[[oname]][,t_to_keep]
     }

     # Lastly we trim the initialization values 
     # from the summary table
     som$tcsummary = som$tcsummary[t_to_keep, ]

     # then we do the time information as well
     som$time = som$time[t_to_keep, ]
   
   }
   else{
     # if an individual simulation has been run we 
     # chop off the values before sstime
     som$simout = som$simout[som$simout$time > sstime, ] }

   return(som)
}
#-----------------------------------------
#-----------------------------------------
select_point  = function(input, output, session){

  output$text_selectpoint = renderText({
    # loading the gui state
    cfg=gui_fetch_cfg(session)
    input$plotfigure_click
    xval = input$plotfigure_click$x
    yval = input$plotfigure_click$y

    # After the first click reactive elements get called twice for some
    # reason. So we store the old clicks in the gui state and see if they
    # change

    # By default we have a new click
    value = ""
    tryCatch(
     { 
       newclick = TRUE
       # First we check to see if there are previous x and y alues
       if(!is.null(cfg$gui$click_x) & !is.null(cfg$gui$click_y)){
         # if they exist we check to see if they are the same 
         # as the current ones
         if((cfg$gui$click_x == xval) & (cfg$gui$click_y == yval) ){
           newclick = FALSE 
         }
       }
       value = toString(c(var2string(xval, 1, nsig_e = 2, nsig_f = 2), var2string(yval,1, nsig_e = 2, nsig_f = 2)))
       if(newclick){
        user_log_entry(cfg, sprintf("Point: %s", value)) 
       }
     },
      warning = function(w) {
      # place warning stuff here
     },
      error = function(e) {
        GUI_log_entry(cfg, sprintf('plotfigure_click: %s ', e$message))
        GUI_log_entry(cfg, sprintf('This can happen while switching y-scale'))
     })

    
    if(value != ""){
     # updating the clicked values
     cfg$gui$click_x = xval
     cfg$gui$click_y = yval
     
     # saving the gui state
     gui_save_cfg(cfg, session)
     }
      
     value})
}


fetch_save_string = function(input, output, session){
   cfg=gui_fetch_cfg(session)
   ss = ""
   # creating simulation save (ss) string
   
   # First we start with the save simlation text.
   # If it's blank then we just use the generic my_simulation
   # if it's not we strip out any white space
   if(is.null(cfg$gui$text_save_sim)){
     ss = 'my_simulation' }
   else{
     ss = gsub("[[:space:]]", "_", cfg$gui$text_save_sim) }
   
   # next we look for the timestamp
   if(cfg$gui$check_timestamp){
     ss = sprintf("%s_%s", ss, format(Sys.time(), format= "%Y_%m_%d_%H_%M_%S")) }

  return(ss)}
#-----------------------------------------
#
# zipping up the important bits and pushing it to the user
#
download_simulation = function(input, output, session){
    output$button_download <- downloadHandler(
      filename = function() {
        #
        # The file name of the zip file to download
        #
        cfg=gui_fetch_cfg(session)
           
        # pulling out the save string
        ss         = fetch_save_string(input, output, session)
        zip_fname  = sprintf('%s.zip', ss)

        # returning the zip file name
        zip_fname },
      content = function(fname) {
        #
        # The contents of the zip file
        #
        cfg      = gui_fetch_cfg(session)
        user_dir = find_user_dir(session)
        save_dir = fetch_save_dir(session)
        # loading the data 
        som=gui_fetch_som(session)

        # pulling out the save string
        ss         = fetch_save_string(input, output, session)

        #
        # Generate the file names to store the files that will be created
        #
        # general r libraries
        if(cfg$options$misc$distribution == "stand alone"){
          fname_ub_r     = file.path(cfg$gui$wd, "library", "r_general", "ubiquity.R")
        } else {
          fname_ub_r     = system.file("scripts", "ubiquity_fcns.R", package="ubiquity")
        }

        # system specific functions
        fname_arc_r      = file.path(cfg$gui$wd, "transient", "app_base", "auto_rcomponents.R")
        # export template
        fname_save_r     = file.path(cfg$options$misc$templates, "r_gui_save.R")
        

        # Library file
        fname_newlib_r_full = sprintf('%s%sanalysis_%s_lib.r',                save_dir, .Platform$file.sep, ss)
        fname_newlib_r_base = sprintf('analysis_%s_lib.r', ss)
        
        # Script file
        fname_newrun_r_full = sprintf('%s%sanalysis_%s.r',                    save_dir, .Platform$file.sep, ss)
        fname_newrun_r_base = sprintf('analysis_%s.r', ss)
        
        # CSV file
        fname_newcsv_r_full = sprintf('%s%sanalysis_%s.csv',                  save_dir, .Platform$file.sep, ss)
        fname_newcsv_r_base = sprintf('analysis_%s.csv', ss)

        # Beginning of html report file names
        fname_report_r_full = sprintf('%s%sanalysis_%s_report',               save_dir, .Platform$file.sep, ss)
        fname_report_r_base = sprintf('analysis_%s_report', ss)

        # Figure files
        fname_timecourse_full = sprintf('%s%sanalysis_%s_timecourse.png',     save_dir, .Platform$file.sep, ss)
        fname_timecourse_base = sprintf('analysis_%s_timecourse.png',                                       ss)
        fname_paramdist_full = sprintf('%s%sanalysis_%s_paramdist.png',       save_dir, .Platform$file.sep, ss)
        fname_paramdist_base = sprintf('analysis_%s_paramdist.png',                                         ss)


        # JMH Dont need these?
        # fname_newzip_r_full = sprintf('%s%sanalysis_%s.zip',                  save_dir, .Platform$file.sep, ss)
        # fname_newzip_r_base = sprintf('analysis_%s.zip', ss)


        # making the csv save command based on the type of simulation
        if(cfg$gui$check_variability){
          save_csv = sprintf('write.csv(file = "%s", x=som$tcsummary)', fname_newcsv_r_full )}
        else{
          save_csv = sprintf('write.csv(file = "%s", x=som$simout)', fname_newcsv_r_full) }
        


        # contents of save files
        lines_lib = c()
        lines_run = c()

        # reading in the data from library and run template files
        # lib files
        cfn = file(fname_ub_r, open="r")
        lines_lib = c(lines_lib, readLines(cfn))
        close(cfn)
        
        cfn = file(fname_arc_r, open="r")
        lines_lib = c(lines_lib, readLines(cfn))
        close(cfn)
        
        # run file
        cfn = file(fname_save_r, open="r")
        lines_run = c(lines_run, readLines(cfn))
        close(cfn)

        # loading user defined functions
        if(!is.null(cfg$gui$functions$user_def)){
          cfg$gui$sysel_simulate$user_def = sprintf('source("%s")',cfg$gui$functions$user_def)
        } else{
          cfg$gui$sysel_simulate$user_def = ''
        }
        # updating certain system elements
        cfg$gui$sysel_simulate$libfile     = fname_newlib_r_base
        cfg$gui$sysel_simulate$system_file = cfg$options$misc$system_file

        # constructing the header labels
        if(cfg$gui$sysel_simulate$iiv == ""){
          cfg$gui$sysel_simulate$iheader = "" }
        else{
          cfg$gui$sysel_simulate$iheader = "#\n# Specifying the IIV elements\n#\n" }

        if(cfg$gui$sysel_simulate$bolus == ""){
          cfg$gui$sysel_simulate$bheader = "" }
        else{
          cfg$gui$sysel_simulate$bheader = "#\n# Specifying the bolus injections \n#\n" }

        if(cfg$gui$sysel_simulate$infusion_rates == ""){
          cfg$gui$sysel_simulate$rheader = "" }
        else{
          cfg$gui$sysel_simulate$rheader = "#\n# Specifying the infusion rates \n#\n" }

        if(cfg$gui$sysel_simulate$covariates     == ""){
          cfg$gui$sysel_simulate$cheader = "" }
        else{
          cfg$gui$sysel_simulate$cheader = "#\n# Specifying the covariates     \n#\n" }



        # Substituting in the simulation script
        # First each simulation element
        for(sysel in names(cfg$gui$sysel_simulate)){
           token_str = sprintf("<%s>", sysel)
           lines_run = gsub(token_str, cfg$gui$sysel_simulate[[sysel]], lines_run)
        }
    
        # defaulting the save variables to false
        # will be updated to true of the save commands
        # are successful
        save_timecourse = FALSE
        save_paramdist  = FALSE

        # Generating the timecourse figure as a png file
        # saving the figure to a file
         tryCatch(
          { 
           PTC = generate_timecourse(input, output, session)
           ggsave(fname_timecourse_full, PTC)
           save_timecourse = TRUE
          },
           warning = function(w) {
           # place warning stuff here
          },
           error = function(e) {
             user_log_entry(cfg, sprintf('unable to save timecourse figure (%s)', fname_timecourse_base))
          })

        # Generating the parameter distribution figure as a png file
        # saving the figure to a file
         if(cfg$gui$check_variability){
           tryCatch(
            { 
             PPD = generate_paramdist(input, output, session)
             ggsave(fname_paramdist_full, PPD)
             save_paramdist = TRUE
            },
             warning = function(w) {
             # place warning stuff here
            },
             error = function(e) {
               user_log_entry(cfg, sprintf('unable to save parameter distribution figure (%s)', fname_paramdist_base))
            })
         }

         sysel_plot_timecourse = "";
         sysel_plot_paramdist  = "";

         # loading the generated plotting code
         if(file.exists(sprintf("%s%sgui_plot_timecourse.RData", user_dir, .Platform$file.sep))){
           load(sprintf("%s%sgui_plot_timecourse.RData", user_dir, .Platform$file.sep))}
         else{
           GUI_log_entry(cfg, 'Unable to find file: gui_plot_timecourse.RData') }

         if(cfg$gui$check_variability){
           if(file.exists(sprintf("%s%sgui_plot_paramdist.RData", user_dir, .Platform$file.sep))){
             load(sprintf("%s%sgui_plot_paramdist.RData", user_dir, .Platform$file.sep))}
           else{
             GUI_log_entry(cfg, 'Unable to find file: gui_plot_paramdist.RData') }
         }
         

         # making the csv save command based on the type of simulation
         if(cfg$gui$check_variability){
           save_csv      = sprintf('write.csv(file = "%s", x=som$tcsummary)', fname_newcsv_r_base )
           save_csv_full = sprintf('write.csv(file = "%s", x=som$tcsummary)', fname_newcsv_r_full )}
         else{
           save_csv      = sprintf('write.csv(file = "%s", x=som$simout)', fname_newcsv_r_base) 
           save_csv_full = sprintf('write.csv(file = "%s", x=som$simout)', fname_newcsv_r_full) }


         # getting the plotting commands
         lines_run = gsub("<plot_timecourse>", sysel_plot_timecourse, lines_run)
         lines_run = gsub("<plot_paramdist>",  sysel_plot_paramdist , lines_run)
         # getting the data save command
         lines_run = gsub("<save_csv>",        save_csv,           lines_run)
         
         # writing the contents of lib, run and csv files
         write(lines_run, file=fname_newrun_r_full, append=FALSE)
         write(lines_lib, file=fname_newlib_r_full, append=FALSE)
         eval(parse(text=save_csv_full)) 


         # generating the reports

         # By default we dont have any reports 
         fname_report_r_full_R1 = NULL
         fname_report_r_full_R2 = NULL
         fname_report_r_full_R3 = NULL
         fname_report_r_full_R4 = NULL
         fname_report_r_full_R5 = NULL
         # JMH delete
         # save_simulation = list()
         if(!is.null(cfg$gui$modelreport_files)){
            # For each specified report we create a tab
            for(report_name in names(cfg$gui$modelreport_files)){
             mg      = generate_model_report(cfg, session, report_name)
             if(mg$success){
               # copying the report for to be included in export
               file.copy(mg$htmlfile, sprintf('%s_%s.html', fname_report_r_full, report_name))
               eval(parse(text=sprintf('fname_report_r_full_%s = "%s_%s.html"', report_name, fname_report_r_full, report_name)))
               eval(parse(text=sprintf('fname_report_r_base_%s = "%s_%s.html"', report_name, fname_report_r_base, report_name)))
             }
           }
         }
        
        # 
        # Now we build up the contents of the zip file
        # 


        # reading in the content of the zip file
        zip_contents = c(fname_newlib_r_full,
                         fname_newcsv_r_full,
                         fname_newrun_r_full)

        
        if(cfg$gui$save$user_log){
           zip_contents = c(zip_contents, cfg$options$logging$file)
        }

        if(cfg$gui$save$system_txt){
           zip_contents = c(zip_contents, file.path(cfg$gui$wd , cfg$options$misc$system_file))
        }

        # if the report file isn't null then it exists and we add it
        # to the zip file
        if(!is.null(fname_report_r_full_R1)){
         zip_contents = c(zip_contents, fname_report_r_full_R1) }
        if(!is.null(fname_report_r_full_R2)){
         zip_contents = c(zip_contents, fname_report_r_full_R2) }
        if(!is.null(fname_report_r_full_R3)){
         zip_contents = c(zip_contents, fname_report_r_full_R3) }
        if(!is.null(fname_report_r_full_R4)){
         zip_contents = c(zip_contents, fname_report_r_full_R4) }
        if(!is.null(fname_report_r_full_R5)){
         zip_contents = c(zip_contents, fname_report_r_full_R5) }

        # same for the figures, if they're not null we add them
        if(!is.null(save_timecourse)){
         zip_contents = c(zip_contents, fname_timecourse_full) 
        }

        if(!is.null(cfg$gui$save_paramdist)){
         zip_contents = c(zip_contents, fname_paramdist_full) 
        }

        # Adding user defined files
        if(!is.null(cfg$gui$functions$user_def)){
          zip_contents = c(zip_contents, cfg$gui$functions$user_def)}

        user_log_entry(cfg, "Simulation saved")
        # writing zip file

        zip(fname, zip_contents, flags = "-j",  zip = Sys.getenv("R_ZIPCMD", "zip"))
      },
      contentType = "application/zip"
    )
}

#-----------------------------------------
fetch_save_dir = function(session){
   cfg=gui_fetch_cfg(session)
   user_dir = find_user_dir(session)
   # if we're deployed then we save these files
   # to the user directory
   if(cfg$gui$deployed){
     save_dir = user_dir
   } else{
   # otherwise we save them in the main working direcotry
     save_dir = cfg$gui$wd 
   }
}

#-----------------------------------------
gui_save_cfg = function(cfg, session){
  user_dir = find_user_dir(session)
  save(cfg, file=sprintf('%s%sgui_state.RData', user_dir, .Platform$file.sep))
}
#-----------------------------------------
gui_fetch_cfg  <-function(session){
  user_dir = find_user_dir(session)
  load(file=sprintf('%s%sgui_state.RData', user_dir, .Platform$file.sep))
  return(cfg)}
#-----------------------------------------
gui_save_som = function(som, session){
  user_dir = find_user_dir(session)
  save(som, file=sprintf('%s%sgui_som.RData', user_dir, .Platform$file.sep))
}
#-----------------------------------------
gui_fetch_som  <-function(session){
  user_dir = find_user_dir(session)
  load(file=sprintf('%s%sgui_som.RData', user_dir, .Platform$file.sep))
  return(som)}
#-----------------------------------------

#-----------------------------------------

simulation_state<- function(input, output, session){

  # flip touch state for tables
  observe({
    input$table_parameters
    cfg=gui_fetch_cfg(session)
    cfg$gui$table_parameters_touched = TRUE  
    GUI_log_entry(cfg, "table_parameters touched")
    gui_save_cfg(cfg, session)
    }, priority=10)
  
  observe({
    input$table_iiv
    cfg=gui_fetch_cfg(session)
    cfg$gui$table_iiv_touched = TRUE  
    GUI_log_entry(cfg, "table_iiv touched")
    gui_save_cfg(cfg, session)
    }, priority=10)

  observe({
    input$table_covariates
    cfg=gui_fetch_cfg(session)
    cfg$gui$table_covariates_touched = TRUE  
    GUI_log_entry(cfg, "table_covariates touched")
    gui_save_cfg(cfg, session)
    }, priority=10)


  #--------------------------------------------------
  # Changes to parameters, inputs, etc are caught here and
  # used to update the cfg variable
  output$text_status = renderText({
  user_dir = find_user_dir(session)
  # Reading int the GUI state
  cfg=gui_fetch_cfg(session)
  sim_status = cfg$gui$sim_status;

  input$button_update

  # needed to force the changes in parameters after a new 
  # parameter set has been selected
  input$table_parameters
  input$table_iiv
  input$table_covariates


  #--[ checkboxes ]---------------------------------------------
  if(!is.null(input$check_repeatdoses)){
    if(cfg$gui$check_repeatdoses !=  input$check_repeatdoses){
      user_log_entry(cfg, sprintf("Repeat doses: %s", toString(input$check_repeatdoses))) 

      # If the repeatdoses is checked before any changes have been made to the
      # bolus dosing then we need to populate those components of the gui
      # components
      if(is.null(cfg$gui$bolus)){
        cfg$gui$bolus$times = cfg$options$inputs$bolus$times$values 
        for(bstate in names(cfg$options$inputs$bolus$species)){
          cfg$gui$bolus$states[[bstate]] = cfg$options$inputs$bolus$species[[bstate]]$values
        }
      }
      GUI_log_entry(cfg, "Stale: check_repeatdoses")
      sim_status = 'Stale' }
    cfg$gui$check_repeatdoses =  input$check_repeatdoses }
  
  if(!is.null(input$check_variability)){
    if(cfg$gui$check_variability !=  input$check_variability){
      user_log_entry(cfg, sprintf("Simulate with variability: %s", toString(input$check_variability))) 
      GUI_log_entry(cfg, "Stale: check_variability")

      # if the plotting code for param dists exists, we delete it
      if(file.exists(sprintf("%s%sgui_plot_paramdist.RData", user_dir, .Platform$file.sep))){
        file.remove(sprintf("%s%sgui_plot_paramdist.RData", user_dir, .Platform$file.sep))}
      sim_status = 'Stale' }

    cfg$gui$check_variability =  input$check_variability }
  

  if(!is.null(input$check_grid)){
    if(cfg$gui$check_grid   !=  input$check_grid){
      user_log_entry(cfg, sprintf("Plot with grid lines: %s", toString(input$check_grid))) }
    cfg$gui$check_grid =  input$check_grid}

  if(!is.null(input$check_log)){
    if(cfg$gui$check_log    !=  input$check_log) {
      user_log_entry(cfg, sprintf("Log10 Y-Scale: %s", toString(input$check_log))) }
    cfg$gui$check_log =  input$check_log}


  if(!is.null(input$check_autox)){
    if(cfg$gui$check_autox    !=  input$check_autox) {
      user_log_entry(cfg, sprintf("Auto X-axis: %s", toString(input$check_autox))) }
    cfg$gui$check_autox =  input$check_autox}


  if(!is.null(input$check_autoy)){
    if(cfg$gui$check_autoy    !=  input$check_autoy) {
      user_log_entry(cfg, sprintf("Auto X-axis: %s", toString(input$check_autoy))) }
    cfg$gui$check_autoy =  input$check_autoy}

  if(!is.null(input$check_timestamp)){
    if(cfg$gui$check_timestamp   !=  input$check_timestamp){
      user_log_entry(cfg, sprintf("Include timestamp: %s", toString(input$check_timestamp))) 
      GUI_log_entry(cfg, "Stale: check_timestamp") }
    cfg$gui$check_timestamp   =  input$check_timestamp   }


  #--[ text boxes ]---------------------------------------------
  # Bolus
  if(!is.null(input$text_bolus_frequency)){
    if(!is.na(as.numeric(input$text_bolus_frequency))){
      if(cfg$gui$text_bolus_frequency !=  as.numeric(input$text_bolus_frequency)){
        user_log_entry(cfg, sprintf("Bolus frequency: %s", input$text_bolus_frequency)) 
        GUI_log_entry(cfg, "Stale: text_bolus_frequency")
        sim_status = 'Stale' }
      cfg$gui$text_bolus_frequency =  as.numeric(input$text_bolus_frequency) }
    }

  if(!is.null(input$text_bolus_number)){
    if(!is.na(as.numeric(input$text_bolus_number))){
      if(cfg$gui$text_bolus_number !=  as.numeric(input$text_bolus_number)){
        user_log_entry(cfg, sprintf("Bolus number: %s", input$text_bolus_number)) 
        GUI_log_entry(cfg, "Stale: text_bolus_number")
        sim_status = 'Stale' }
      cfg$gui$text_bolus_number =  as.numeric(input$text_bolus_number)    }
    }

  # IIV   
  if(!is.null(input$text_nsub)){
    if(!is.na(as.numeric(input$text_nsub))){
      if(cfg$gui$text_nsub !=  as.numeric(input$text_nsub)){
        user_log_entry(cfg, sprintf("Number of subjects: %s", input$text_nsub)) 
        GUI_log_entry(cfg, "Stale: text_nsub")
        sim_status = 'Stale' }
      cfg$gui$text_nsub =  as.numeric(input$text_nsub)}
    }

  if(!is.null(input$text_ci)){
    if(!is.na(as.numeric(input$text_ci))){
      if(cfg$gui$text_ci !=  as.numeric(input$text_ci)){
        user_log_entry(cfg, sprintf("Prediction interval: %s%%", input$text_ci)) 
        GUI_log_entry(cfg, "Stale: text_ci")
        sim_status = 'Stale' }
      cfg$gui$text_ci =  as.numeric(input$text_ci)}
    }

  if(!is.null(input$text_save_sim)){
    if(cfg$gui$text_save_sim !=  input$text_save_sim){
      user_log_entry(cfg, sprintf("Save string: %s", input$text_save_sim)) 
      }
    cfg$gui$text_save_sim =  input$text_save_sim}

  # plotting
  if(!is.null(input$text_autox)){
    # converting the text_autox into a vector
    good_text_autox = FALSE
    tryCatch(
     { 
      eval(parse(text= sprintf('num_autox = c(%s)', input$text_autox))) 
      good_text_autox = TRUE
     },
      warning = function(w) {
      # place warning stuff here
     },
      error = function(e) {
        GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
        user_log_entry(cfg, sprintf('unable to parse string (%s)', input$text_autox))
     })
     
     

    # if it was successful we then look at the vector to see if it 
    # is formatted correctly: length =2, and first entry less than second
    if(good_text_autox){
      if(length(num_autox) == 2){
        if(num_autox[1] < num_autox[2]){
          if(any(cfg$gui$text_autox  !=  num_autox)){
             user_log_entry(cfg, sprintf("Xlim set to: c(%s)", input$text_autox)) 
             GUI_log_entry(cfg, "Stale: text_autox")
             sim_status = 'Stale' 
             cfg$gui$text_autox = num_autox
           }
        }
        else{
          user_log_entry(cfg, sprintf('User specified x-limits: should be increasing (%s)', input$text_autox))
          good_text_autox = FALSE
        }
      }
      else{
        good_text_autox = FALSE
        user_log_entry(cfg, sprintf('User specified x-limits: length should be 2 (%s)', input$text_autox))
      }
    # whatever value good has we use it to update cfg
    cfg$gui$good_text_autox = good_text_autox
    }
  }


  if(!is.null(input$text_autoy)){
    # converting the text_autoy into a vector
    good_text_autoy = FALSE
    tryCatch(
     { 
      eval(parse(text= sprintf('num_autoy = c(%s)', input$text_autoy))) 
      good_text_autoy = TRUE
     },
      warning = function(w) {
      # place warning stuff here
     },
      error = function(e) {
        GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
        user_log_entry(cfg, sprintf('unable to parse string (%s)', input$text_autoy))
     })

    # if it was successful we then look at the vector to see if it 
    # is formatted correctly: length =2, and first entry less than second
    if(good_text_autoy){
      if(length(num_autoy) == 2){
        if(num_autoy[1] < num_autoy[2]){
          if(any(cfg$gui$text_autoy  !=  num_autoy)){
             user_log_entry(cfg, sprintf("Ylim set to: c(%s)", input$text_autoy)) 
             cfg$gui$text_autoy = num_autoy
           }
        }
        else{
          user_log_entry(cfg, sprintf('User specified y-limits: should be increasing (%s)', input$text_autoy))
          good_text_autoy = FALSE
        }
      }
      else{
        good_text_autoy = FALSE
        user_log_entry(cfg, sprintf('User specified y-limits: length should be 2 (%s)', input$text_autoy))
      }
    # whatever value good has we use it to update cfg
    cfg$gui$good_text_autoy = good_text_autoy
    }
  }

  #--[ select_outputs ]------------------------------------
  if(!is.null(input$select_outputs)){
    cfg$gui$outputs = input$select_outputs
  }
  #--[ select_outputs done ]-------------------------------

  

  #--[ table_parameters ]---------------------------------------
  # we only look at the parameters table if
  # pset hasn't changed
  if(!cfg$gui$pset_change){
    # If the table_parmaeters is not null then changes have been made
    # we need to go through and modfiy the values for this parameter set
    if(!is.null(input$table_parameters) & cfg$gui$table_parameters_touched){
      for (pidx in seq(1,length(input$table_parameters$data))){
        # If the current row has the string "NA" in the second column
        # then it's a row header/grouping row and we ignore it
        # If not then it's a parameter and we update that parameter
        # in the parameter valuess.
        if(!is.null(input$table_parameters$data[[pidx]][[2]])){
        if(!is.na(input$table_parameters$data[[pidx]][[2]])){
          # pulling out the parameter name
          pname = input$table_parameters$data[[pidx]][[1]] 
          pupdate = FALSE

          # If the user deletes the contents of the cell in the table and
          # reactive picks it up the as.numeric function will return NA nad
          # screw things up down below. This should prevent that
          if(!is.na(as.numeric(as.numeric(input$table_parameters$data[[pidx]][[2]]))) ){
            # if the parameter has already been over written 
            if(pname %in% names(cfg$gui$parameters)){
              # we check to see if it's changed
              if(cfg$gui$parameters[[pname]] != as.numeric(input$table_parameters$data[[pidx]][[2]])){
                pupdate=TRUE
              }
            }
            # if the is parameter hasn't been overwritten by the user already
            # we see if it's different from the default value
            else if(cfg$parameters$values[[pname]] != as.numeric(input$table_parameters$data[[pidx]][[2]])){
              # this is the first time the parameter has been overwritten
              pupdate=TRUE
              }
            
            
            # If a parameter update has been triggered by the above conditions
            if(pupdate){
              cfg$gui$parameters[[pname]] = as.numeric(input$table_parameters$data[[pidx]][[2]])
              GUI_log_entry(cfg, "Stale: table_parameters")
              user_log_entry(cfg, sprintf("Setting parameter: %s = %s", pname, var2string(as.numeric(input$table_parameters$data[[pidx]][[2]]),1)))
              sim_status = 'Stale' 
              }
          }
        }
        }
      }
    }
  }
  #--[ table_parameters done ]----------------------------------
    
  #--[ table_bolus ]---------------------------------------
  # If there are changes in the table_bolus input 
  # then we need to update bolus dosing
  if(!is.null(input$table_bolus)){
    # storing the bolus dosing information in 'bolus'
    bolus = c()

    # bgood is used to tell if the bolus information is formatted correctly
    bgood = TRUE
    # bupdate is used to tell if the bolus information in the table is
    # different than the information currently stored in cfg$gui$bolus
    bupdate = FALSE

    # extracting the bolus times
    bolus$times = str2list(cfg, input$table_bolus$data[[1]][[2]])
    if(is.null(cfg$gui$bolus$times)){
      # checking to see if any bolus times 
      # have been specified by the user
      bupdate = TRUE }
    else if(length(bolus$times) != length(cfg$gui$bolus$times)){
       # if they have we look to see if they are the same length as the
       # previously specified times
      bupdate = TRUE }
    else if(any(bolus$times != cfg$gui$bolus$times)){
      # if they are different from the previously specified times
      # then we look to see if any of them are different
      bupdate = TRUE }

    # Now we go through dosing information for each compartment
    for (bidx in seq(2,length(input$table_bolus$data))){

      # pulling out the bolus information for the current compartment
      bstate = input$table_bolus$data[[bidx]][[1]] 
      bvalue = str2list(cfg, input$table_bolus$data[[bidx]][[2]])
      
      
      # storing the bolus information for the state in bolus$states
      if(length(bolus$times) == length(bvalue)){
        bolus$states[[bstate]] = bvalue }
      else{
       user_log_entry(cfg, sprintf("Number of doses into %s (%d) and dosing times (%d) are not the same", bstate, length(bvalue),length(bolus$times)))
       bgood = FALSE}

      if(is.null(cfg$gui$bolus$states)){
        # checking to see if any bolus states
        # have been specified by the user
        bupdate = TRUE }
      else if(length(bolus$states[[bstate]]) != length(cfg$gui$bolus$states[[bstate]])){
         # if they have we look to see if they are the same length as the
         # previously specified times
        bupdate = TRUE }
      else if(any(bolus$states[[bstate]] != cfg$gui$bolus$states[[bstate]])){
        # if they are different from the previously specified times
        # then we look to see if any of them are different
        bupdate = TRUE }
    }

    # Saving the information from the bolus dosing table 
    if(bgood){
      if(bupdate){
        sim_status = 'Stale'
        user_log_entry(cfg, "Bolus injections updated.")
      }
      cfg$gui$bolus = bolus 
    }
    else{   
      # usermessage
      user_log_entry(cfg, "Bolus injections not set.") }
  }
  #--[ table_bolus done ]----------------------------------



  #--[ table_iiv ]-----------------------------------------
  # we only look at the iiv table if
  # pset hasn't changed
  if(!cfg$gui$pset_change){
   if(!is.null(input$table_iiv) & cfg$gui$table_iiv_touched){
     # iupdate is used to tell if _any_ information in the iiv table has
     # changed
     iupdate = FALSE
   
     for(iridx in (seq(1,length(input$table_iiv$params$rowHeaders)))){
       for(icidx in (seq(1,length(input$table_iiv$params$colHeaders)))){
         rname = input$table_iiv$params$rowHeaders[[iridx]]
         cname = input$table_iiv$params$colHeaders[[icidx]]
         # Checking the variance
         if(icidx <= iridx){
           # iiv information from gui (user input)
           iiv_gui = input$table_iiv$data[[iridx]][[icidx]] 

           # If the user deletes the contents of the cell in the table and
           # reactive picks it up the as.numeric function will return NA nad
           # screw things up down below. This should prevent that
           if(!is.na(as.numeric(iiv_gui))){
             # default iiv information from the system file
             iiv_sys = system_fetch_iiv(cfg, IIV1=rname, IIV2=cname)
             # iiv information from the gui (previously)
             iiv_gui_old = cfg$gui$iiv[[rname]][[cname]]
             
             # ciupdate is used to tell if the current iiv term has changed
             ciupdate = FALSE
             # if the old GUI entry is null we see if the current gui entry is
             # different from the value in the system file
             if(is.null(iiv_gui_old)){
               if(iiv_sys != iiv_gui){
                 ciupdate = TRUE
                 iupdate = TRUE
               }
             }
             
             else if(iiv_gui_old != iiv_gui){
             # if the old gui entry isn't null we see if it's different 
             # from the current one
               ciupdate = TRUE
               iupdate = TRUE
             }
             
             if(ciupdate){
               user_log_entry(cfg, sprintf("IIV: %s_%s  set to %3e", rname, cname, iiv_gui))
               eval(parse(text=paste(sprintf('cfg$gui$iiv$%s$%s = iiv_gui', rname, cname))))
             }
           }
         }
       }
     }
     # storing the iivs that are different from the default
     if(iupdate & cfg$gui$check_variability){
       sim_status = 'Stale'
     }
     if(iupdate){
       user_log_entry(cfg, "IIV information updated.")
     }
   }
  }
  #--[ table_iiv done ]------------------------------------

  #--[ table_rates ]---------------------------------------
  if(!is.null(input$table_rates)){
    rates  = c() 
    rgood  = TRUE
    rupdate = FALSE

    # There are three rows per rate
    for(ridx in seq(1,length(input$table_rates$data)/3)){
      # the offset here is used to jump from rate to rate
      roffset = (ridx-1)*3
      rname   = input$table_rates$data[[roffset + 1]][[1]]
      rtimes  = str2list(cfg, input$table_rates$data[[roffset+2]][[2]])
      rvalues = str2list(cfg, input$table_rates$data[[roffset+3]][[2]])
      # the length should be the same for each of these
      if(length(rtimes) == length(rvalues)){
        irupdate = FALSE
        # if the rate has not been defined in the gui before, we assign the 
        # values for the gui to the default
        if(is.null(cfg$gui$infusion_rates[[rname]])){
           cfg$gui$infusion_rates[[rname]]$times  = cfg$options$inputs$infusion_rates[[rname]]$times$values  
           cfg$gui$infusion_rates[[rname]]$values = cfg$options$inputs$infusion_rates[[rname]]$levels$values 
        }
        # if it's been defined in the gui before, we check to see if it's
        # changed
        else if(any(cfg$gui$infusion_rates[[rname]]$times != rtimes) |
                any(cfg$gui$infusion_rates[[rname]]$values!= rvalues)){
            rupdate  = TRUE
            irupdate = TRUE
        }

        # this individual rate has been updated
        if(irupdate){
          cfg$gui$infusion_rates[[rname]]$times  = rtimes
          cfg$gui$infusion_rates[[rname]]$values = rvalues 
          user_log_entry(cfg, sprintf("Rate set (%s)", rname))
          user_log_entry(cfg, sprintf(" -> Times:  %s ", toString(rtimes)))
          user_log_entry(cfg, sprintf(" -> Values: %s ", toString(rvalues)))
        }
      }
      else{
        user_log_entry(cfg, sprintf("Lengths do not match for rate %s", rname))
        user_log_entry(cfg, sprintf("Number of Times   (%d)",                length(rtimes)))
        user_log_entry(cfg, sprintf("Number of Values  (%d)",                length(rvalues)))
        rgood = FALSE }
    }

    if(rgood){
      if(rupdate){
        sim_status = 'Stale'
        user_log_entry(cfg, "Infusion rates updated.")
      }
    }
    else{
      # usermessage
      user_log_entry(cfg, "Unable to update rates perhaps the user is not done.")
    }
  }
  #--[ table_rates done ]----------------------------------

  #--[ table_covariates ]----------------------------------
  # we only look at the covariates table if
  # pset hasn't changed
  if(!cfg$gui$pset_change){
    if(!is.null(input$table_covariates)){
      cgood  = TRUE
      # There are three rows per rate
      for(cidx in seq(1,length(input$table_covariates$data)/3)){
        cupdate= FALSE
        # the offset here is used to jump from rate to rate
        coffset = (cidx-1)*3
        cname   = input$table_covariates$data[[coffset + 1]][[1]]
        ctimes  = str2list(cfg, input$table_covariates$data[[coffset+2]][[2]])
        cvalues = str2list(cfg, input$table_covariates$data[[coffset+3]][[2]])
        # the length should be the same for each of these
        if(length(ctimes) == length(cvalues)){
          # Populating covariates in the gui state variable with the default
          # values if they have not been changed by the user
          if(is.null(cfg$gui$covariates[[cname]])){
            if(all(cfg$options$inputs$covariates[[cname]]$times$values  == ctimes) |
               all(cfg$options$inputs$covariates[[cname]]$values$values == cvalues)){
              cfg$gui$covariates[[cname]]$times  = cfg$options$inputs$covariates[[cname]]$times$values
              cfg$gui$covariates[[cname]]$values = cfg$options$inputs$covariates[[cname]]$values$values
            }
            else{
              cupdate = TRUE
            }
          }
          # This covariate has been updated so we store those updated values
          else if(any(cfg$gui$covariates[[cname]]$times  != ctimes) |
                  any(cfg$gui$covariates[[cname]]$values != cvalues)){
            cupdate = TRUE
          }
    
          if(cupdate){
            sim_status = 'Stale' 
            cfg$gui$covariates[[cname]]$times  = ctimes
            cfg$gui$covariates[[cname]]$values = cvalues
            user_log_entry(cfg, sprintf("Covariate set (%s)", cname))
            user_log_entry(cfg, sprintf(" -> Times:  %s ", toString(ctimes)))
            user_log_entry(cfg, sprintf(" -> Values: %s ", toString(cvalues)))
            user_log_entry(cfg, "Covariates updated.")
          }
    
          
        }
        else{
          user_log_entry(cfg, sprintf("Lengths do not match for covariate %s", cname))
          user_log_entry(cfg, sprintf("Number of Times   (%d)",                length(ctimes)))
          user_log_entry(cfg, sprintf("Number of Values  (%d)",                length(cvalues)))
          cgood = FALSE }
      }
      if(!cgood){
        # usermessage
        user_log_entry(cfg, "Unable to update covariates perhaps the user is not done.")
      }
    }
  }
  #--[ table_covariates done ]-----------------------------


  # parameter set has changed
  # This is fliped in the update_parameters function
  #--[ pset ]---------------------------------------------------
  if(!is.null(input$pset)){
    if(cfg$gui$pset_change){
     # we set the simulation state to stale and 
     # zero out any changes made in parameters, covariates 
     # and IIV terms
     sim_status = 'Stale' 

     cfg$gui$parameters   = list()
     cfg$gui$covariates   = list()
     cfg$gui$iiv          = list()

     # Now we flip the pset_change state back to false
     cfg$gui$pset_change  = FALSE

     # setting the state of the table to untouched
     cfg$gui$table_parameters_touched = FALSE
     cfg$gui$table_iiv_touched        = FALSE
     cfg$gui$table_covariates_touched = FALSE
    }
  }
  #--[ pset done ]----------------------------------------------

  # Saving the simulation status
  cfg$gui$sim_status = sim_status;

  #
  # Writing the gui state back to a file
  #      
  GUI_log_entry(cfg, sprintf("Simulation status set: %s", sim_status))
  gui_save_cfg(cfg, session)


  #
  # If simulation state is 'stale' we clear out all of the 
  # generated reports. This will force the App to regenerate 
  # them when tabs are accessed or if an export is triggered
  #
  if(sim_status == "Stale"){
    # finding all of the reports
    # basically any files in the user's 
    # directory that end in R#.html
    # where # is a number from 1-5
    user_dir   = find_user_dir(session)
    user_files = list.files(user_dir)
    user_files = user_files[grepl('R\\d\\.html$', user_files)]
 
    # browser()

    for(rfile in user_files){
      # getting the full path to the file
      rfile = sprintf('%s%s%s',user_dir, .Platform$file.sep, rfile)
      file.remove(rfile)
    }
  }


  #is.null(input$text_status)
  status =sprintf(" Simulation  %s", sim_status)})
  #-----------------------------------------

}
#-----------------------------------------


#-----------------------------------------
# generates the parameter table and updates 
# it based on changes in the parameter set
update_parameters <-function(input, output, session){
  output$table_parameters = renderRHandsontable({
  cfg=gui_fetch_cfg(session);
  if(is.null(input$pset)){
    cfg=system_select_set(cfg, cfg$parameters$current_set)}
  else{
    if(cfg$parameters$current_set != input$pset){
      # Because the parameter set has been updated we need to zero out any
      # parameter changes that have been made
      cfg$gui$pset_change       = TRUE
    }
    cfg=system_select_set(cfg, input$pset)
    }

  user_log_entry(cfg, sprintf('Parameter set: %s', cfg$parameters$sets[[cfg$parameters$current_set]]$name))
  
  # Parameter values:
  types        = unique(cfg$parameters$matrix$type)
  parametersDF = data.frame("Description"=c(), "Value"=c(), 'Units'=c())

  for(CURRTYPE in (types)){

    if(any(
          (cfg$parameters$matrix$editable == "yes" | cfg$gui$admin_mode) 
          & cfg$parameters$matrix$type == CURRTYPE)){
      parametersDF = rbind(parametersDF, data.frame("Description"=c(CURRTYPE), "Value"=c(NA),     'Units'=c(NA)))
      for(PIDX in seq(1,length(cfg$parameters$matrix$name))){
        PNAME  = as.character(cfg$parameters$matrix$name[PIDX])
        PVALUE = as.character(cfg$parameters$values[PIDX])
        PEDIT  = as.character(cfg$parameters$matrix$editable[PIDX])
        PTYPE  = as.character(cfg$parameters$matrix$type[PIDX])
        PPTYPE = as.character(cfg$parameters$matrix$ptype[PIDX])
        PUNITS = as.character(cfg$parameters$matrix$units[PIDX])
      
        # adding parameters of the current type 
        if((PTYPE == CURRTYPE) & (cfg$gui$admin_mode | PEDIT == "yes")){
          parametersDF = rbind(parametersDF, data.frame("Description"=c(PNAME), "Value"=c(PVALUE), 'Units'=c(PUNITS)));
        }
      }
    }
  }

  

  # Now we save the new parameter set and the zeroed values
  gui_save_cfg(cfg, session)

  
  ptable     = 
  hot_col(rhandsontable(parametersDF, readOnly=TRUE, rowHeaders=NULL, width="100%"), 
          'Value', readOnly=FALSE)  # making the values column editable

  })
}
#-----------------------------------------
# Generates the table of bolus information
generate_bolus <-function(input, output, session){
  cfg=gui_fetch_cfg(session)
  if("bolus" %in% names(cfg$options$inputs)){
    output$table_bolus = renderRHandsontable({

     # Creating the bolus times row
     mybolus=data.frame("Description"=c("Dose Times"), 
                        "Value"=c(toString(cfg$options$inputs$bolus$times$values)), 
                        "Units"=c(as.character(cfg$options$inputs$bolus$times$units)), stringsAsFactors = FALSE)


     # adding each state to receive a dose
     for(SPIDX in  names(cfg$options$inputs$bolus$species)){
       mybolus = rbind(mybolus, data.frame("Description"=c(as.character(SPIDX)),  
                                           'Value'=c(toString(cfg$options$inputs$bolus$species[[SPIDX]]$values)),  
                                           'Units'=c(cfg$options$inputs$bolus$species[[SPIDX]]$units)))
     
     }

     rh_bolus_table = hot_col(rhandsontable(mybolus, readOnly=TRUE, rowHeaders = NULL, width="100%"), 
                              'Value', readOnly=FALSE)
     #rh_bolus_table = hot_cell(rh_bolus_table, 1, 2, "Injection times separated by a comma")
     })
  }
}
#-----------------------------------------
# Generates the table of infusions 
generate_rates <-function(input, output, session){
  cfg=gui_fetch_cfg(session)
  if("infusion_rates" %in% names(cfg$options$inputs)){
    output$table_rates = renderRHandsontable({
      myrates=data.frame("Description"=c(), 
                         "Value"=c(), 
                         "Units"=c(), stringsAsFactors = FALSE)
      for(rate_name in names(cfg$options$inputs$infusion_rates)){
        myrates=rbind(myrates, 
                data.frame("Description"=c(rate_name), 
                           "Value"=c(NA), 
                           "Units"=c(NA)))
        myrates=rbind(myrates, 
                data.frame("Description"=c('Time'), 
                           #"Value"=c(paste(c(cfg$options$inputs$infusion_rates[[rate_name]]$times$values), collapse=', ' )), 
                           "Value"=c(toString(cfg$options$inputs$infusion_rates[[rate_name]]$times$values)),
                           "Units"=c(as.character(cfg$options$inputs$infusion_rates[[rate_name]]$times$units))))
        myrates=rbind(myrates, 
                data.frame("Description"=c('Rate'), 
                           "Value"=c(toString(cfg$options$inputs$infusion_rates[[rate_name]]$levels$values)), 
                           "Units"=c(as.character(cfg$options$inputs$infusion_rates[[rate_name]]$levels$units))))
      }  
      hot_col(rhandsontable(myrates, readOnly=TRUE, rowHeaders = NULL, width="100%"), 
              'Value', readOnly=FALSE)
    })
  }
}
#-----------------------------------------

#-----------------------------------------
# Generates the table of covariates
generate_covariates <-function(input, output, session){
  cfg=gui_fetch_cfg(session)
  if("covariates" %in% names(cfg$options$inputs)){
    output$table_covariates = renderRHandsontable({
      mycovariates=data.frame("Description"=c(), 
                              "Value"=c(), 
                              "Units"=c(), stringsAsFactors = FALSE)
      for(covariate_name in names(cfg$options$inputs$covariates)){

        if(is.null(input$pset)){
          cfg=system_select_set(cfg, 'default')}
        else{
          cfg=system_select_set(cfg, input$pset)}

        time_values      =      paste(c(cfg$options$inputs$covariates[[covariate_name]]$times$values),  collapse=', ' ) 
        covariate_values =      paste(c(cfg$options$inputs$covariates[[covariate_name]]$values$values), collapse=', ' )
        time_units       = as.character(cfg$options$inputs$covariates[[covariate_name]]$times$units)
        covariate_units  = as.character(cfg$options$inputs$covariates[[covariate_name]]$values$units)
        covariate_interp = as.character(cfg$options$inputs$covariates[[covariate_name]]$cv_interp)


        mycovariates=rbind(mycovariates, 
                data.frame("Description"=c(covariate_name), 
                           "Value"=c(NA), 
                           "Units"=c(paste('(', covariate_interp, ')', sep=""))))
        mycovariates=rbind(mycovariates, 
                data.frame("Description"=c('Time'), 
                           "Value"=c(time_values), 
                           "Units"=c(time_units)))
        mycovariates=rbind(mycovariates, 
                data.frame("Description"=c('Value'), 
                           "Value"=c(covariate_values), 
                           "Units"=c(covariate_units)))
      }  
      hot_col(rhandsontable(mycovariates, readOnly=TRUE, rowHeaders = NULL, width="100%"), 'Value', readOnly=FALSE)
    })
  }
}
#-----------------------------------------

#-----------------------------------------
# generates the iiv table 
generate_iiv <-function(input, output, session){
  output$table_iiv = renderRHandsontable({
    cfg=gui_fetch_cfg(session);
    if(is.null(input$pset)){
      cfg=system_select_set(cfg, 'default')}
    else{
      cfg=system_select_set(cfg, input$pset)}

      iivnames = names(cfg$iiv$iivs)
      iivvalues = cfg$iiv$values

      # removing the upper triangular portion of the matrix
      for(IIVROW in seq(1,length(iivnames))){
        for(IIVCOL in seq(1,length(iivnames))){
          if(IIVCOL > IIVROW){
           iivvalues[IIVROW, IIVCOL] = NA
          }
        }
      }


      for(IIVIDX in seq(1,length(iivnames))){
        if(IIVIDX == 1){
          eval(parse(text=paste(sprintf("myiiv = data.frame(%s = iivvalues[,IIVIDX])",as.character(iivnames[[IIVIDX]]))))) 
        } else{
          eval(parse(text=paste(sprintf("myiiv = cbind(myiiv, %s = iivvalues[,IIVIDX])",as.character(iivnames[[IIVIDX]]))))) }
      }

      row.names(myiiv) = iivnames 
      htiiv = rhandsontable(myiiv, readOnly=FALSE, width="100%", digits=14)
      htiiv = hot_cols(htiiv, renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (col > row) {
           td.style.background = 'grey';
           td.style.color = 'grey';
           cellProperties.readOnly = true;
          } 
        }")
      
      htiiv})

   # This forces the panel to be updated when 
   # the parameter set has been changed
   observe({updateTabsetPanel(session, 'panel_variability') })
  }

#-----------------------------------------
#'@title Collect Multiple Plots
#'@description:
#' Adapted from here:
#' http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#'@param ... list of plot objects  
#'@param plotlist list of plot objects  
#'@param cols number of columns
#'@param layout of the multiplot
#'@return multiplot object 
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  require("grid")
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#-----------------------------------------
  generate_paramdist  <-function(input, output, session){
   cfg = gui_fetch_cfg(session)
   # If variability is checked we load 
   # the simulation results and plot 
   # the parameter distribution
   pall = NULL
   if(cfg$gui$check_variability){

     #user_log_entry(cfg, "DBG: output: plotparamdist (before)")

       input$tabset_figures


       # loading the cfg and the simulated results 
       som = gui_fetch_som(session)
     
       piiv = names(cfg$iiv$parameters)

       # Determining the number of columns based
       # on the number of panels
       if(length(piiv) == 1){
         ncol = 1 }
       else if(length(piiv) <= 4){
         ncol = 2 }
       else{
         ncol = 3 }
       
       # Determining the number of bins
       if(cfg$gui$text_nsub <= 20){
         bins = 8  }
       else if(cfg$gui$text_nsub <= 100){
         bins = 20 }
       else{
         bins = 30 }

       plot_paramdist = "\n\n"
       
       ph_subs = c()
       for(pname in piiv){
         plot_paramdist = sprintf('%sps.%s = ggplot(som$subjects$parameters, aes(x=%s)) + \n', plot_paramdist, pname, pname)
         plot_paramdist = sprintf('%s        geom_histogram(colour="black", bins=%d) \n',      plot_paramdist, bins)
         plot_paramdist = sprintf('%s        ylab("")\n',                                      plot_paramdist)
         plot_paramdist = sprintf('%sps.%s =  prepare_figure(ps.%s, purpose="present")\n\n',   plot_paramdist, pname, pname)
         ph_subs = c(ph_subs, sprintf('ps.%s', pname))
       }

       tryCatch(
        { 
         eval(parse(text=paste(plot_paramdist)))
        },
         warning = function(w) {
         # place warning stuff here
        },
         error = function(e) {
           GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
           GUI_log_entry(cfg, sprintf('unable to parse string (%s)', plot_paramdist))
        })
       
       plot_paramdist_post = sprintf('pall = multiplot(%s, cols=%d)\n\n', paste(ph_subs, collapse = ", "), ncol)
     
       plot_paramdist = sprintf('%s\n%s', plot_paramdist, plot_paramdist_post)
     
       # saving the plotting strings
       cfg$gui$sysel_plot_paramdist = plot_paramdist

       sysel_plot_paramdist  = plot_paramdist
       save(sysel_plot_paramdist, file=sprintf("%s%sgui_plot_paramdist.RData", 
                                  find_user_dir(session), .Platform$file.sep))

       gui_save_cfg(cfg, session)

       eval(parse(text=paste(plot_paramdist_post)))
       }
}

#-----------------------------------------
#   update_paramdist  <-function(input, output, session){
# 
# 
#    cfg = gui_fetch_cfg(session)
#    # If variability is checked we load 
#    # the simulation results and plot 
#    # the parameter distribution
#    if(cfg$gui$check_variability){
# 
#      #user_log_entry(cfg, "DBG: output: plotparamdist (before)")
#      output$plotparamdist<- renderPlot({
# 
#        input$tabset_figures
# 
# 
#        # loading the cfg and the simulated results 
#        som = gui_fetch_som(session)
#      
#        piiv = names(cfg$iiv$parameters)
# 
#        # Determining the number of columns based
#        # on the number of panels
#        if(length(piiv) == 1){
#          ncol = 1 }
#        else if(length(piiv) <= 4){
#          ncol = 2 }
#        else{
#          ncol = 3 }
#        
#        # Determining the number of bins
#        if(cfg$gui$text_nsub <= 20){
#          bins = 8  }
#        else if(cfg$gui$text_nsub <= 100){
#          bins = 20 }
#        else{
#          bins = 30 }
# 
#        plot_paramdist = "\n\n"
#        
#        ph_subs = c()
#        for(pname in piiv){
#          plot_paramdist = sprintf('%sps.%s = ggplot(som$subjects$parameters, aes(x=%s)) + \n', plot_paramdist, pname, pname)
#          plot_paramdist = sprintf('%s        geom_histogram(colour="black", bins=%d) \n',      plot_paramdist, bins)
#          plot_paramdist = sprintf('%s        ylab("")\n',                                      plot_paramdist)
#          plot_paramdist = sprintf('%sps.%s =  prepare_figure(ps.%s, purpose="present")\n\n',   plot_paramdist, pname, pname)
#          ph_subs = c(ph_subs, sprintf('ps.%s', pname))
#        }
# 
#        tryCatch(
#         { 
#          eval(parse(text=paste(plot_paramdist)))
#         },
#          warning = function(w) {
#          # place warning stuff here
#         },
#          error = function(e) {
#            GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
#            GUI_log_entry(cfg, sprintf('unable to parse string (%s)', plot_paramdist))
#         })
#        
#        plot_paramdist_post = sprintf('multiplot(%s, cols=%d)\n\n', paste(ph_subs, collapse = ", "), ncol)
#      
#        plot_paramdist = sprintf('%s\n%s', plot_paramdist, plot_paramdist_post)
#      
#        # saving the plotting strings
#        cfg$gui$sysel_plot_paramdist = plot_paramdist
# 
#        sysel_plot_paramdist  = plot_paramdist
#        save(sysel_plot_paramdist, file=sprintf("%s%sgui_plot_paramdist.RData", 
#                                   find_user_dir(session), .Platform$file.sep))
# 
#        gui_save_cfg(cfg, session)
# 
#        eval(parse(text=paste(plot_paramdist_post)))}) 
#     }
#   }

#-----------------------------------------
  update_all_figures  <-function(input, output, session){

  cfg = gui_fetch_cfg(session)

  # Timecourse figure
  output$plotfigure<- renderPlot({
    generate_timecourse(input, output, session)}) 
  # Parameter distribution figure
  if(cfg$gui$check_variability){
   output$plotparamdist<- renderPlot({
     generate_paramdist(input, output, session)}) 
   }
}

#-----------------------------------------
generate_timecourse  <-function(input, output, session){
   cfg = gui_fetch_cfg(session)
   som = gui_fetch_som(session)

   # Loading user defined functions
   if(!is.null(cfg$gui$functions$user_def)){
     tryCatch(
      { 
       eval(parse(text=paste(sprintf('source("%s")',cfg$gui$functions$user_def))))
      },
       warning = function(w) {
       # place warning stuff here
      },
       error = function(e) {
         GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
      })
   }

   
   if(!is.null(cfg$gui$functions$plot_ind) & !cfg$gui$check_variability){
     # using custom individual plotting function
     pstr  = sprintf("p = %s\n", cfg$gui$functions$plot_ind)

     tryCatch(
      { 
       eval(parse(text=paste(pstr)))
      },
       warning = function(w) {
       # place warning stuff here
      },
       error = function(e) {
         GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
         GUI_log_entry(cfg, sprintf('unable to parse string (%s)', pstr))
      })
   

   } else if(!is.null(cfg$gui$functions$plot_var) & cfg$gui$check_variability){
   # using custom population function
     # using custom individual plotting function
     pstr  = sprintf("p = %s\n", cfg$gui$functions$plot_var)

     tryCatch(
      { 
       eval(parse(text=paste(pstr)))
      },
       warning = function(w) {
       # place warning stuff here
      },
       error = function(e) {
         GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
         GUI_log_entry(cfg, sprintf('unable to parse string (%s)', pstr))
      })


   } else{
   # Standard automated figure generation for the gui
     
     
     # Getting the TS
     if(is.null(cfg$options$misc$TS)){
       output_TS = "time"}
     else{
       output_TS = cfg$options$misc$TS }
     
     pstr = sprintf("p = ggplot() \n")
     
     # Storing all of the output information in the vector yall
     # to be used to identify the bounds at the end
     #yall = c()
     
     line_colors = "";
     
     
     octr = 1;
     for(output in cfg$gui$outputs){
     
       # With Variability
       if(cfg$gui$check_variability){
         if(cfg$gui$check_log){
           pstr = sprintf("%s p = p + geom_ribbon(data=som$tcsummary[(som$tcsummary$o.%s.lb_ci>0) & (som$tcsummary$o.%s.ub_ci > 0) ,], aes(x=ts.%s, ymin=o.%s.lb_ci, ymax=o.%s.ub_ci), fill='%s', alpha=0.6)\n",
                           pstr,                                                       output,                        output,                    output_TS, output,           output, cfg$gui$colors$region[octr]  )
         
           pstr = sprintf("%s p = p + geom_line(data=som$tcsummary[som$tcsummary$o.%s.median > 0,], aes(x=ts.%s, y=o.%s.median, color='%s'), linetype='solid', size=0.9) \n", 
                          pstr,                                                    output,             output_TS, output,         output) }
         else{
           pstr = sprintf("%s p = p + geom_ribbon(data=som$tcsummary, aes(x=ts.%s, ymin=o.%s.lb_ci, ymax=o.%s.ub_ci), fill='%s', alpha=0.6)\n",
                           pstr,                                                output_TS, output,           output, cfg$gui$colors$region[octr])
           pstr = sprintf("%s p = p + geom_line(data=som$tcsummary, aes(x=ts.%s, y=o.%s.median, color='%s'), linetype='solid', size=0.9) \n", 
                          pstr,                                       output_TS, output,           output) }
       }
       # Single simulation
       else{
         if(cfg$gui$check_log){
           pstr = sprintf("%s p = p + geom_line(data=som$simout[som$simout$%s > 0,], aes(x=ts.%s, y=%s, color='%s'), linetype='solid', size=0.9) \n", 
                   pstr, output, output_TS, output, output) }
         else{
           pstr = sprintf("%s p = p + geom_line(data=som$simout, aes(x=ts.%s, y=%s, color='%s'), linetype='solid', size=0.9) \n", 
                   pstr,  output_TS, output, output) }
       }
     
        #line_colors = c(line_colors, cfg$gui$colors$solid[octr])
        #line_colors = c(line_colors, sprintf(output=cfg$gui$colors$solid[octr])
        if(line_colors == ""){
          line_colors = sprintf('"%s"="%s"', output,cfg$gui$colors$solid[octr])
        }
        else{
          line_colors = sprintf('%s, "%s"="%s"', line_colors, output,cfg$gui$colors$solid[octr])
        }
     
       # now we construct the plot string
       octr = octr+1
       # if we have more outputs than colors then we repeat them
       if(octr > length(cfg$gui$colors$solid)){
         octr = 1 }
     }
     
     pstr = sprintf("%s p = p + xlab('Time (%s)')                     \n",pstr, output_TS )
     pstr = sprintf("%s p = p + ylab('Output')                        \n",pstr )
     pstr = sprintf("%s p = prepare_figure('present', fo=p, y_tick_major=%s, x_tick_major=%s)\n",pstr, toString(cfg$gui$check_grid), toString(cfg$gui$check_grid) )

     #pstr = sprintf("%s p = p + scale_colour_manual(values=c('%s'))   \n", pstr, paste(line_colors, collapse="', '"))
     pstr = sprintf("%s p = p + scale_colour_manual(values=c(%s))   \n", pstr, line_colors)
     pstr = sprintf("%s p = p + theme(legend.title = element_blank()) \n",pstr)
     pstr = sprintf("%s p = p + theme(legend.position = 'bottom')     \n",pstr)
     
     #pstr = sprintf("%s p = p + theme(legend.position='bottom')      \n",pstr )
     
     
     
     # evaling all of the plot strings up to this point
     # so we can create p and use that to determine the 
     # limits of the plot up to this point
     
     tryCatch(
      { 
       eval(parse(text=paste(pstr)))
      },
       warning = function(w) {
       # place warning stuff here
      },
       error = function(e) {
         GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
         GUI_log_entry(cfg, sprintf('unable to parse string (%s)', pstr))
      })
     
     # Next we look at the axis limits to see if they are user specified or
     # if they should be figured out automatically
     
     # If the user has unchecked autox and the text input is good
     # then we alter the axis based on what the user has selected
     pstr_post = ""
     if(!cfg$gui$check_autox & cfg$gui$good_text_autox  ){
       pstr_post = sprintf("%s p = p + xlim(%s)   \n",pstr_post, toString(cfg$gui$text_autox))
     }
     else{
     # otherwise we pull the range from the plot object p and update the GUI
     # with those values
       eval(parse(text=sprintf("cfg$gui$text_autox = c(%s,%s)", 
                  var2string(layer_scales(p)$x$range$range[1], maxlength = 1, nsig_e=2, nsig_f=4), 
                  var2string(layer_scales(p)$x$range$range[2], maxlength = 1, nsig_e=2, nsig_f=4))))
     
       updateTextInput(session, 'text_autox', value= 
         sprintf('%s, %s', 
             var2string(cfg$gui$text_autox[1] , maxlength = 1, nsig_e=2, nsig_f=4),
             var2string(cfg$gui$text_autox[2] , maxlength = 1, nsig_e=2, nsig_f=4)))
     }
     
     # now we repeat the same steps for the y axis
     if(!cfg$gui$check_autoy & cfg$gui$good_text_autoy  ){
        if(cfg$gui$check_log){
          # Changing the yscale to log 10 with user specified limits
          pstr_post = sprintf("%s p = gg_log10_yaxis(p, ylim_min = %s, ylim_max= %s)\n",pstr_post, toString(cfg$gui$text_autoy[1]), toString(cfg$gui$text_autoy[2]))

        }
        else{
          pstr_post = sprintf("%s p = p + ylim(%s)   \n",pstr_post, toString(cfg$gui$text_autoy))
        
        }
     }
     else{
        # Changing the yscale to log 10 using the default limits
        if(cfg$gui$check_log){
          pstr_post = sprintf("%s p = gg_log10_yaxis(p)  \n",pstr_post ) }
     
     
       eval(parse(text=sprintf("cfg$gui$text_autoy = c(%s,%s)", 
                  var2string(layer_scales(p)$y$range$range[1], maxlength = 1, nsig_e=2, nsig_f=4), 
                  var2string(layer_scales(p)$y$range$range[2], maxlength = 1, nsig_e=2, nsig_f=4))))
     
       updateTextInput(session, 'text_autoy', value= 
         sprintf('%s, %s', 
             var2string(cfg$gui$text_autoy[1] , maxlength = 1, nsig_e=2, nsig_f=4),
             var2string(cfg$gui$text_autoy[2] , maxlength = 1, nsig_e=2, nsig_f=4)))
     }
     
     # evalutating the "post" plot strings
     # to modify the axes
     tryCatch(
      { 
       eval(parse(text=paste(pstr_post)))
      },
       warning = function(w) {
       # place warning stuff here
      },
       error = function(e) {
         GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
         GUI_log_entry(cfg, sprintf('unable to parse string (%s)', pstr_post))
      })
     
     # concatenating them together to save 
     pstr = sprintf('%s\n%sprint(p)', pstr, pstr_post)
     
     
   }

   # saving the plotting strings
   cfg$gui$sysel_plot = pstr
   
   sysel_plot_timecourse = pstr
   save(sysel_plot_timecourse, file=sprintf("%s%sgui_plot_timecourse.RData", find_user_dir(session), .Platform$file.sep))
   gui_save_cfg(cfg, session)


p}


  

#--[ update_simulation ]---------------------------------
update_simulation <- function(input, output, session) {
   #loading the most up to date cfg variable
   cfg=gui_fetch_cfg(session);

   # Loading user defined libraries
   if(!is.null(cfg$gui$functions$user_def)){
     tryCatch(
      { 
       eval(parse(text=paste(sprintf('source("%s")',cfg$gui$functions$user_def))))
      },
       warning = function(w) {
       # place warning stuff here
      },
       error = function(e) {
         GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
      })
   }


  # Updating the simulation with the changes from the GUI
  if(cfg$gui$sim_status != 'Fresh'){
    sysel = c()

    #--[ options ]---------------------------------
    sysel$options = ""

    # output times
    ot = eval(parse(text=cfg$options$misc$output_times))
    tstart = min(ot)
    # used to calculate the step size
    tstart_step = min(ot)
    tstop  = max(ot)

    # If a start time has been specified then we use that one
    if(!is.null(cfg$gui$sim_starttime)){
      tstart = cfg$gui$sim_starttime } 
    
    minstep = cfg$gui$minstep
    
    # If the user has specified a stepsize for the steady state stabilization we use that, otherwise
    # we use the normal minstep
    if(!is.null(cfg$gui$ss_minstep)){
      ss_minstep = cfg$gui$ss_minstep}
    else{
      ss_minstep = minstep}
    
    if(!is.null(cfg$gui$sim_starttime)){
      tstart = cfg$gui$sim_starttime } 

    # adjusting the bounds on the time vector
    # for simulation purposes if autox is false
    if(!cfg$gui$check_autox){
      # text_autox is in the units of plotting
      # that needs to be converted to the simulation 
      # units to determine for defining the output times
      tstop = max(cfg$gui$text_autox)/(cfg$options$time_scales[[cfg$options$misc$TS]]) }
    
    if(tstart < 0){
      # SS initialization required
      sysel$options = sprintf('%soutput_times_ss_init = seq(%d, %d, %.4e)\n', sysel$options, tstart, 0,     ss_minstep*(0     - tstart))
      sysel$options = sprintf('%soutput_times_post_ss = seq(%d, %d, %.4e)\n', sysel$options, 0,      tstop,    minstep*(tstop - 0))
      sysel$options = sprintf('%soutput_times = sort(unique(c(output_times_ss_init, output_times_post_ss)))\n', sysel$options)
    }
    else{
      # SS not required
      sysel$options = sprintf('%soutput_times = seq(%d, %d, %.4e)\n ', sysel$options, tstart, tstop, cfg$gui$minstep*(tstop - tstart_step))}

    # Appending the output times definition:
    sysel$options = sprintf('%scfg = system_set_option(cfg, "simulation", "output_times", output_times)\n', sysel$options)

    #--[ parameters ]---------------------------------
    # Pulling out the default parameters for the current parameter set
    sysel$parameters = "parameters = system_fetch_parameters(cfg) \n "
    if(!is.null(cfg$gui$parameters)){
      for(pname in names(cfg$gui$parameters)){
        sysel$parameters = sprintf('%sparameters[["%s"]] = %s \n ', sysel$parameters, pname, toString(cfg$gui$parameters[[pname]]))
      }
    }

    #--[ bolus ]--------------------------------------
    sysel$bolus        = ""
    if(!is.null(cfg$gui$bolus)){
      btimes = cfg$gui$bolus$times;
      
      # adding the repeat dosing times to the end of the current btimes # vector
      if(cfg$gui$check_repeatdoses){
        btimes = c(btimes, seq(1,cfg$gui$text_bolus_number)*cfg$gui$text_bolus_frequency + tail(btimes, n=1)) }

      # clearing the default bolus information
      sysel$bolus = sprintf("cfg=system_zero_inputs(cfg, bolus=TRUE, rates=FALSE) \n\n")

      # setting the bolus information for each state
      for(sname in names(cfg$gui$bolus$states)){
        # pulling out the bolus values
        bvalues = cfg$gui$bolus$states[[sname]] 

        # adding the repeat dosing values to the end of the current bvalues vector
        if(cfg$gui$check_repeatdoses){
          bvalues = c(bvalues, rep(tail(bvalues, n=1), cfg$gui$text_bolus_number)) }
        sysel$bolus = sprintf('%s cfg = system_set_bolus(cfg, state  = "%s",  \n',   sysel$bolus, sname)
        sysel$bolus = sprintf('%s                             times  = c(%s), \n',   sysel$bolus, toString(btimes))
        sysel$bolus = sprintf('%s                             values = c(%s)) \n\n', sysel$bolus, toString(bvalues))
      
      }
    }

    #--[ iiv ]----------------------------------------
    sysel$iiv        = ""

    # these elements are added only if variability has been selected
    if(cfg$gui$check_variability){
      # IIV options first
      if(!is.null(cfg$gui$text_nsub)){
       sysel$iiv = sprintf('%s cfg = system_set_option(cfg, "stochastic", "nsub", %s)\n', sysel$iiv, toString(cfg$gui$text_nsub)) }
      if(!is.null(cfg$gui$text_ci)){
       sysel$iiv = sprintf('%s cfg = system_set_option(cfg, "stochastic", "ci",   %s)\n', sysel$iiv, toString(cfg$gui$text_ci)) }
      
      # Then we overwrite the values
      if(!is.null(cfg$gui$iiv)){
        for(iname1 in names(cfg$gui$iiv)){
          for(iname2 in names(cfg$gui$iiv[[iname1]])){
            sysel$iiv = 
            sprintf('%s cfg = system_set_iiv(cfg, IIV1="%s", IIV2="%s", %s)\n', 
                   sysel$iiv,
                   iname1, 
                   iname2, 
                   toString(cfg$gui$iiv[[iname1]][[iname2]]))
          }
        }
      }
    }



    #--[ rates ]--------------------------------------
    sysel$infusion_rates        = ""
    if(!is.null(cfg$gui$infusion_rates)){
      # clearing the default infusion rate information
      sysel$infusion_rates = sprintf("cfg=system_zero_inputs(cfg, bolus=FALSE, rates=TRUE) \n\n")

      # Now we dump definitions for each rate
      for(rname in names(cfg$gui$infusion_rates)){
        rtimes = cfg$gui$infusion_rates[[rname]]$times
        rvalues= cfg$gui$infusion_rates[[rname]]$values
        sysel$infusion_rates = sprintf('%s cfg = system_set_rate(cfg, rate   =  "%s",  \n', sysel$infusion_rates, rname)
        sysel$infusion_rates = sprintf('%s                            times  =  c(%s), \n', sysel$infusion_rates, toString(rtimes))
        sysel$infusion_rates = sprintf('%s                            levels =  c(%s)) \n\n', sysel$infusion_rates, toString(rvalues))
      }
    }

    #--[ covariates ]---------------------------------
    sysel$covariates = ""
    if(!is.null(cfg$gui$covariates)){
      # looping through each covariate and setting it in cfg
      for(cname in names(cfg$gui$covariates)){
        ctimes = cfg$gui$covariates[[cname]]$times
        cvalues = cfg$gui$covariates[[cname]]$value
        sysel$covariates = sprintf('%scfg = system_set_covariate(cfg, covariate = "%s",    \n', sysel$covariates, cname)
        sysel$covariates = sprintf('%s                                times     = c(%s),   \n', sysel$covariates, toString(ctimes))
        sysel$covariates = sprintf('%s                                values    = c(%s))   \n\n', sysel$covariates, toString(cvalues))
      }
    }

    # The strings built above will now be executed to populate
    # the elements of cfg that are different in the GUI
    for(ele in names(sysel)){
      tryCatch(
       { 
        eval(parse(text=paste(sysel[[ele]]))) 
       },
        warning = function(w) {
        # place warning stuff here
       },
        error = function(e) {
          GUI_log_entry(cfg, sprintf('eval error: %s ', e$message))
          GUI_log_entry(cfg, sprintf('unable to parse string (%s)', sysel[[ele]]))
       })}

    # Defining the parameter set
    # This is done _after_ the eval 
    # to be present in exporting
    sysel$pset = cfg$parameters$current_set

    # Running simulations
    # If check_variability is true
    # then we run stochastic simulations
    sim_tic  = proc.time()
    system_log_debug_save(cfg, 
       file_name = 'app_pre_sim',
       values    = list(cfg=cfg, parameters=parameters))
    if(cfg$gui$check_variability){
      user_log_entry(cfg, "Beginning simulations")
      user_log_entry(cfg,         " -> type: stochastic  ")
      user_log_entry(cfg, sprintf(" -> number of subjects: %d",cfg$options$stochastic$nsub))
      user_log_entry(cfg, sprintf(" -> integrating with %s", cfg$options$simulation_options$integrate_with))
      sysel$sim = sprintf("som = %s\n", cfg$gui$functions$sim_var)
      eval(parse(text=paste(sysel["sim"]))) 

    }
    else{
    #otherwise we just run an individual simulation
      user_log_entry(cfg, "Beginning simulation")
      user_log_entry(cfg,         " -> type: individual  ")
      user_log_entry(cfg, sprintf(" -> integrating with %s", cfg$options$simulation_options$integrate_with))
      sysel$sim = sprintf("som = %s\n", cfg$gui$functions$sim_ind)

      # Using progress indicator do tell the user what's going on
      
      pb <- shiny::Progress$new()
      pb$set(message = "Running Simulation, be patient", value = 0)
      eval(parse(text=paste(sysel["sim"]))) 
      pb$close()
    }
    system_log_debug_save(cfg, 
       file_name = 'app_post_sim',
       values    = list(cfg=cfg, parameters=parameters, som=som))

    # If we're not in admin mode we trim off any initialization times,
    # otherwise we include them
    if(!cfg$gui$admin_mode){
      som = system_trim_som_initialization(cfg, som, variability=cfg$gui$check_variability) }


    gui_save_som(som, session)
    sim_toc  = proc.time()
    telapsed =  (sim_toc - sim_tic)[["elapsed"]]
    user_log_entry(cfg, sprintf("Simulation complete (%s seconds)", var2string(telapsed, 1)))
    cfg$gui$sim_status = 'Fresh'
    # storing the sysel in the cfg variable
    # to be used when exporing
    cfg$gui$sysel_simulate = sysel
    
    # now we save the cfg variable with the system elements
    # as strings to be used for exporting 
    gui_save_cfg(cfg, session)


  }
}
#--[ update_simulation done ]----------------------------
user_log_init <- function(input, output, session) {
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

  # returning the full path to the user directory
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
  
  # initializing the general ubiquity log
  cfg = system_set_option(cfg, 
                        group="logging", 
                        option="file",
                        file.path(user_dir, "ubiquity_log.txt"))
                        # JMH del sprintf('%s%subiquity_log.txt', user_dir, .Platform$file.sep))
  cfg = system_log_init(cfg)
  GUI_log_entry(cfg, "server.R startup")
  
  # initialzing the user log
  cfg$gui$user_log_file =  sprintf('%s%suser_log.txt', user_dir, .Platform$file.sep)
  file.create(cfg$gui$user_log_file)


  # Default to integrating with r scripts
  cfg$options$simulation_options$integrate_with  = "r-file"
  # If the dynamic library exists we try to load it
  if(file.exists(file.path("transient","app_base",  paste("ubiquity_app_base", .Platform$dynlib.ext, sep = "")))){
    GUI_log_entry(cfg, "Found dynamic library attempting to load")
    dyn.load(file.path("transient", "app_base",  paste("ubiquity_app_base", .Platform$dynlib.ext, sep = ""))) }

  # If the library has been loaded we switch to C
  if(is.null(getLoadedDLLs()$ubiquity_app_base) == FALSE){
    if(getLoadedDLLs()$ubiquity_app_base[["dynamicLookup"]] == TRUE){
      GUI_log_entry(cfg, "Dynamic library seems to be loaded, setting")
      GUI_log_entry(cfg, "integration method to c-file")
      cfg$options$simulation_options$integrate_with  = "c-file"
    }
  }
  # saving the cfg variable
  gui_save_cfg(cfg, session)
}

apply_overwrite <- function(cfg){
  # pulling the default values for the currently selected parameterset
  parameters = cfg$parameters$values

  # if the user has specified different
  # parameters we update them here
  if(!is.null(cfg$gui$parameters)){
    for(pname in names(cfg$gui$parameters)){
      if(pname %in% names(parameters)){
        parameters[[pname]] = cfg$gui$parameters[[pname]]
      }
    }
  
  }
  return(parameters)
}


# generates the html report
# RID is the report ID R1, R2, ... or R5
generate_model_report <- function(cfg, session, RID){

    parameters = apply_overwrite(cfg)
    som=gui_fetch_som(session)

    params = c()
    params$cfg        = cfg
    params$parameters = parameters
    params$som        = som


    # getting the file names for the 
    user_dir = find_user_dir(session)
    Rmdfile  = sprintf('%s%s%s', cfg$gui$wd, .Platform$file.sep, cfg$gui$modelreport_files[[RID]]$file)
    htmlfile = sprintf('%s%smodel_report_%s.html', user_dir, .Platform$file.sep, RID)
    htmlfile_base = paste("model_report_", RID, ".html", sep="")

    # If the html file does not exits and the Rmd file ends in html then we 
    # just use that as the report instead of rendering it.
    if(!file.exists(htmlfile)){
    if(grepl("\\.html$", Rmdfile, ignore.case=TRUE)){
      file.copy(Rmdfile, htmlfile, overwrite=TRUE)
    }
    }

    success = FALSE
    message = "Unable to generate html report"
    # If the report has already been generated 
    if(file.exists(htmlfile)){
      success = TRUE
      message = "success: previous report" 
      user_log_entry(cfg, sprintf("Using previously generated report: %s", htmlfile)) 
    
    } 
    else{
      user_log_entry(cfg, sprintf("Generating report: %s", htmlfile)) 
      # rendering the markdown to a temporary html file
     tryCatch(
      { 
       suppressWarnings(
        rmarkdown::render(Rmdfile,
                          params        = params,
                          output_format = "html_document",
                          output_file   = htmlfile)
        )
        success = TRUE
        message = "success: report generated"
      },
       warning = function(w) {
       # place warning stuff here
      },
        error = function(e) {
      })
    }


    output = c()
    output$htmlfile      = htmlfile
    output$htmlfile_base = htmlfile_base
    output$success       = success
    output$message       = message
    return(output)
}


# generates the model report
update_uimodelreport <- function(input, output, session){

  output$uimodelreport_R1 = renderUI({
    # forcing an update when text_status has been updated
    input$button_update
    input$text_status  

    cfg = gui_fetch_cfg(session)
    if(cfg$gui$sim_status == "Fresh"){
      # generating the report in html
      mg      = generate_model_report(cfg, session, "R1")
      if(mg$success){
        result  = tags$iframe(seamless = "seamless",
                  src   = file.path(find_user_dir(session, full=FALSE), mg$htmlfile_base),
                  width = 600, height=700)
      } else {
        result = "Report generation failed"
        user_log_entry(cfg, "Unable to generate report") }

    } else{
      result = 'The simulation status is stale, you need to push the Update Plot Button before regenerating the report' }
  
  result})

  output$uimodelreport_R2 = renderUI({
    # forcing an update when text_status has been updated
    input$button_update
    input$text_status  

    cfg = gui_fetch_cfg(session)
    if(cfg$gui$sim_status == "Fresh"){
      # generating the report in html
      mg      = generate_model_report(cfg, session, "R2")
      if(mg$success){
        result  = tags$iframe(seamless = "seamless",
                  src   = file.path(find_user_dir(session, full=FALSE), mg$htmlfile_base),
                  width = 600, height=700)
      } else {
        result = "Report generation failed"
        user_log_entry(cfg, "Unable to generate report") }

    } else{
      result = 'The simulation status is stale, you need to push the Update Plot Button before regenerating the report' }
  
  result})

  output$uimodelreport_R3 = renderUI({
    # forcing an update when text_status has been updated
    input$button_update
    input$text_status  

    cfg = gui_fetch_cfg(session)
    if(cfg$gui$sim_status == "Fresh"){
      # generating the report in html
      mg      = generate_model_report(cfg, session, "R3")
      if(mg$success){
        result  = tags$iframe(seamless = "seamless",
                  src   = file.path(find_user_dir(session, full=FALSE), mg$htmlfile_base),
                  width = 600, height=700)
      } else {
        result = "Report generation failed"
        user_log_entry(cfg, "Unable to generate report") }

    } else{
      result = 'The simulation status is stale, you need to push the Update Plot Button before regenerating the report' }
  
  result})

  output$uimodelreport_R4 = renderUI({
    # forcing an update when text_status has been updated
    input$button_update
    input$text_status  

    cfg = gui_fetch_cfg(session)
    if(cfg$gui$sim_status == "Fresh"){
      # generating the report in html
      mg      = generate_model_report(cfg, session, "R4")
      if(mg$success){
        result  = tags$iframe(seamless = "seamless",
                  src   = file.path(find_user_dir(session, full=FALSE), mg$htmlfile_base),
                  width = 600, height=700)
      } else {
        result = "Report generation failed"
        user_log_entry(cfg, "Unable to generate report") }

    } else{
      result = 'The simulation status is stale, you need to push the Update Plot Button before regenerating the report' }
  
  result})

  output$uimodelreport_R5 = renderUI({
    # forcing an update when text_status has been updated
    input$button_update
    input$text_status  

    cfg = gui_fetch_cfg(session)
    if(cfg$gui$sim_status == "Fresh"){
      # generating the report in html
      mg      = generate_model_report(cfg, session, "R5")
      if(mg$success){
        result  = tags$iframe(seamless = "seamless",
                  src   = file.path(find_user_dir(session, full=FALSE), mg$htmlfile_base),
                  width = 600, height=700)
      } else {
        result = "Report generation failed"
        user_log_entry(cfg, "Unable to generate report") }

    } else{
      result = 'The simulation status is stale, you need to push the Update Plot Button before regenerating the report' }
  
  result})
}

server <- function(input, output, session) {

  # Initializing the the session for 
  # the current user
  initialize_session(session) 
  addResourcePath(find_user_dir(session, full=FALSE), find_user_dir(session))
  cfg=gui_fetch_cfg(session)
  user_log_init(input, output, session)

  # populating the GUI with the model information
  update_parameters(input, output, session)
  generate_bolus(input, output, session)
  generate_rates(input, output, session)
  generate_covariates(input, output, session)
  generate_iiv(input, output, session)

  output$my_plot_tabs = renderUI({

      # Creating the tab for the plot
      timecourse_tab =  tabPanel("Timecourse",  id="panel_timecourse",  
                                plotOutput('plotfigure', 
                                click  = "plotfigure_click", 
                                width  = cfg$gui$dims$timecourse$width, 
                                height = cfg$gui$dims$timecourse$height))
     
      # Creating subjects tab
      paramdist_tab =  tabPanel("System Parameters", id="panel_paramdist",  
                     plotOutput('plotparamdist', 
                                width  = cfg$gui$dims$paramdist$width, 
                                height = cfg$gui$dims$paramdist$height))
     
      # Tab for model diagram
      modeldiagram_tab =  NULL
     
      # If a file was found then we enable the diagram tab
      if(!is.null(cfg$gui$modeldiagram_file)){
        modeldiagram_tab = tabPanel("Model Diagram", id="panel_modeldiagram", 
                        imageOutput('diagramfigure', 
                                   width  = cfg$gui$dims$modeldiagram$width, 
                                   height = cfg$gui$dims$modeldiagram$height))
      }


      mpt=c('timecourse_tab')

      if(!is.null(input$check_variability)){
        if(input$check_variability){
          mpt=c(mpt, 'paramdist_tab') 
        }
      }
      
      if(!is.null(modeldiagram_tab)){
        mpt=c(mpt, 'modeldiagram_tab') 
      }

      # Creating model report tabs

      if(!is.null(cfg$gui$modelreport_files)){
        # For each specified report we create a tab
        for(report_name in names(cfg$gui$modelreport_files)){
          if(is.null(cfg$gui$modelreport_files[[report_name]]$title)){
            tab_title = "Model Report"
          } else {
            tab_title = cfg$gui$modelreport_files[[report_name]]$title }
            
          # making a string that contains the tab definition and evaluating it
          mr_str = sprintf('modelreport_tab_%s = tabPanel("%s", id="panel_modelreport_%s", htmlOutput("uimodelreport_%s"))',
                                               report_name,  tab_title,                report_name,                    report_name)
          eval(parse(text=mr_str))

          # appending the tab to the list of tabs
          mpt=c(mpt, sprintf('modelreport_tab_%s', report_name)) 
        }
      }


      mptstr = sprintf('tabsetPanel(type="tabs", id="tabset_figures", %s)', paste(mpt, collapse=", "))
      eval(parse(text=mptstr))
  })

  # updating the tabs in the GUI dynamically depending 
  # on what is present in this particular model
  output$my_parameter_tabs = renderUI({

    # Creating selections for parameter sets:
    PSETS = c()
    for(SETNAME in names(cfg$parameters$sets)){
      PSETS[[cfg$parameters$sets[[SETNAME]]$name]] = SETNAME }
    # Creating the tab for the parameters:
    parameter_tab = tabPanel("Parameters", 
               wellPanel(id = "PPanel",style = "overflow-y:scroll; max-height: 600px", 
               selectInput("pset", label = NULL, choices = PSETS, selected = cfg$parameters$current_set),
               rHandsontableOutput("table_parameters")))
    
    # Creating the tab for the variability
    variability_tab = NULL
    if(("iivs" %in% names(cfg[["iiv"]])) &  cfg$gui$display$iivtab ){
    variability_tab = tabPanel("Variability", 
                      wellPanel(id = "panel_variability",style = "overflow-y:scroll; max-height: 600px", 
                       checkboxInput('check_variability', "Simulate with variability", FALSE),
                       div(style="display:inline-block; max-width: 80px", textInput("text_nsub", "Number of Subjects", toString(as.integer(cfg$gui$text_nsub)))),
                       div(style="display:inline-block; max-width: 50px", ''),
                       div(style="display:inline-block; max-width: 80px", textInput("text_ci", "Prediction Interval (%)", toString(as.integer(cfg$gui$text_ci)))),
                       p(),
                       strong("Variance-Covariance Matrix"), 
                         rHandsontableOutput("table_iiv")))
    }


    if(("iivs" %in% names(cfg[["iiv"]])) &  cfg$gui$display$iivtab ){
      tabsetPanel(type="tabs", parameter_tab, variability_tab)}
    else{
      tabsetPanel(type="tabs", parameter_tab)}
  })


  output$my_input_tabs = renderUI({

    covariates_tab = 
    tabPanel("Covariates",
             wellPanel(id = "panel_covariates",style = "overflow-y:scroll; max-height: 450px", 
             rHandsontableOutput("table_covariates")))
    
    infusions_tab = 
    tabPanel("Infusions", 
             wellPanel(id = "panel_infusions",style = "overflow-y:scroll; max-height: 450px", 
             rHandsontableOutput("table_rates")))
    
    bolus_tab =  
    tabPanel("Injections",
             wellPanel(id = "panel_bolus",style = "overflow-y:scroll; max-height: 450px", 
             rHandsontableOutput("table_bolus"), 
             checkboxInput('check_repeatdoses', 'Repeat the last dose', value=cfg$gui$check_repeatdoses),
             div(style="display:inline-block", textInput('text_bolus_frequency', label=NULL, value=toString(as.integer(cfg$gui$text_bolus_frequency)))), "(Dosing Interval)",
             tags$style(type='text/css', "#text_bolus_frequency { width: 50px; }"), br(),
             div(style="display:inline-block", textInput('text_bolus_number', label=NULL, value=toString(as.integer(cfg$gui$text_bolus_number)))), "(Number of doses)", 
             tags$style(type='text/css', "#text_bolus_number    { width: 50px; }")))
    
    noinputs_tab = 
    tabPanel("Inputs", "No input information (bolus, infusion rates, or covariates) has been specified")

    input_tabs = c()
    
    # constructing the tabs based on whether the input type exists:
    if("bolus" %in% names(cfg$options$inputs)){
      input_tabs = append(input_tabs, 'bolus_tab') }
    if("infusion_rates" %in% names(cfg$options$inputs)){
      input_tabs = append(input_tabs, 'infusions_tab') }
    if("covariates" %in% names(cfg$options$inputs)){
      input_tabs = append(input_tabs, 'covariates_tab') }

    # if no inputs  exists we create a default tab
    if(length(input_tabs) == 0){
      input_tabs = append(input_tabs, 'noinputs_tab') }
      
     # tabsetPanel(type="tabs", bolus_tab, infusions_tab , covariates_tab)
     eval(parse(text=sprintf('tabsetPanel(type="tabs", %s)', paste(input_tabs, collapse = ', ') )))
  })

  # initializing the plot
  if(cfg$gui$sim_status == "Initialization"){
    user_log_entry(cfg, sprintf("Initial simulation based on default information (%s)", ubiquity_distribution))
    update_simulation(input,output, session)
    update_all_figures(input, output, session)
  }

  # only updating the plot when the button is clicked
  observeEvent(input$button_update,{
    update_simulation(input,output, session)})

  # Updating the simulation state based on user input
  simulation_state(input, output, session)
  update_uimodelreport(input,output,session)
  
  # Certain button clicks, checks, etc will force the plot to update
  # toggling grid
  observeEvent(input$check_grid,{
    update_all_figures(input, output, session) })
  # Toggling y scale
  observeEvent(input$check_log, {
    update_all_figures(input, output, session) })
  # Updating the plot 
  observeEvent(input$button_update,{
    update_all_figures(input, output, session) })

  # Updating the plot 
  observeEvent(input$plotfigure_click,{
    select_point(input, output, session)
  })

  # force figures to be updated when the tabs are selectd
  observeEvent(input$tabset_figures,{
    update_all_figures(input, output, session) })


  # Downloading the current simulation
  download_simulation(input, output, session)

  

  # Pushes the model diagram to the GUI
  if(!is.null(cfg$gui$modeldiagram_file)){
    output$diagramfigure = renderImage({ 
       list(src = cfg$gui$modeldiagram_file,
            height = 450,
            alt = "Model Diagram")}, deleteFile = FALSE)
  }

}


