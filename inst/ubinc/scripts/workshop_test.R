rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)
options(error=utils::recover)


# This script is designed to the sections of the workshop. It will copy each
# section into a sub directory and then run the files. It just runs them to
# see if they will run to completion. It will only work with the package
# distribution

library(ubiquity)


# Workshop sections
ws= list()
ws$Simulation$scripts = c("analysis_multiple.r",
                          "analysis_multiple_file.r",
                          "analysis_single.r")

ws$Estimation$scripts = c("analysis_parent.r",
                          "analysis_parent_metabolite.r",
                          "analysis_parent_metabolite_global.r",
                          "analysis_parent_metabolite_nm_data.r")

ws$Reporting$scripts  = c("make_report_PowerPoint.R",
                          "make_report_Word.R")

ws$Titration$scripts  = c("analysis_repeat_dosing.r",
                          "analysis_repeat_infusion.r",
                          "analysis_state_reset.r",
                          "analysis_visit_dosing_titration.r",
                          "analysis_visit_dosing_titration_stochastic.r",
                          "analysis_visit_infusion_dosing.r")


totest = names(ws)

#totest = "Reporting"

totest = "Simulation"


# First we clear out any testing directories already present
for(sec in totest){
  if(dir.exists(sec)){
    unlink(sec, recursive=TRUE)
  }
}


# Now we go through and test each section
for(sec in totest){
  # making a directory for the workshop section
  dir.create(path=sec)
  # moving into the directory
  setwd(sec)
  # fetching the section of the workshop
  fr = workshop_fetch(section=sec)
  # moving back into the testing directory
  setwd('..')

  cat(sprintf("# \n"))
  cat(sprintf("# \n"))
  cat(sprintf("# \n"))
  cat(sprintf("# Testing Section: %s \n", sec))
  cat(sprintf("# \n"))
  cat(sprintf("# \n"))
  cat(sprintf("# \n"))
  # Running each script
  for(script_file in ws[[sec]]$scripts){
    cat(sprintf("# \n"))
    cat(sprintf("# Testing Script: %s \n", script_file))
    cat(sprintf("# \n"))
    syscmd = sprintf("R -e 'setwd(\"%s\"); source(\"%s\")'", sec, script_file)
    system(syscmd)
  }
}



