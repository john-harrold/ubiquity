rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

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

ws$Reporting$scripts  = c("make_report.R")


totest = names(ws)

totest = "Reporting"


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

  # making that new directory the working directory
  setwd(sec)


  # fetching the section of the workshop
  fr = workshop_fetch(section=sec)

  cat(sprintf("# \n"))
  cat(sprintf("# \n"))
  cat(sprintf("# Testing Section: %s \n", sec))
  cat(sprintf("# \n"))
  cat(sprintf("# \n"))
  # Running each script
  for(file in ws[[sec]]$scripts){
    cat(sprintf("# \n"))
    cat(sprintf("# Testing Script: %s \n", file))
    cat(sprintf("# \n"))
    source(file)
  }
  setwd('..')
}



