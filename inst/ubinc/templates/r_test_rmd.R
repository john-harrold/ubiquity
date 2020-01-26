rm(list=ls())
library("shiny")
library("rhandsontable")
library("deSolve")
library("ggplot2")


if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path('library', 'r_general', 'ubiquity.R')) }

cfg = build_system(output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

Rmdfile = "system_report.Rmd"

# If you run the app, put in the values you want in terms of parameters and
# dosing then run the simulation it should save the following file with a
# snapshot of the values that will be sent into the report for generation:
load(file.path("transient", "app_base", "app_post_sim.RData"))
params = list()
params$cfg        = values$cfg
params$som        = values$som
params$parameters = values$parameters

# This is needed for debugging
cfg$options$misc$operating_environment = 'script'

rmarkdown::render(Rmdfile,
                  params        = params,
                  output_format = "html_document")
