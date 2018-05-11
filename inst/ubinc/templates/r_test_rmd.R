rm(list=ls())
library("shiny")
library("rhandsontable")
library("deSolve")
library("ggplot2")


if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path('library', 'r_general', 'ubiquity.R')) }

cfg = build_system()

Rmdfile = "system_report.Rmd"
load("transient/rgui/default/gui_som.RData")
load("transient/rgui/default/gui_state.RData")
params = list()
params$cfg = cfg
params$som = som

# This is needed for debugging
cfg$options$misc$operating_environment = 'script'

rmarkdown::render(Rmdfile,
                  params        = params,
                  output_format = "html_document")
