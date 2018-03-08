rm(list=ls())
library("shiny")
library("rhandsontable")
library("deSolve")
library("ggplot2")

source('library/r_general/ubiquity.r')
source('transient/auto_rcomponents.r')

Rmdfile = "my_report.Rmd"
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
