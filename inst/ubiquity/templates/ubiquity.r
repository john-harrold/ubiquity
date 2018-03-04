
require(deSolve)
require(ggplot2)
require(foreach)
require(doParallel)
require(doRNG)

# build_system
#
# Builds the system.txt file and attempts to compile and load the C library
#

#'@export
#'@title Building The System
#'@description  Builds the specified system file creating the targets for R and other languages as well as the templates for performing simulations and estimations. 
#'
#'@param system_file name of the file defining the system in the \href{https://ubiquity.tools}{ubiquity} format (default = 'system.txt')
#'@param distribution indicates weather you are using a 'package' or a 'stand alone' 
#' distribution of ubiquity. If set to 'automatic' the build script will first 
#' look to see if the ubiquity R package is installed. If it is installed it
#' will use the package. Otherwise, it will assume a 'sand alone' package.
#'
#'@examples
#' # build_system(system_file='system.txt')
build_system <- function(debug          =  FALSE, 
                         clean          =  TRUE, 
                         system_name    = "mysystem",
                         distribution   = "automatic",
                         system_file    = "system.txt") {

# If the distribution is set to automatic we first try to find the ubiquity
# package. If that's not available then we default to "stand alone"
if(distribution == "automatic"){
 if("ubiquity" %in% rownames(installed.packages())){
   distribution = "package" }
 else{
   distribution = "stand alone" }
} else if(distribution == "package"){
   # If it's set to package we make sure the package is installed and
   # if ti's not we default to stand alone
   if(!("ubiquity" %in% rownames(installed.packages()))){
     cat(sprintf("#> Warning: package selected but not found\n"))
     distribution = "stand alone" }
}


# For stand alone distributions we just use the default template and transient
# directory
if(distribution == "stand alone"){
  templates       = file.path("library", "templates")
  temp_directory  = "transient"
  build_script_pl = "build_system.pl"
}

# For the package we pull the package install location to point to files
# needed to build the system
if(distribution == "package"){
  package_dir     = find.package('ubiquity', lib.loc = NULL, quiet = FALSE, verbose = getOption("verbose"))
  temp_directory  = file.path(getwd(), "transient")
  templates       = file.path(package_dir, 'ubiquity')
  build_script_pl = file.path(package_dir, 'exec', 'build_system.pl')
}


# This is required here to make sure it's loaded before the callable library
require(deSolve)

# Basic commands:
# system("cp transient/r_ode_model.c  .")
# system("R CMD SHLIB r_ode_model.c")
# dyn.load(paste("r_ode_model", .Platform$dynlib.ext, sep = ""))
# This will check to see if the library is loaded:
# getLoadedDLLs()$r_ode_model[["dynamicLookup"]];
  
# turning off warnings so we don't get a lot of
# stuff from the system commands below
options(warn=-1)
cat(sprintf("#> Building the system: %s \n", system_file))
cat(sprintf("#> Distribution type:   %s \n", distribution))

build_command = sprintf("perl '%s' '%s' '%s' '%s' '%s'", build_script_pl, system_file, temp_directory, templates, distribution)
output = system(build_command, intern=TRUE)

# CFILE is used to indicate if we have compiled and loaded the CFILE successfully 
# We defalut to TRUE and then set it to false below if there are any problems encountered.
CFILE = TRUE

if(length(output) > 0){
  cat("#> Build reported errors and\n")
  cat("#> may have failed, see below:\n")
  for(line in output){
    cat(paste(line, "\n"))
  }
  rm('line')
} else{
  cat("#> Done \n")}

#
# Cleaning up any older versions of the C file
#
# if it's loaded we remove it from memory:
if(('r_ode_model' %in% names(getLoadedDLLs()))){
  dyn.unload(getLoadedDLLs()$r_ode_model[["path"]])}

# making the output directory to store generated information
if(!file.exists('output')){
  cat("#> Creating output directory \n")
  dir.create('output')
}

#next we remove any files to make sure we start from scratch
if(file.exists(file.path(temp_directory, paste("r_ode_model", .Platform$dynlib.ext, sep = "")))){
   file.remove(file.path(temp_directory, paste("r_ode_model", .Platform$dynlib.ext, sep = ""))) }
if(file.exists(file.path(temp_directory, "r_ode_model.o"))){
   file.remove(file.path(temp_directory, "r_ode_model.o")) }


# Now we compile the C file
cat("#> Compiling C version of system \n")
if(file.exists(file.path(temp_directory, 'r_ode_model.c'))){
  # storing the working directory and 
  # changing the working directory to the
  # temp directory
  mywd = getwd()
  setwd(temp_directory)
  # Compling the C file
  output =  system('R CMD SHLIB r_ode_model.c', intern=TRUE, ignore.stderr=!debug)
  if(debug == TRUE){
    cat(output)}
  if("status" %in% names(attributes(output))){
    cat("#> Failed: Unable to compile C file\n")
    CFILE = FALSE
  }else{
    # Loading the shared library
    cat("#> Loading the shared C library\n")
    dyn.load(file.path(temp_directory, paste("r_ode_model", .Platform$dynlib.ext, sep = "")))
  }
  # Returning to the working directory
  setwd(mywd)
}else{
  cat(sprintf("#> Failed: file %s%sr_ode_model.c not found \n",temp_directory, .Platform$file.sep))
  CFILE = FALSE
}

if(CFILE == FALSE){
  cat("#> C model not available compile manually using the\n") 
  cat("#> following command to debug:           \n") 
  cat(sprintf("#> system('R CMD SHLIB %s%sr_ode_model.c') \n", temp_directory, .Platform$file.sep))
  
  }


#source('transient/auto_rcomponents.r');
if(file.exists(file.path(temp_directory, "auto_rcomponents.r"))){
  source("transient/auto_rcomponents.r")
  cfg = system_fetch_cfg()
} else {
  cfg = list()
}

rm('output', CFILE)
# turning warnings back on
options(warn=0)
return(cfg)}

# system_fetch_parameters = system_load_data(cfg, dsname, data_file, data_sheet)
#
#'@export
#'@title Loading Datasets 
#'@description Loads datasets at the scripting level from the following
#'formats (based on the file extension)
#'\itemize{
#' \item csv - comma delimited 
#' \item tab - tab delimited
#' \item xls - excel spread sheet
#'}
#'
#' Multiple datasets can be loaded as long as they are given different
#' names. Datasets should be in a NONMEM-ish format with the
#' first row containing the column header names.
#'
#'@param cfg ubiquity system object    
#'@param dsname short name of the dataset to be used to link this dataset to different operations
#'@param data_file file name of the dataset
#'@param data_sheet argument identifying the name of the sheet in an excel file
#'
#'@return cfg - ubiquity system object    
#'
#'@examples
#' # cfg = system_load_data(cfg, 
#' #                        dsname     = "DSNAME", 
#' #                        data_file  = "data.xls", 
#' #                        data_sheet = "sheetname")
#' # cfg = system_load_data(cfg, 
#' #                        dsname     = "DSNAME", 
#' #                        data_file  = "data.csv")
#' # cfg = system_load_data(cfg, 
#' #                        dsname     = "DSNAME", 
#' #                        data_file  = "data.tab")
system_load_data <- function(cfg, dsname, data_file, data_sheet){


  # Reading the data based on the file extension
  if(regexpr(".xls$", as.character(data_file), ignore.case=TRUE) > 0){
    cfg$data[[dsname]]$values = as.data.frame(read.xls(data_file, sheet=data_sheet))
    cfg$data[[dsname]]$data_file$sheet  = data_sheet
  }

  if(regexpr(".csv$", as.character(data_file), ignore.case=TRUE) > 0){
    cfg$data[[dsname]]$values = read.csv(data_file, header=TRUE)
  }

  if(regexpr(".tab$", as.character(data_file), ignore.case=TRUE) > 0){
    cfg$data[[dsname]]$values = read.delim(data_file, header=TRUE)
  }
  cfg$data[[dsname]]$data_file$name  = data_file

  return(cfg)
}


#'@export
#'@title Selecting Parameter Sets
#'@description The system file can contain multiple parameterizations using
#' the \code{<PSET:?:?>?} notation. This function provides the means for
#' switching between these parameterizations, and (optionally) specifying a
#' subset of parameters estimated when performing parameter estimation. 
#'
#'@param cfg ubiquity system object    
#'@param set_name string containing the name of the parameter set
#'@param parameter_names list of parameter names to be estimated 
#'
#'@return cfg - ubiquity system object    
#'
#'@examples
#' # Examples
#' # cfg = system_select_set(cfg, ’default’)
#' # pnames = c('CL', 'Vp')
#' # cfg = system_select_set(cfg, ’myset', pnames)
system_select_set = function(cfg, set_name='default', parameter_names=NULL){
#
# takes the system information variable cfg and makes the values in the string
# 'set name'  the active values
#

# defining parameters for the current set
if(is.null(cfg$parameters$sets[[set_name]])){
  vp(cfg,sprintf('Could not find set: %s', set_name))
  vp(cfg,sprintf('Returning the default set instead'))
  set_name = 'default'
  cfg$parameters$matrix$value = cfg$parameters$sets$default$values;
  cfg$parameters$current_set  = 'default';
  }

  cfg$parameters$matrix$value  = cfg$parameters$sets[[set_name]]$values;
  cfg$parameters$current_set   = set_name;
  p_idx = 1;
  for(p_name in names(cfg$options$mi$parameters)){
    eval(parse(text=sprintf('cfg$parameters$values$%s = cfg$parameters$matrix$value[[p_idx]]', p_name)))
    p_idx = 1+p_idx
  }

  cfg$parameters$values = as.data.frame(cfg$parameters$values);

# checking to make sure the values specified in parameter_names are 
# actual parameters :)
if(!is.null(parameter_names)){
  # parameter names selected for estimation that do not exist
  mpn = setdiff(parameter_names, names(cfg$options$mi$parameters))
  if(length(mpn) > 0){
    parameter_names = NULL
    vp(cfg, sprintf('The following parameters were selected'))
    vp(cfg, sprintf('to be estimated but have not been defined:'))
    vp(cfg, sprintf('  %s', paste(mpn, collapse=',                ')))
    vp(cfg, sprintf('Check your spelling or create  this parameter '))
    vp(cfg, sprintf('in the system file using the <P> descriptor   '))
    vp(cfg, sprintf('Defaulting to _ALL_ parameters being estimated'))
  }

}

# if the parameter_names list is null we select them all for estimation
if(is.null(parameter_names)){
  parameter_names = names(cfg$options$mi$parameters)
}



tmp_to_estimate_system   = c()
tmp_to_estimate_variance = c() 

# ordering the parameters system and then variance
for(p_name in parameter_names){
  if(cfg$parameters$matrix[cfg$parameters$matrix$name == p_name, ]$ptype == "system"){
    tmp_to_estimate_system = c(tmp_to_estimate_system, p_name) }
  else{
    tmp_to_estimate_variance = c(tmp_to_estimate_variance, p_name) }
}

tmp_to_estimate_all = c(tmp_to_estimate_system, tmp_to_estimate_variance)


# setting objective function type:
if(length(tmp_to_estimate_variance) == 0){
  cfg$estimation$objective_type = 'wls' }
else{
  cfg$estimation$parameters$system = length(tmp_to_estimate_system);
  cfg$estimation$objective_type = 'ml' }


cfg$estimation$parameters$matrix = c()

#
# Storing the parameter information for estimation
# this is a reduced set of parameters (those that are being estimated)
p_idx = 1
# Initializing the guess list
cfg$estimation$parameters$guess = list()
cfg$estimation$mi               = list()
for(p_name in tmp_to_estimate_all){
  # matrix
  cfg$estimation$parameters$matrix = 
       rbind(cfg$estimation$parameters$matrix, cfg$parameters$matrix[cfg$parameters$matrix$name ==  p_name, ])
  # indices for mapping
  cfg$estimation$mi[[p_name]] = p_idx
  # vector of guesses
  eval(parse(text=sprintf('cfg$estimation$parameters$guess$%s = cfg$parameters$values$%s', p_name, p_name)))
  p_idx = p_idx + 1;
}

cfg$estimation$parameters$guess = unlist(as.data.frame(cfg$estimation$parameters$guess))


# defining covariates
for(cov_name in names(cfg$options$inputs$covariates)){
  # checking to see if the current covariate (cov_name) has a value specified
  # for the current parameter set (set_name). If it doesn't then the default
  # is used. If it does then these parameter set specific values are used
  if(is.null(cfg$options$inputs$covariates[[cov_name]]$parameter_sets[[set_name]])){
    cfg$options$inputs$covariates[[cov_name]]$times$values  = cfg$options$inputs$covariates[[cov_name]]$parameter_sets$default$times
    cfg$options$inputs$covariates[[cov_name]]$values$values = cfg$options$inputs$covariates[[cov_name]]$parameter_sets$default$values
  }
  else{
    cfg$options$inputs$covariates[[cov_name]]$times$values  = cfg$options$inputs$covariates[[cov_name]]$parameter_sets[[set_name]]$times
    cfg$options$inputs$covariates[[cov_name]]$values$values = cfg$options$inputs$covariates[[cov_name]]$parameter_sets[[set_name]]$values
  }
}


# defining the iivs
if(!is.null(cfg$iiv)){
  if(set_name %in% names(cfg$iiv$sets)){
    iiv_set_name = set_name }
  else{
    iiv_set_name = 'default' }
   
  # indices
  cfg$options$mi$iiv  = cfg$options$mi$iiv_sets[[iiv_set_name]]

  # iiv details
  cfg$iiv$current_set = iiv_set_name
  cfg$iiv$iivs        = cfg$iiv$sets[[iiv_set_name]]$iivs 
  cfg$iiv$parameters  = cfg$iiv$sets[[iiv_set_name]]$parameters
  cfg$iiv$values      = cfg$iiv$sets[[iiv_set_name]]$values
}

return(cfg)
}


# parameters = system_fetch_parameters(cfg)
#
#'@export
#'@title Get System Parameters
#'
#'@description
#' Fetch the parameters of the currently selected parameter set. To switch
#' between parameter sets use \code{\link{system_select_set}}
#'
#'@param cfg ubiquity system object    
#'
#'@return parameters - List of parameters for the selected parameter set or the default parameter set if the selected set does not exist
#'
#'@examples
#' # parameters = system_fetch_parameters(cfg)
system_fetch_parameters <- function(cfg){
  return(cfg$parameters$values)}


# cfg = system_fetch_iiv(cfg, IIV1, IIV2)
#
#
#
#
#'@export
#'@title Get Variability Terms
#'@description Extract elements of the current variance covariance matrix
#' specified in the system file with \code{<IIV:?:?> ?}, \code{<IIVCOR:?:?>?}, \code{<IIVSET:?:?> ?}, \code{<IIVCORSET:?:?>?}
#'
#'@param cfg ubiquity system object    
#'@param IIV1 row name of the variance/covariance matrix
#'@param IIV2 column name of the variance/covariance matrix element
#'
#'@return value - from the variance/covariance matrix   
#'@examples
#' #  val = system_fetch_iiv(cfg, IIV1="ETACL", IIV2="ETAVc")'
#'@seealso \code{\link{system_set_iiv}}
system_fetch_iiv <- function(cfg, IIV1, IIV2){
  
  VALUE =  -1
  if("iiv" %in% names(cfg)){
    
    IIV1_idx = match(c(IIV1), names(cfg$iiv$iivs))
    IIV2_idx = match(c(IIV2), names(cfg$iiv$iivs))
    
    
    if(is.na(IIV1_idx)){
      vp(cfg, sprintf("IIV %s not found", IIV1)) 
    }else if(is.na(IIV1_idx)){
      vp(cfg, sprintf("IIV %s not found", IIV2)) 
    }else{
      VALUE =  cfg$iiv$values[IIV1_idx, IIV2_idx]
    }
  } else {
    vp(cfg, "system_fetch_iiv() ")
    vp(cfg, "No IIV information was found") 
    vp(cfg, "These can be specified using: ") 
    vp(cfg, "<IIV:?>, <IIV:?:?>, and <IIVCOR:?:?> ")
  }
return(VALUE)}

#'@export
#'@title Zero All Model Inputs
#'@description Multiple default inputs can be specified in the system file. At
#' the scripting level this function can be used to eliminate all of them then
#' apply ony the subsequently specified inputs. 
#'
#'@param cfg ubiquity system object    
#'@param bolus boolean value indicating weather bolus inputs should be set to zero
#'@param rates boolean value indicating weather infusion rate inputs should be set to zero
#'
#'@return cfg - ubiquity system object    
#'
#'@examples
#' # Clear all inputs:
#' # cfg = system_zero_inputs()
#' # Clear only infusion rates
#' # cfg = system_zero_inputs(cfg, bolus=TRUE, rates=FALSE)
#'@seealso \code{\link{system_set_rate}}, \code{\link{system_set_bolus}}
system_zero_inputs <- function(cfg, bolus=TRUE, rates=TRUE){
  # zeroing out the bolus values
  if(bolus == TRUE){
    if('bolus' %in% names(cfg$options$inputs)){
      # first we add a dummy bolus at time 0
      cfg$options$inputs$bolus$times$values = c(0)
      # now we add a zero bolus for each species
      for(species in names(cfg$options$inputs$bolus$species)){
        cfg$options$inputs$bolus$species[[species]]$values = c(0)
      }
    }
  }
  
  # next we zero out all of the rate inputs as well
  if(rates == TRUE){
    for(rate    in  names(cfg$options$inputs$infusion_rates)){
      cfg$options$inputs$infusion_rates[[rate]]$times$values  = c(0)
      cfg$options$inputs$infusion_rates[[rate]]$levels$values = c(0)
    }
  }
return(cfg)}

# cfg = system_set_covariate(cfg, covariate, times, values)
#
#
#'@export
#'@title Set Covariate Values
#'@description Covariates specified in the system file using  \code{<CV:?>}
#' and \code{<CVSET:?:?>} will have their default values for a given parameter
#' set. This function is a means to overwrite those values.
#'
#'@param cfg ubiquity system object    
#'@param covariate name of the covariate
#'@param times list of times (system time units)
#'@param values corresponding list of values 
#'
#'@return cfg - ubiquity system object    
#'
#'@examples
#' cfg = system_set_covariate(cfg, 
#'                            covariate = "COV",
#'                            times     = c(1, 3, 25),
#'                            values    = c(1, 2,  1))
system_set_covariate <- function(cfg, covariate, times, values){
  isgood = TRUE
  if(!(length(times) == length(values)) ) {
    cat(" #-> The times and values have differnt lengths\n") 
    isgood = FALSE
    }
  if(!(covariate %in% names(cfg$options$inputs$covariates))){
    cat(sprintf(" #-> The covariate name %s could not be found\n", covariate)) 
    isgood = FALSE
  }
  if(isgood){
    cfg$options$inputs$covariates[[covariate]]$times$values  = times 
    cfg$options$inputs$covariates[[covariate]]$values$values = values
  } else {
    cat(sprintf(" #-> Something went wrong and the covariate, \n")) 
    cat(sprintf(" #-> was not set, see the messages above.\n")) }

return(cfg)}


# cfg = system_set_rate(cfg, rate, times, levels)
#
#
#
#'@export
#'@title Set Infusion Rate Inputs
#'@description Defines infusion rates specified in the system file using  \code{<R:?>}
#'
#'@param cfg ubiquity system object    
#'@param rate name of infusion rate    
#'@param times list of time values   
#'@param levels corresponding list of infusion values   
#'
#'@return cfg - ubiquity system object    
#'
#'@examples
#' # cfg = system_set_rate(cfg,
#' #            rate   = "RNAME",
#' #            times  = c(0, 1), 
#' #            levels = c(10, 0))
#' # Examples
#'@seealso \code{\link{system_zero_inputs}}
system_set_rate <- function(cfg, rate, times, levels){
  isgood = TRUE
  if(!(length(times) == length(levels)) ) {
    cat(" #-> The times and levels have differnt lengths\n") 
    isgood = FALSE
    }
  if(!(rate %in% names(cfg$options$inputs$infusion_rates))){
    cat(sprintf(" #-> The rate name %s could not be found\n", rate)) 
    isgood = FALSE
  }
  if(isgood){
    cfg$options$inputs$infusion_rates[[rate]]$times$values  = times 
    cfg$options$inputs$infusion_rates[[rate]]$levels$values = levels
  } else {
    cat(sprintf(" #-> Something went wrong and the rate, \n")) 
    cat(sprintf(" #-> was not set, see the messages above.\n")) }
return(cfg)}

# cfg = system_set_option(cfg, group, option, value)
#
#
#'@export
#'@title Setting Analysis Options
#'@description Different options associated performing analyses (e.g running
#' simulations, performing parameter esitmations, logging, etc.) can be set
#' with this function
#'
#'@param cfg ubiquity system object    
#'@param group options are grouped together by the underlying activity being performed: "solver", "stochastic", "simulation", "estimation", "logging", or "titration"
#'@param option for each group there are a set of options 
#'@param value corresponding value for the option 
#'
#'@return cfg - ubiquity system object    
#'
#'@details 
#'
#' \bold{\code{group=solver}}
#'
#' Depending on the solver, different options can be set. The documentation
#' for  \code{\link[deSolve]{deSolve}} lists the different solvers. For a full list of options, see the
#' documentation for the specific solver (e.g. \code{?lsoda}). Some common options
#' to consider are:
#' \itemize{
#' \item \code{"atol"} - Relative error tolerance
#' \item \code{"rtol"} - Absolute error tolerance
#' \item \code{"hmin"} - Minimum integration step size
#' \item \code{"hmax"} - Maximum integration step size
#' }
#' To select the \code{vode} solver and set the maximum step size to 0.01, the
#' following would be used:
#' \preformatted{
#' cfg=system_set_option(cfg,
#'                       group  = "simulation",
#'                       option = "solver", 
#'                       value  = "vode")
#'
#' cfg=system_set_option(cfg,
#'                       group  = "solver",
#'                       option = "hmax", 
#'                       value  = 0.01)
#' }
#'
#'
#' \bold{\code{group="simulation"}}
#'\itemize{
#' \item \code{"include_important_output_times"} - Automatically add bolus, infusion rate switching times, etc: \code{"yes"}(default), \code{"no"}.
#' \item \code{"integrate_with"} - Specify if the ODE solver should use the Rscript (\code{"r-file"}) or compiled C (\code{"c-file"}), if the build process can compile and load the C version it will be the default otherwise it will switch over to the R script.
#' \item \code{"output_times"} - Vector of times to evaulate the simulation (default \code{seq(0,100,1)}).
#' \item \code{"solver"} - Selects the ODE solver: \code{"lsoda"} (default), \code{"lsode"}, \code{"vode"}, etc.; see the documentation for \code{\link[deSolve]{deSolve}} for an exhaustive list.
#' }
#'
#' \bold{\code{group="stochastic"}}
#'
#' When running stochastic simulations (inter-individual variability applied to system
#' parameters) it can be useful to specify the following:
#' \itemize{
#'  \item\code{"ci"} - Confidence interval (default \code{95})
#'  \item\code{"nsub"} - Number of subjects (default \code{100})
#'  \item\code{"seed"} - Seed for the random numebr generator (default \code{8675309})
#'  \item\code{"ponly"} - Only generate the subject parameters but do not run the simulations (default \code{FALSE})
#'  \item\code{"outputs"} - A list of the predicted outputs to include (default all outputs defined by \code{<O>})
#'  \item\code{"states"} - A list of the predicted states to include(default all states)
#'  \item\code{"sub_file"} - Name of data set loaded with (\code{\link{system_load_dataset}}) containing subject level parameters and coviariates
#'  \item\code{"sub_sample"} - Controls how subjects are sampled from the dataset
#'  }
#'
#' If you wanted to generate \code{1000} subjects but only wanted the parameters, you would
#' use the following:
#' \preformatted{
#' cfg = system_set_option(cfg,
#'                         group  = "stochastic", 
#'                         option = "nsub ",
#'                         value  = 1000)
#'
#' cfg = system_set_option(cfg,
#'                         group  = "stochastic", 
#'                         option = "ponly",
#'                         value  = TRUE )
#' }
#'
#'
#' If you wanted to exclude states and only include the output \code{Cp_nM}, you would do
#' the following:
#' \preformatted{
#' cfg = system_set_option (cfg, 
#'                          group  = "stochastic",
#'                          option = "states",
#'                          value  = list())
#'
#' cfg = system_set_option (cfg, 
#'                          group  = "stochastic",
#'                          option = "outputs",
#'                          value  = c("Cp_nM")) 
#' }
#'
#' To pull subject information from a data file instead of generating the subject
#' parameters from IIV information the \code{sub_file} option can be used. The value here
#' \code{SUBFILE_NAME} is the name given to a dataset loaded with
#' (\code{\link{system_load_dataset}}):
#'
#' \preformatted{
#' cfg=system_set_option(cfg, 
#'                       group  = "stochastic",
#'                       option = "sub_file",
#'                       value  = "SUBFILE_NAME")
#' }
#'  
#' Sampling from the dataset can be controlled using the \code{sub_file_sample} option:
#'  
#' \preformatted{
#' cfg=system_set_option(cfg, 
#'                       group  = "stochastic",
#'                       option = "sub_file_sample",
#'                       value  = "with replacement")
#' }
#'  
#' Sampling can be done sequentially (\code{"sequential"}), with replacement
#' (\code{"with replacement"}), or without replacement (\code{"without replacement"})
#'
#' \bold{\code{group="estimation"}}
#'
#' The default estimation in R is performed using either the \code{optim} or \code{optimx} libraries.
#' This is selected by setting the \code{optimizer} option:
#'  
#' \preformatted{
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "optimizer",
#'                         value  = "optim")
#' }
#'  
#' The optimization routine then specified using the \code{method}. By default this \code{option} is
#' set to \code{Nelder-Mead}.
#'  
#' \preformatted{
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "method",
#'                         value  = "Nelder-Mead")
#' }
#'  
#' And different attributes are then selected using the control.
#'  
#' \preformatted{
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "control",
#'                         value  = list(trace  = TRUE,
#'                                       maxit  = 500,
#'                                       REPORT = 10))
#' }
#' 
#' For the different methods and control options, see the documentation for the \code{optim}
#' and \code{optimx} libraries.
#'
#' To perform a global optimization you can install either the particle swarm (\code{\link[pso]{pso}})
#' genetic algorithm (\code{\link[GA]{GA}}) libraries.
#' To use the particle swarm set the \code{optimizer} and \code{method}:
#'  
#' \preformatted{
#' library("pso")
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "optimizer",
#'                         value  = "pso")
#' 
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "method",
#'                         value  = "psoptim")
#' }
#' 
#' The control option is a list described \code{\link[psoptim]{psoptim}} documentation.
#'
#' To use the genetic algorithm set the optimizer and method:
#' 
#' \preformatted{
#' library("GA")
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "optimizer",
#'                         value  = "ga")
#' 
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "method",
#'                         value  = "ga")
#' }
#' 
#' The control option is a list and the list elements are the named options in the GA
#' documentation. Use the following as an example:
#' 
#' \preformatted{
#' cfg = system_set_option(cfg, 
#'                         group  = "estimation",
#'                         option = "control",
#'                         value  = list(maxiter  = 10000,
#'                                      optimArgs = list(
#'                                        method  = "Nelder-Mead",
#'                                        maxiter = 1000)))
#' }
#' 
#' To alter initial guesses see: \code{\link{system_set_guess}}
#'
#' \bold{\code{group="titration"}}
#'
#' \code{"titrate"} - By default titration is disable (set to \code{FALSE}). If you are
#' going to use titration, enable it here by setting this option to \code{TRUE}.
#' This will force #' \code{\link{simulate_subjects}} to use 
#' \code{\link{run_simulation_titration}} internally when running simulations.
system_set_option <- function(cfg, group, option, value){
 
  groups = c('solver', 'stochastic', 'simulation', 'estimation', 'logging', 'titration')
  
  errormsg = '';
  # checking the user input
  isgood = TRUE
  if(group %in% groups){
    # setting stochastic options
    if(group == 'stochastic'){

      if((option == "states") | (option == "outputs")){
        for(val in value){
          if(!(val %in% names(cfg$options$mi[[option]]))){
            errormsg = sprintf(' #-> %s %s \n', errormsg, val)
            isgood = FALSE
          }
        } 
      }

      # Making sure the specified dataset is loaded
      if(option == "sub_file"){
        if(!(value %in% names(cfg$data))){
          errormsg =  sprintf('%s #-> Error: dataset >%s< not found, please load first \n', errormsg, value)
          errormsg =  sprintf('%s #-> using system_load_dataset() \n', errormsg)
          isgood = FALSE
        }
      }
      if(option == "sub_file_sample"){
        if(!any(value == c("with replacement", "sequential", "without replacement"))){
          errormsg =  sprintf('%s #-> The value  %s is invalid and must be one of the following \n', errormsg, toString(value))
          errormsg =  sprintf('%s #->   sequential          - sample from data file sequentially \n', errormsg)
          errormsg =  sprintf('%s #->   with replacement    - sample from data file with replacement \n', errormsg)
          errormsg =  sprintf('%s #->   without replacement - sample from data file with out replacement \n', errormsg)
          isgood = FALSE
        }
      }


      if(isgood){
        cfg$options$stochastic[[option]] = value
      }else{
       errormsg = sprintf(' #-> The following %s are not valid\n%s', option, errormsg)
      }
      
      
      }
    
    # setting simulation options
    if(group == "simulation"){
      cfg$options$simulation_options[[option]] = value}


    # titration options
    if(group == 'titration'){
      if(option == "titrate")
        if(is.logical(value)){
           cfg$titration$titrate = value
        }
        else{
           errormsg = sprintf('%s #-> The titrate option should be TRUE or FALSE\n', errormsg)
           isgood = FALSE
        
        }
      }
      
    
    # setting solver options
    if(group == 'solver'){
      cfg$options$simulation_options$solver_opts[[option]] = value}

    # setting logging options
    if(group == 'logging'){
      cfg$options$logging[[option]] = value}
    

    # setting estimation options
    if(group == 'estimation'){
      cfg$estimation$options[[option]] = value}
    
  } else {
    # flagging a bad group
    isgood = FALSE
    errormsg = sprintf("%s #-> The specified group (%s) is invalid\n", errormsg, group)
    errormsg = sprintf("%s #-> Valid groups are:  \n", errormsg)
    for(valid in groups){
      errormsg = sprintf("%s #->   ->%s\n", errormsg, valid) }
  }
  
  
  # If the error flag has been switched above, then we print some inforamtion for the user
  if(!isgood){
    cat(sprintf("#> ------------------------------------\n")) 
    cat(sprintf("#> system_set_option()                 \n")) 
    cat(sprintf("#> Something went wrong and the option,\n")) 
    cat(sprintf("#> was not set:\n")) 
    cat(errormsg)
    cat(sprintf("#> ------------------------------------\n")) 
    }
  
return(cfg)}

#'@export
#'@title Titration Rules
#'@description Defines a new titration rule and the times when that rule is evaluated
#'
#'@param cfg ubiquity system object    
#'@param name name for the titration rule
#'@param times list of times when the rule will be evaluated 
#'@param timesscale time scale associated with the titraiton times (as defined by \code{<TS:?>})
#'
#'@return cfg - ubiquity system object    
#'
#'@details
#' \preformatted{
#' cfg = system_new_tt_rule(cfg,
#'                          name      = "rname",
#'                          times     = c(0, 2, 4),
#'                          timescale = "weeks")'
#' }
#' A titration rule identifies a set of times (\code{times}) and an associated time
#' scale (\code{timescale}) in which titration events can potentially occur. Any
#' times scale, as defined in the system file with \code{<TS:?>}, can be used in
#' place of "weeks" above. The \code{name}, \code{"rname"} above, is used to link the
#' titration rule to different conditions discussed below. The name should be
#' a string beginning with a letter, and it can contain any combination of
#' numbers, letters, and underscores. With the rule created we can then add conditions to that rule.'
#'
#'@seealso \code{\link{system_set_tt_cond}} \code{\link{run_simulation_titration}}
system_new_tt_rule <- function(cfg, name, times, timescale){

  isgood = TRUE
  # empty list holding the new titration inforamtion

  if(!timescale %in% names(cfg$options$time_scales)){
    isgood = FALSE
    errormsg = sprintf('The timescale: "%s" was not defined', timescale)
  }

  # checking the timescale to make sure it's been defined
  if(isgood){
  titrate = list()
  # storing the times and timescale
  titrate$times     = times
  titrate$timescale = timescale
  # converting those times to simtimes
  titrate$simtimes  = system_ts_to_simtime(cfg, times, timescale)

  # Storing the titration information in cfg
  cfg$titration$rules[[name]] = titrate
  }

  if(!isgood){
    vp(cfg, "------------------------------------") 
    vp(cfg, "system_new_tt_rule()                ") 
    vp(cfg, "Something went wrong and the        ") 
    vp(cfg, "titration rule was not set          ") 
    vp(cfg, errormsg)
    vp(cfg, "------------------------------------")
    }
return(cfg)
}


#'@export
#'@title title
#' Description 
#'
#'@param cfg ubiquity system object    
#'@param name name for the titration rule
#'@param cond boolean expression that evaluates as \code{TRUE} when the action should be triggered
#'@param action what should be done (changing the dose, state change, etc) 
#'@param value code to be stored in the titration history to track when this condition has been triggered
#'
#'@return cfg - ubiquity system object    
#'
#'
#'@details
#' \preformatted{
#' cfg = system_new_tt_cond(cfg,
#'                          name = "rname",
#'                          cond = "BOOLEAN EXPRESSION",
#'                          action = "EXPRESSION",
#'                          value = "VALUE")
#'}
#'
#' Next we can define a set of conditions and corresponding actions. The \code{name}
#' input will associate this condition with a previously defined rule. For each
#' time defined when the rule was created, the condition (\code{cond}) will be
#' evaluated. If that condition evaluates as true then the \code{action} will be
#' evaluated. Lastly, when a condition action is evaluated, the \code{value} is stored
#' in the titration history.
#'
#' Multiple conditions can be associated with a rule. The internal titration
#' history will track each one where a condition has been evaluated as true, but
#' the simulation output will only show the \bold{last} condition to be evaluated as
#' true.
#'
#' The \code{cond} field is a string that, when evaluated, will produce a boolean value
#' (\code{TRUE} or \code{FALSE}). If you simply want to force an action at each of the times
#' for a given rule you can use: \code{cond = "TRUE"}. Alternatively you can provide
#' mathematical expressions.
#'
#' The \code{action} field is evaluated when \code{cond} is true. To modify how a simulation
#' is going to be performed, you will want to modify the \code{SIMINT_cfgtt}
#' variable using the different system commands. Certain common tasks have
#' prototype functions created to make it easier for the user:
#' \itemize{
#' \item \code{SI_TT_BOLUS} - Set bolus dosing
#' \item \code{SI_TT_RATE} - Set infusion inputs
#' \item \code{SI_TT_STATE} - Reset system states
#' }
#'
#' \bold{\code{SI_TT_BOLUS}}
#'
#' The simplest way to apply a bolus when the condition is true is to use the following:
#'
#' \preformatted{
#' action = "SI_TT_BOLUS[state=’At’, 
#'                       values=c(10, 10, 10), 
#'                       times=c(0, 1, 2)]"
#' }
#'
#' The \code{values} and \code{times} are vectors of numbers of equal length. The dosing and
#' time units are those specified in the \code{system.txt} file for the \code{<B:?>} delimiter. The
#' times are relative to the titration time. So \code{0} above means at the titration time.
#'
#' It’s possible to specify an interval and a number of times to repeat the last dose
#' using the following:
#'
#' \preformatted{
#' action = "SI_TT_BOLUS[state    = ’At’, 
#'                       values   = c(5, 5, 10), 
#'                       times    = c(0, 2, 4), 
#'                       repdose  = ’last’, 
#'                       number   = 7, 
#'                       interval = 4]"
#' }
#'
#' This will give a dose of \code{5} at the titration point and \code{2} time units later. The dose of \code{10}
#' at time \code{4} will be repeated \code{7} times every \code{4} time units. So a total of 8 (\code{7 + 1}) doses
#' at \code{10} will be administered. Remember the time units were those defined in \code{<B:?>}.
#' The input \code{repdose} can be either \code{’last’} or \code{’none’}.
#'
#' \bold{Note:} The main string is in double quotes \code{" "} but the strings in the protype
#' argument (e.g. \code{’last’}) are in single quotes \code{’ ’}.
#'
#' \bold{\code{SI_TT_RATE}} 
#'
#' If you created an infusion named \code{Dinf} using \code{<R:?>} and the infusion units
#' are min (times) and mg/min (rates). To have a 60 minute infusion of 20
#' mg/min then we would do the following:
#'
#' \preformatted{
#' action = "SI_TT_RATE[rate=’Dinf’, times=c(0, 60), levels=c(20.0, 0)]"
#' }
#'
#' If we wanted to do this every day for 9 more days (a total of 10 days) we can repeat
#' the sequence:
#'
#' \preformatted{
#' action = "SI_TT_RATE[rate     = ’Dinf’, 
#'                      times    = c(0, 60), 
#'                      levels   = c(20, 0), 
#'                      repdose  = ’sequence’, 
#'                      number   = 9, 
#'                      interval = 24*60]"
#' }
#'
#' The input \code{repdose} can be either \code{’sequence’} or \code{’none’}.
#'
#' \bold{Note:} The time units and dosing rate are those specified using \code{<R:?>}.
#'
#' \bold{\code{SI_TT_STATE}} 
#'
#' To provide fine control over states at titration points the state reset
#' prototype is provided. For example, if you are modeling an assay where
#' there is a wash step and you want to drop a concentration to zero. If you
#' have a state named \code{Cc} defined in your \code{system.txt} and you want to set
#' it to \code{0.0} in a condition the following action would work.
#'
#' \preformatted{
#' action = "SI_TT_STATE[Cc][0.0]"
#' }
#'
#' The value here is a number but you can use any mathematical
#' combination of variables available in the titration environment. Also you
#' can create your own user function and place the function call within the
#' brackets above.
#'
#'@examples
#' # Examples
#'@seealso \code{\link{system_new_tt_rule}} \code{\link{run_simulation_titration}}
system_set_tt_cond <- function(cfg, name, cond, action, value='-1'){

  isgood = TRUE


  if(!(name %in% names(cfg$titration$rules))){
    errormsg = sprintf('The rule "%s" was not found, first create the rule using system_new_tt_rule then add conditions', name)
    isgood = FALSE
  }


  action_parsed = action
  value_parsed = value

  # creating an empty condition
  if(isgood){
    tc = list()
    tc$cond          = cond
    tc$action        = action
    tc$value         = value

    
    # parsing the action
    action_parsed = parse_patterns(cfg, action)


    tc$action_parsed = action_parsed
    tc$value_parsed  = value_parsed
    # adding the condition to the list of conditions for the current rule
    if(is.null(cfg$titration$rules[[name]]$conditions)){
      cname = 'c1'
    } else {
      cname = sprintf('c%d', (length(names(cfg$titration$rules[[name]]$conditions))+1)) }
    cfg$titration$rules[[name]]$conditions[[cname]] = c(tc)

  }

  if(!isgood){
    vp(cfg, "------------------------------------") 
    vp(cfg, "system_set_tt_cond()                ") 
    vp(cfg, "Something went wrong and the        ") 
    vp(cfg, "titration condition was not set     ") 
    vp(cfg, errormsg)
    vp(cfg, "------------------------------------")
    }


return(cfg)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
parse_patterns  <- function(cfg, str){

  patterns = list()

  # newstr will have the string with the substitutions
  newstr = str

  # List of the possible patterns
  patterns$bolus$pattern = 'SI_TT_BOLUS['
  patterns$bolus$replace = 'SIMINT_cfgtt = system_set_tt_bolus(cfg=SIMINT_cfgtt, SIMINT_ARG_1,  tt_ts=SIMINT_tt_ts,  tsinfo=SIMINT_scales)'
  patterns$bolus$narg    = 1;

  patterns$rate$pattern  = 'SI_TT_RATE['
  patterns$rate$replace  = 'SIMINT_cfgtt = system_set_tt_rate(cfg=SIMINT_cfgtt, SIMINT_ARG_1,  tt_ts=SIMINT_tt_ts,  tsinfo=SIMINT_scales)'
  patterns$rate$narg     = 1;

  patterns$state$pattern = 'SI_TT_STATE['
  patterns$state$replace = 'SIMINT_ARG_1 = SIMINT_ARG_2; SIMINT_IC[["SIMINT_ARG_1"]] = SIMINT_ARG_2'
  patterns$state$narg    = 2;


  # We loop through each pattern and see if it's in the string
  # if it's in the string we replace it over and over again 
  # until we get them all
  for(pname in names(patterns)){


    found_pname = FALSE
    found_error = FALSE

    # if we find the pattern for pname in the string
    # we indicate using the found variable and set the 
    # error counter to 1
    if(grepl(patterns[[pname]]$pattern, newstr, fixed=TRUE)){
      error_cntr  = 1
      errormsg    = 'None'
      found_pname = TRUE }
   
   
    while(found_pname){
    
      # attempting to replace the first instance of the pattern
      # storing the parse results in pr
      pr = find_bracketed_arguments(str     = newstr,
                                    pattern = patterns[[pname]]$pattern,
                                    replace = patterns[[pname]]$replace,
                                    narg    = patterns[[pname]]$narg)

      # if the parsing was successful
      # we store the new_string list element in newstr
      if(pr$isgood){
        newstr = pr$new_string
      }
      else{
        errormsg = pr$errormsg
        found_pname = FALSE
        found_error = TRUE 
      }

      
      # if the new string (after successive replacements) no longer has the
      # pattern we stop
      if(!grepl(patterns[[pname]]$pattern, newstr, fixed=TRUE)){
        found_pname = FALSE }
   
      if(error_cntr >= 100){
        found_pname = FALSE
        found_error = TRUE
        errormsg    = 'Exceeded the maximum number of maxes (100), stuck in a loop?'
      
      }
    
     # incrementing the error counter
     error_cntr = error_cntr + 1
    }
   
    if(found_error){
      vp(cfg, 'Error parsing patterns')
      vp(cfg, sprintf('String:       %s', str))
      vp(cfg, sprintf('Pattern name: %s', pname))
      vp(cfg, sprintf('Pattern:      %s', patterns[[pname]]$pattern))
      vp(cfg, sprintf('Error Message:%s', errormsg))
    
    }
  
  }

 return(newstr)
}

#
# parses strings to find abstract functions SIFUNC[ and extract the arguments
# from that function and replace it with actual functions and any additional
# arguments needed
#
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
find_bracketed_arguments <- function(str, pattern, replace = '', narg, op = '[', cp=']'){

  # getting the length of the string
  strlen = nchar(str)

  isgood     = TRUE 
  errormsg   =  ''
  new_string = ''
  blank_str  = strrep(' ', strlen)

  # finding the pattern position
  ppos = regexec(pattern, str, fixed=TRUE)


  # checking to see if the pattern is in str
  if(ppos >0){
    pstart    = ppos[[1]][1]
    arg_start = c(attr(ppos[[1]], "match.length")) + pstart -1
    arg_stop  = c()
   
    # counter used to keep track of excess brackets
    excess_p  = 0
   
    procstr = TRUE
    strpos   = arg_start[1] + 1
   
    while(procstr){
   
      # pulling out the current character
      strele = substr(str, strpos, strpos)
   
   
      if((length(arg_start) == length(arg_stop))& (strele == op)){
        arg_start = c(arg_start, strpos) 
        exess_p = 0
      }
      else if(strele == op){
        excess_p = excess_p + 1
      }
      # if we find a closing parenthesis and
      # excess is zero then we've found an 
      # end to the argument
      else if((strele == cp) & (excess_p == 0)){
        arg_stop  = c(arg_stop, strpos) 
      }
      else if(strele==cp){
        excess_p = excess_p - 1
      } 
   
      # if we get to the end of the string
      # then we stop processing it
      if(strpos >= strlen){
        procstr = FALSE
      }
      # if we found matching braces for the number 
      # of arguments then we stop
      if(length(arg_stop) == narg){
        procstr = FALSE }
   
      # incrementing the string position
      strpos = strpos+1
    } 
   
   
    # Checking to make sure we found the same number of start/stop options
    if(length(arg_start) == length(arg_stop)){

      if(narg == length(arg_start)){
        # extracting arguments from the string
        ext_args = c()
        for(idx in 1:length(arg_start)){
           # SIMINT_ARG_1 SIMINT_ARG_2
           newarg = substr(str, arg_start[idx] + 1, arg_stop[idx] - 1)
           replace = gsub(sprintf('SIMINT_ARG_%d', idx), newarg, replace, fixed=TRUE)
           ext_args = c(ext_args, newarg)
        }
       
        new_string = 
        sprintf('%s%s%s', 
                 substr(str, 1,pstart-1),                           # from the beginning until jsut before the function starts
                 replace,                                           # new stuff in the middle
                 substr(str, arg_stop[length(arg_stop)]+1, strlen)) # Just after the function starts to the end
      } else{
       isgood = FALSE
       errormsg = sprintf("Number of arguments specified (%d) different from number found (%d)", narg, length(arg_stop))
     }
   }
   else{
     isgood = FALSE
     errormsg = sprintf("Start indicators (%d) different from stop indicators(%d)", length(arg_start), length(arg_stop))
   }
   
   # Creating a blank_string with markers where 
   # the ID'd positions are in the original string
   if(isgood){
     substring(blank_str, pstart, pstart) = 'S'
     for(idx in 1:length(arg_start)){
      substring(blank_str, arg_start[idx], arg_start[idx]) = toString(idx)
      substring(blank_str, arg_stop[idx],  arg_stop[idx])  = toString(idx)
     
     }
   }
  
  } else{
    isgood = FALSE
    errormsg = sprintf("unable to find patter: '%s' in string", pattern)
  }
  
  finfo = list()
  finfo$isgood     = isgood
  finfo$errormsg   = errormsg
  finfo$str        = str         # original string
  finfo$new_string = new_string  # string with replacement 
  finfo$blank_str  = blank_str   # string with position markers


return(finfo)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_set_tt_bolus <- function(cfg, state, values, times, tt_ts,  tsinfo, repdose="none", interval=1, number=0){

# calculating the bolus time in bolus doing units 
# As specified by <B:times> ..
# 
# state    - Dosing state/compartment (Defind in <B:events>)
# values   - list of dosing amounts (in dosing unites defined by <B:events>)
# times    - list of dosing times relative to the current titration time (in # time units defiend by <B:times>)
# repdose  - "none", "last", "all"
# interval - interval to repeat in the units defined in <B:times>
# number   - number of times to repeat 
#

offset = tt_ts$time/tsinfo$bolus

if(repdose == "none"){
  bolus_times  = offset+times 
  bolus_values = values
  }
else if(repdose == "last"){
  bolus_times  = offset+c(times, 1:number*interval) 
  bolus_values = c(values, rep(x=values[length(values)], times=number))
  }


cfg = system_set_bolus(cfg    = cfg,
                       state  = state,
                       times  = bolus_times,
                       values = bolus_values)
return(cfg)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_set_tt_rate <- function(cfg, rate, times, levels, tt_ts, tsinfo, repdose="none", interval=1, number=0){


# calculating the offset based on the current titration time
#
#  Titration time (in simulation units)
#  ------------------------------------------------- = titration time in rate units
#     Rate time scale (simulation units/rate units)
#

offset = tt_ts$time/tsinfo$infusion_rates[[rate]]

if(repdose == "sequence"){

  rate_times  = c()
  rate_levels = c()
  start_times = seq(0,number)

  for(tidx in start_times){
     rate_times  = c(rate_times, (times+offset+ interval*tidx))
     rate_levels = c(rate_levels, levels)
    }

  } 
else {
  rate_times  = times + offset
  rate_levels = levels

  }


cfg = system_set_rate(cfg    = cfg,
                      rate   = rate,
                      times  = rate_times,
                      levels = rate_levels)

return(cfg)
}

# cfg = system_set_bolus(cfg, state, times, values)
#
#'@export
#'@title Set Bolus Inputs
#'@description Defines infusion rates specified in the system file using  \code{<B:times>} and   \code{<B:events>} 
#'
#'@param cfg ubiquity system object    
#'@param state name of the state to apply the bolus
#'@param times list of injection times 
#'@param values corresponding list injection values     
#'
#'@return cfg - ubiquity system object    
#'
#'@examples
#' # cfg = system_set_bolus(cfg,
#' #            state  = "SNAME",
#' #            times  = c(0, 1), 
#' #            values = c(10, 0))
#' # Examples
#'@seealso \code{\link{system_zero_inputs}}
system_set_bolus <- function(cfg, state, times, values){
  
  errormsg = '';
  # checking the user input
  isgood = TRUE
  if(!(length(times) == length(values))){
    errormsg = sprintf("%s#> The times and values have differnt lengths\n", errormsg)
    cat("#> \n") 
    isgood = FALSE
    }
  if(!(state %in% names(cfg$options$inputs$bolus$species))){
    errormsg = sprintf("%s#> The state %s could not be found\n", errormsg, state)
    isgood = FALSE
  }
  
  if(isgood){
    bolus_old = cfg$options$inputs$bolus;
    # getting all of the times both previous and those in the 
    # current state being specified
    all_times = unique(sort(c(bolus_old$times$values, times)))
    
    # looping through the species and figuring out which times we need to keep
    all_times_keep = c();
    for(current_time in all_times){
      keep_time = FALSE
      for(species in names(cfg$options$inputs$bolus$species)){
        # if the speceis is the one being updated then we 
        # look and see if the current time is in the list 
        # of times to be updated
        if(species == state){
          if(!is.na(match(current_time, times))){
            keep_time = TRUE
          }
        # Otherwise this is a different species. So we see if the time
        # is in the bolus_old list. If it is, we see if this species 
        # has a non-zero value
        } else{ 
          if(!is.na(match(current_time, bolus_old$times$values))){
            # pulling out the index in bolus_old that corresponds to this time
            time_index = match(current_time, bolus_old$times$values)
            if(bolus_old$species[[species]]$values[[time_index]] > 0){
              keep_time = TRUE
            }
          }
        }
      }
      # keep_time should be true if there is a value specified in the current
      # state being udpated or if there is a non-zero value in the other
      # states. We then add this to all_times_keep:
      if(keep_time == TRUE){
        all_times_keep = c(all_times_keep, current_time)
      }
    }
    
    # 
    # zeroing out the bolus information for the species
    # 
    for(species in names(cfg$options$inputs$bolus$species)){
      cfg$options$inputs$bolus$species[[species]]$values = c()
    }
    
    #
    # Now building the bolus list based on 
    # all_times_keep and the values specified above
    #
    for(current_time in all_times_keep){
      for(species in names(cfg$options$inputs$bolus$species)){
        # default value of dose set to zer0
        species_value = 0
        # then we check to see if it's nonzero and overwrite accordingly
        if(species == state){
          if(!is.na(match(current_time, times))){
             time_index = match(current_time, times) 
             species_value = values[time_index]
          }
        }
        else{
          if(!is.na(match(current_time, bolus_old$times$values))){
            time_index = match(current_time, bolus_old$times$values) 
            species_value = bolus_old$species[[species]]$values[[time_index]]
          }
        }
        
        # storing the bolus value for the specific species
        cfg$options$inputs$bolus$species[[species]]$values = 
          c(cfg$options$inputs$bolus$species[[species]]$values, species_value)
      }
    }
    cfg$options$inputs$bolus$times$values = all_times_keep

  } else {
    vp(cfg, sprintf("------------------------------------")) 
    vp(cfg, sprintf("system_set_bolus()                  ")) 
    vp(cfg, sprintf("Something went wrong and the bolus, ")) 
    vp(cfg, sprintf("was not set:")) 
    cat(    errormsg)
    vp(cfg, sprintf("------------------------------------")) 
    
    }

return(cfg)}

# cfg = system_set_iiv(cfg, IIV1, IIV2, VALUE)
#
#
#'@export
#'@title Set variability terms
#'@description Set elements of the current variance covariance matrix
#' specified in the system file with \code{<IIV:?:?> ?}, \code{<IIVCOR:?:?>?}, \code{<IIVSET:?:?> ?}, \code{<IIVCORSET:?:?>?}
#'
#'@param cfg ubiquity system object    
#'@param IIV1 row name of the variance/covariance matrix
#'@param IIV2 column name of the variance/covariance matrix element
#'@param VALUE value to assign to the variance/covariance matrix element
#'
#'@return cfg - ubiquity system object    
#'@examples
#' # cfg = system_set_iiv(cfg,
#' #                      IIV1 = "ETACL",
#' #                      IIV2 = "ETAVc",
#' #                      VALUE=0.03)
#'@seealso \code{\link{system_fetch_iiv}}
system_set_iiv <- function(cfg, IIV1, IIV2, VALUE){
  if("iiv" %in% names(cfg)){
    IIV1_idx = match(c(IIV1), names(cfg$iiv$iivs))
    IIV2_idx = match(c(IIV2), names(cfg$iiv$iivs))
    
    if(is.na(IIV1_idx)){
      cat(sprintf(" #-> IIV %s not found \n", IIV1)) 
    }else if(is.na(IIV1_idx)){
      cat(sprintf(" #-> IIV %s not found \n", IIV2)) 
    }else{
      cfg$iiv$values[IIV1_idx, IIV2_idx] = VALUE
      cfg$iiv$values[IIV2_idx, IIV1_idx] = VALUE
    }
  } else {
    vp(cfg, "system_set_iiv()")
    vp(cfg, "No IIV information was found") 
    vp(cfg, "These can be specified using: ") 
    vp(cfg, "<IIV:?>, <IIV:?:?>, and <IIVCOR:?:?> ")
  }
return(cfg)}

# The following are implementations of the tic and toc commands from matlab.
# The original source is here:
# http://stackoverflow.com/questions/1716012/stopwatch-function-in-r
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)

  return(toc-tic)
} 

# system_view(cfg,field="all") 
# field can be parameters, bolus, rate, covariate, iiv, datasets
#
#
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_view <- function(cfg,field="all") {
  
  msg = ''
  
  # Processing infusion rate information
  if(field == "all" | field== "parameters"){
      msg = sprintf("%s #-> Parmaeter Information\n", msg)
      msg = sprintf("%s #-> Parameter set selected: \n", msg)
      msg = sprintf("%s #->   %s  \n", msg, cfg$parameters$current_set)
      msg = sprintf("%s #->   %s  \n", msg,  cfg$parameters$sets[[cfg$parameters$current_set]]$name)
      msg = sprintf("%s #-> Default parameters for current set:  \n", msg )
      msg = sprintf("%s #->%s |  %s | %s \n", msg,
                  pad_string('name', 18), 
                  pad_string('value', 12), 
                  pad_string('units', 15))
      msg = sprintf("%s #->%s \n", msg, paste(replicate(52, "-"), collapse = ""))
      for(pidx in 1:length(cfg$parameters$matrix$name)){
      msg = sprintf("%s #->%s |  %s | %s \n", msg,
                  pad_string(as.character(cfg$parameters$matrix$name[pidx]), 18), 
                  var2string(cfg$parameters$matrix$value[pidx], 12), 
                  pad_string(as.character(cfg$parameters$matrix$units[pidx]), 15))
      }
      msg = sprintf("%s #->%s \n", msg, paste(replicate(52, "-"), collapse = ""))
    
      msg = sprintf("%s\n", msg)
  }
  
  
  # Processing bolus information
  if(field == "all" | field== "bolus"){
    if("bolus" %in% names(cfg$options$inputs))  {
      msg = sprintf("%s #-> Bolus dosing details \n", msg)
      msg = sprintf("%s #->%s |  %s | %s | %s\n",  msg,
                 pad_string("field", 10),
                 pad_string("values", 10),
                 pad_string("scaling", 10),
                 pad_string("units", 10))
      msg = sprintf("%s #->%s \n", msg, paste(replicate(50, "-"), collapse = ""))
      msg = sprintf("%s #->%s |  %s | %s | %s\n", msg,
                 pad_string("times", 10),
                 pad_string(paste(cfg$options$inputs$bolus$times$values, collapse=" "), 10),
                 pad_string(cfg$options$inputs$bolus$times$scale, 10),
                 pad_string(cfg$options$inputs$bolus$times$units, 10))
      
      for(species in names(cfg$options$inputs$bolus$species)){
        msg = sprintf("%s #->%s |  %s | %s | %s\n", msg,
                   pad_string(species, 10),
                   pad_string(paste(cfg$options$inputs$bolus$species[[species]]$values, collapse=" "), 10),
                   pad_string(cfg$options$inputs$bolus$species[[species]]$scale, 10),
                   pad_string(cfg$options$inputs$bolus$species[[species]]$units, 10))
      }
      msg = sprintf("%s #->%s \n\n", msg, paste(replicate(50, "-"), collapse = ""))
      
    } else {
      msg = sprintf("%s #-> No bolus information found\n", msg) }
  }
  
  # Processing infusion rate information
  if(field == "all" | field== "rate"){
    if("infusion_rates" %in% names(cfg$options$inputs))  {
      msg = sprintf("%s #-> Infusion rate details \n", msg)
      msg = sprintf("%s #->%s | %s | %s | %s | %s\n", msg,
                 pad_string("Rate ", 10),
                 pad_string("field", 10),
                 pad_string("values", 10),
                 pad_string("scaling", 10),
                 pad_string("units", 10))
      msg = sprintf("%s #->%s \n", msg, paste(replicate(65, "-"), collapse = ""))
      for(rate    in cfg$options$inputs$infusion_rate_names){
        msg =sprintf("%s #->%s | %s | %s | %s | %s\n", msg,
                   pad_string(rate, 10),
                   pad_string('time', 10),
                   pad_string(paste(cfg$options$inputs$infusion_rates[[rate]]$times$values, collapse=" "), 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$times$scale, 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$times$units, 10))
        msg =sprintf("%s #->%s | %s | %s | %s | %s\n", msg,
                   pad_string('', 10),
                   pad_string('levels', 10),
                   pad_string(paste(cfg$options$inputs$infusion_rates[[rate]]$levels$values, collapse=" "), 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$levels$scale, 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$levels$units, 10))
      }
      msg =sprintf("%s #->%s \n\n", msg, paste(replicate(65, "-"), collapse = ""))
    } else {
      msg =sprintf("%s #-> No infusion rate information found\n",msg) }
  }
  
  # Processing covariate information
  if(field == "all" | field== "covariate"){
    if("covariates" %in% names(cfg$options$inputs))  {
      msg = sprintf("%s #-> Covariate details \n", msg)
      msg = sprintf("%s #->%s | %s | %s | %s\n", msg,
                 pad_string(" Covariate", 10),
                 pad_string("field", 10),
                 pad_string("values", 10),
                 pad_string("units", 10))
      msg = sprintf("%s #->%s \n", msg, paste(replicate(50, "-"), collapse = ""))
        for(covariate in names(cfg$options$inputs$covariates)){
          msg =sprintf("%s #->%s | %s | %s | %s\n", msg,
                     pad_string(covariate, 10),
                     pad_string('time', 10),
                     pad_string(paste(cfg$options$inputs$covariates[[covariate]]$times$values, collapse=" "), 10),
                     pad_string(      cfg$options$inputs$covariates[[covariate]]$times$units, 10))
          msg =sprintf("%s #->%s | %s | %s | %s\n", msg,
                     pad_string(sprintf('(%s)', cfg$options$inputs$covariates[[covariate]]$cv_type), 10),
                     pad_string('levels', 10),
                     pad_string(paste(cfg$options$inputs$covariates[[covariate]]$values$values, collapse=" "), 10),
                     pad_string(      cfg$options$inputs$covariates[[covariate]]$values$units,  10))
        }
        msg =sprintf("%s #->%s \n\n", msg, paste(replicate(50, "-"), collapse = ""))
    } else {
      msg =sprintf("%s #->No covariate information found\n\n", msg)}
  }
  
  # Processing iiv information    
  if(field == "all" | field== "iiv"){
    if("iiv" %in% names(cfg))  {
      msg = sprintf("%s #-> IIV details \n", msg)
      msg = sprintf("%s #-> IIV/Parameter set: \n", msg)
      msg = sprintf("%s #-> %s \n", msg, cfg$iiv$current_set)
      msg = sprintf("%s #-> Variance/covariance matrix \n", msg)
      iivs = names(cfg$iiv$iivs)
      # creating the headers
      msg = sprintf("%s #-> %s",msg,  pad_string('', 18))
      for(colidx in 1:length(iivs)){
        msg = sprintf("%s%s",msg, pad_string(iivs[colidx], 18)) }
        msg = sprintf("%s\n",msg)
      for(rowidx in 1:length(iivs)){
        msg = sprintf("%s #-> %s",msg,  pad_string(iivs[rowidx], 18))
        for(colidx in 1:length(iivs)){
          msg = sprintf("%s%s",msg,  var2string( cfg$iiv$values[rowidx,colidx], 18))
        }
        msg = sprintf("%s\n",msg)
      }
        
      msg = sprintf("%s #-> On parameters \n", msg)
      for(pname in names(cfg$iiv$parameters)){
         msg = sprintf("%s #-> %s, %s(%s)\n",msg,
                       pad_string(pname,10),
                       pad_string(cfg$iiv$parameters[[pname]]$iiv_name,10),
                       cfg$iiv$parameters[[pname]]$distribution, 10)  
      }
      
    } else {
      msg = sprintf("%s #-> No IIV information found\n", msg) }
  }

  #
  # Simulation Options
  #
  if(field == "all" | field== "simulation"){
     msg = sprintf("%s\n #-> Simulation details \n\n", msg)
     if('integrate_with' %in% names(cfg$options$simulation_options)){
       msg = sprintf("%s #-> integrate_with          %s \n", msg, cfg$options$simulation_options$integrate_with)
     }
     if('output_times' %in% names(cfg$options$simulation_options)){
       msg = sprintf("%s #-> output_times            %s \n", msg, var2string_gen(cfg$options$simulation_options$output_times))
     }
  }
  #
  # Solver Options
  #

  #
  # Stochastic Options
  #



  #
  # Datasets
  #
  if(field == "all" | field== "datasets"){
      if("data" %in% names(cfg))  {
        msg = sprintf("%s\n #-> Dataset details \n", msg)
        for(ds_name   in names(cfg$data)){
          msg = sprintf("%s #-> ----------------\n", msg)
          msg = sprintf("%s #-> Name:      %s\n", msg, ds_name)
          msg = sprintf("%s #-> Data File: %s\n", msg, cfg$data[[ds_name]]$data_file$name)
          if("sheet" %in% names(cfg$data[[ds_name]]$data_file)){
            msg = sprintf("%s #-> Sheet:     %s\n", msg, cfg$data[[ds_name]]$data_file$sheet)
          }
          msg = sprintf("%s #-> Columns:   %s\n", msg, paste(colnames(cfg$data[[ds_name]]$values), collapse=", "))
          msg = sprintf("%s #-> Rows:      %d\n", msg, nrow(cfg$data[[ds_name]]$values))
        }
      } else {
       msg = sprintf("%s #-> No datasets loaded\n", msg) }
  }


  #
  # Estimation Options
  #
  if(field == "all" | field== "estimation"){
     msg = sprintf("%s\n #-> Estimation details \n\n", msg)
     msg = sprintf("%s #-> Parameter set:          %s \n", msg, cfg$parameters$current_set)
     msg = sprintf("%s #-> Parameters estimated:   %s \n", msg, toString(names(cfg$estimation$mi)))
     msg = sprintf("%s #-> objective_type          %s \n", msg, cfg$estimation$objective_type)
     msg = sprintf("%s #-> observation_function    %s \n", msg, cfg$estimation$options$observation_function)

  }


  #
  # Dataset information
  #
  if(field == "all" | field== "cohorts"){
     if("cohorts" %in% names(cfg))  {
       msg = sprintf("%s\n #-> Cohort details \n\n", msg)
       for(ch_name   in names(cfg$cohorts)){
         msg = sprintf("%s #-> Cohort: %s\n", msg, ch_name)
         msg = sprintf("%s #-> ----------------\n", msg)
         msg = sprintf("%s #-> dataset: %s\n", msg, cfg$cohorts[[ch_name]]$dataset)
         msg = sprintf("%s #-> Cohort options (options) \n", msg)
         if('options' %in% names(cfg$cohorts[[ch_name]])){
           for(opname in names(cfg$cohorts[[ch_name]]$options)){
             msg = sprintf("%s #->     %s = c(%s)      \n", msg, opname, 
             toString(cfg$cohorts[[ch_name]]$cf[[opname]]))
           }
         } else{
           msg = sprintf("%s #->     none           \n", msg)
         }

         #options
         msg = sprintf("%s #-> \n", msg)

         #filter 
         msg = sprintf("%s #-> Cohort filter (cf) \n", msg)
         if('cf' %in% names(cfg$cohorts[[ch_name]])){
           for(col_name in names(cfg$cohorts[[ch_name]]$cf)){
             msg = sprintf("%s #->     %s = c(%s)      \n", msg, col_name, 
             toString(cfg$cohorts[[ch_name]]$cf[[col_name]]))
           }
         } else{
           msg = sprintf("%s #->     none           \n", msg)
         }

         msg = sprintf("%s #-> \n", msg)

         msg = sprintf("%s #-> Cohort-specific parametrers (cp) \n", msg)
         if('cp' %in% names(cfg$cohorts[[ch_name]])){
           for(pname in names(cfg$cohorts[[ch_name]]$cp)){
             msg = sprintf("%s #->     %s = %s        \n", msg, pname, 
             toString(cfg$cohorts[[ch_name]]$cp[[pname]]))
           }
         } else{
           msg = sprintf("%s #->     none           \n", msg)
         }
         #filter 
         msg = sprintf("%s #-> \n", msg)

         msg = sprintf("%s #-> Inputs \n", msg)
         #inputs 
         msg = sprintf("%s #-> \n", msg)

         msg = sprintf("%s #-> Outputs \n", msg)
         for(oname in names(cfg$cohorts[[ch_name]]$outputs)){

           msg = sprintf("%s #->   >%s<         \n", msg, oname)
           msg = sprintf("%s #->    Dataset:    \n", msg)
           msg = sprintf("%s #->     Sample Time  %s \n", msg, 
                   cfg$cohorts[[ch_name]]$outputs[[oname]]$obs$time)
           msg = sprintf("%s #->     Observation  %s \n", msg, 
                   cfg$cohorts[[ch_name]]$outputs[[oname]]$obs$value)
           if('missing' %in% names(cfg$cohorts[[ch_name]]$outputs[[oname]]$obs)){
               msg = sprintf("%s #->     Missing      %s \n", msg, 
                       toString(cfg$cohorts[[ch_name]]$outputs[[oname]]$obs$missing))
           }

           msg = sprintf("%s #->                \n", msg)

           msg = sprintf("%s #->    Model:      \n", msg)
           msg = sprintf("%s #->     Timescale    %s \n", msg, 
                   cfg$cohorts[[ch_name]]$outputs[[oname]]$model$time)
           msg = sprintf("%s #->     Output       %s \n", msg, 
                   cfg$cohorts[[ch_name]]$outputs[[oname]]$model$value)
           msg = sprintf("%s #->     Variance     %s \n", msg, 
                   cfg$cohorts[[ch_name]]$outputs[[oname]]$model$variance)
           msg = sprintf("%s #->    ---         \n", msg)

         }
         #outputs 
         msg = sprintf("%s #-> \n", msg)

       }
     } else {
       msg = sprintf("%s #-> No cohort information found\n", msg) }
  }
  
  # Processing infusion rate information
  if(field == "all" | field== "XXX"){
   #  if("infusion_rates" %in% names(cfg$options$inputs))  {
   #  } else {
   #    cat(" #-> No XXX information found\n") }
  }
  
return(msg)}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
var2string_gen <- function(var)  {
if(is.vector(var)){
  mystr = sprintf('min = %s; max = %s; length = %d ', 
  var2string(min(var), maxlength=0, nsig_f=1),
  var2string(max(var), maxlength=0, nsig_f=1), length(var)) 
} else {
  if(is.numeric(var)){
    mystr = var2string(var)
  } else {
    mystr = toString(var)
  }
}
return(mystr)
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
var2string <- function(var,maxlength=0, nsig_e = 3, nsig_f = 4) {
#  str = var2string(var, 12) 
#  converts the numerical value 'var' to a padded string 12 characters wide

if(var == 0){
 str = '0' 
}else if((var < .01 )| (var > 999)){
  #str = sprintf('%.3e', var )
  eval(parse(text=sprintf("str = sprintf('%%.%de', var )",nsig_e)))
}
else{
  #str = sprintf('%.4f', var )}
   eval(parse(text=sprintf("str = sprintf('%%.%df', var )",nsig_f)))
  }

str = pad_string(str, maxlength)
return(str)}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
pad_string <-function(str, maxlength=1, location='beginning'){
#  str = padstring(str, maxlength)
#
#  adds spaces to the beginning of the string 'str' until it is length
#  'maxlength'

  
  if(nchar(str)<maxlength)  {
    # calculating the number of spaces to add
    pad_length = maxlength-nchar(str) 
    # appending the spaces to the beginning of str
    if(location == "beginning"){
      str = sprintf('%s%s', paste(replicate(pad_length, " "), collapse = ""),str)
    }
    else{
      str = sprintf('%s%s', str, paste(replicate(pad_length, " "), collapse = ""))
    }
  }
return(str)}



#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
simulate_subjects = function (parameters, cfg, progress_message = "Simulating Subjects:"){
#function [predictions] = simulate_subjects(parameters, cfg)
#
# Inputs:
#
# cfg - System configuration variable generated in the following manner:
#
# cfg = system_fetch_cfg()
# cfg = system_select_set(cfg, 'default')
#
# parameters - vector of typical parameter values. This can be obtained from
# the cfg variable:
#
# parameters = system_fetch_parameters(cfg)
#
# cfg$options$stochastic
# list with the following fields:
#
#   nsub 
#      number of subjects to simulate  (default 100)
#
#   seed  
#      seed for random number generator (default 8675309)
#
#   ci    
#      desired confidence interval (e.g. 95)
#
#   ponly 
#      generate only the parameters and do not perform the simulation
#      TRUE, or FALSE (default)
#
#   sub_file 
#      name of the data structure loaded with system_load_dataset
#
#
# These values can then be modified as necessary.
#
# Output:
#
# The predictions data structure contains the following:
#
# predictions$tcsummary
#   This is a data frame that summarizes the predictions with the following
#   fields:
#     ts.TIMESCALE
#     s.STATE.X
#     o.OUTPUT.X
#
#   Where TIMESCALE, STATE, and OUTPUT refer to the named timescales states
#   and outputs. X can be either the mean, median, lb_ci or ub_ci (the latter
#   represent the lower and upper bounds on the confidence interval).
#
#
# predictions$subjects 
#   Contains the parameters and secondary parameters, one row for each subject  
#
# predictions$times
#   A field for every timescale containing the sample times from the
#   simulation.
#
# predictions$states and predictions$outputs -
#   There is a field for each state or output which contains a profile for
#   each subject (one per column) and each row corresponds to the sampling
#   times in predictions$times

p = list()

# defining the default values
nsub              = 100
seed              = 8675309
ci                = 95
ponly             = FALSE
sub_file          = NULL
sub_file_sample   = 'with replacement'
sub_file_ID_col   = 'SIMINT_ID'
sub_file_TIME_col = 'SIMINT_TIME'
# Used to map IDs form the sub_file to 
# Subject IDs
sub_file_ID_map   = data.frame(file_ID = c(),
                               sub_ID  = c())

state_names  = names(cfg$options$mi$states)
output_names = names(cfg$options$mi$outputs)

if("stochastic" %in% names(cfg$options)){
# Parsing stochastic options
  if("nsub" %in% names(cfg$options$stochastic)){
    nsub = cfg$options$stochastic$nsub
  }
  
  if("seed" %in% names(cfg$options$stochastic)){
    seed = cfg$options$stochastic$seed
  } 
  
  if("ci" %in% names(cfg$options$stochastic)){
    ci   = cfg$options$stochastic$ci
  } 
  
  if("sub_file" %in% names(cfg$options$stochastic)){
    sub_file   = cfg$options$stochastic$sub_file
  } 

  if("sub_file_sample" %in% names(cfg$options$stochastic)){
    sub_file_sample   = cfg$options$stochastic$sub_file_sample
  } 

  if("ponly" %in% names(cfg$options$stochastic)){
    ponly = cfg$options$stochastic$ponly
  } 
  if("states" %in% names(cfg$options$stochastic)){
    state_names = cfg$options$stochastic$states
  } 

  if("outputs" %in% names(cfg$options$stochastic)){
    output_names = cfg$options$stochastic$outputs
    # By default all outputs will include those with and without residual error.
    # If the user specifies outputs manually, then we also add in the output
    # with error if it has been defined in the system file.
    output_names_specified = output_names;
    for(output_name in output_names_specified){
      if(output_name %in% names(cfg$ve)){
        output_names = c(output_names, sprintf('SIOE_%s', output_name))
      }
    }
  } 
}


max_errors = 100;

isgood = TRUE;

if("iiv" %in% names(cfg) | !is.null(sub_file)){

  # If the subjects file is null we check the IIV matrix
  if(is.null(sub_file)){
    # otherwise we check the IIV
    if(min((eigen((cfg$iiv$values + (cfg$iiv$values))/2))$values) <= 0){
      cat("----------------------------------------------\n ");
      cat("  simulate_subjects()                         \n ");
      cat("> Warning: The variance/covariance matrix is not\n ");
      cat("> positive semi-definite. Testing only the diagonal\n ");
      cat("> elements. I.e. no covariance/interaction terms\n");
    
      cfg$iiv$values = diag(diag(cfg$iiv$values))
      if(min((eigen((cfg$iiv$values + (cfg$iiv$values))/2))$values) <= 0){
        cat("> Failed using only diagonal/variance elements.");
        cat("> Check the specified IIV elements in");
        cat("> cfg$iiv$values");
        isgood = FALSE 
      } else {
        cat("> Using only the diagional elements seems to   ");
        cat("> have worked. Understand that the results do  ");
        cat("> not include any interaction.                 ");
      }
    }
  }
  else{
  
      # Summarizing information about the data file
      sub_file_dataset        = cfg$data[[sub_file]]$values
      sub_file_nsub           = length(unique(sub_file_dataset[[sub_file_ID_col]]))
      sub_file_file_name      = cfg$data[[sub_file]]$data_file$name

      # Parameter information
      sub_file_p_found        =       intersect(names(parameters), names(sub_file_dataset))
      sub_file_p_missing      =       setdiff(names(parameters), names(sub_file_dataset))
      if(length(sub_file_p_found) > 0){
        sub_file_p_found_str  = paste(intersect(names(parameters), names(sub_file_dataset)), collapse=', ') }
      else{
        sub_file_p_found_str  =  "None" }
      if(length(sub_file_p_found) > 0){
        sub_file_p_missing_str= paste(setdiff(names(parameters), names(sub_file_dataset)), collapse=', ')}
      else{
        sub_file_p_missing_str  =  "None" }

      # Covariate information
      sub_file_cov_all = names(cfg$options$inputs$covariates)
      if(length(sub_file_cov_all) > 0){
        # Covariate details
        sub_file_cov_found      = intersect(sub_file_cov_all, names(sub_file_dataset))
        sub_file_cov_missing    =   setdiff(sub_file_cov_all, names(sub_file_dataset))
        if(length(sub_file_cov_found) > 0){
          sub_file_cov_found_str  = paste(sub_file_cov_found, collapse=', ') }
        else{
          sub_file_cov_found_str  =  "None" }
        if(length(sub_file_cov_missing) > 0){
          sub_file_cov_missing_str  = paste(sub_file_cov_missing, collapse=', ') }
        else{
          sub_file_cov_missing_str  =  "None" }
        }
      else {
        # No covariates
        sub_file_cov_found      = c()
        sub_file_cov_missing    = c()
        sub_file_cov_found_str  = "" 
        sub_file_cov_missing_str= "" 
      }

  
  }
  
  # Set the random seed
  set.seed(seed)

  if(isgood){

    if(!is.null(sub_file)){                            
    if((nsub > sub_file_nsub & sub_file_sample == "without replacement")){
       vp(cfg, "----------------------------------------------")
       vp(cfg, "simulate_subjects ()")
       vp(cfg, sprintf("Warning: The number of subjects requested (%d) is greater than", nsub))
       vp(cfg, sprintf("the number in the subjects dataset (%d) so it is not", sub_file_nsub))
       vp(cfg, sprintf("possible to sample without replacement. Changing sampling"))
       vp(cfg, sprintf("method to 'with replacement'"))
       vp(cfg, "----------------------------------------------")
       sub_file_sample = "with replacement"
    }
    }


    vp(cfg, sprintf("Simulating multiple subjects (%d)", nsub))
    vp(cfg, sprintf("Integrating with:            %s ",  cfg$options$simulation_options$integrate_with))
    vp(cfg, sprintf("Parallel set to:             %s ",  cfg$options$simulation_options$parallel))
    vp(cfg, sprintf("Number of cores:             %d ",  cfg$options$simulation_options$compute_cores))
    if(!is.null(sub_file)){                            
    vp(cfg, sprintf("Subjects file:               %s ", sub_file_file_name))
    vp(cfg, sprintf("   Parameters from file:     %s ", sub_file_p_found_str))
    vp(cfg, sprintf("   Default parameters used:  %s ", sub_file_p_missing_str))
    vp(cfg, sprintf("   Subjects in file:         %d ", sub_file_nsub))
    vp(cfg, sprintf("   Sampling:                 %s ", sub_file_sample))
      if(length(sub_file_cov_all) > 0){
      vp(cfg, sprintf("   Covariates from file:     %s ", sub_file_cov_found_str))
      vp(cfg, sprintf("   Default covariates used:  %s ", sub_file_cov_missing_str))
      }
      else{
      vp(cfg, "   Covariates:               None specified in system ")
      }
    }
  }


  vp(cfg, "Generating the parameters for subjects.")
  vp(cfg, "Be patient, there may be a delay...")


  # generating the parameters for each of the subjects
  sub_idx = 1;

  # If the subject file null then we generate the subjects using 
  # the specified IIV
  if(is.null(sub_file)){
    while((sub_idx <= nsub) & isgood) {
      subject = generate_subject(parameters,  cfg);
      parameters_subject = subject$parameters;
      if(sub_idx == 1){
        p$subjects$parameters           = parameters_subject
      } else{
        p$subjects$parameters           = rbind(p$subjects$parameters,          parameters_subject)}
    
      sub_idx = sub_idx + 1;
    }
  }
  else{
    # Sampling the subject IDs from the sub_file based on the methodology
    # specified by the user
    if(sub_file_sample == "sequential"){
      file_IDs = rep_len(sub_file_dataset[[sub_file_ID_col]], nsub) 
      }
    else if(sub_file_sample == "with replacement"){
      file_IDs = sample(sub_file_dataset[[sub_file_ID_col]], 
                       size    =  nsub,
                       replace =TRUE) 
      }
    else if(sub_file_sample == "without replacement"){
      file_IDs = sample(sub_file_dataset[[sub_file_ID_col]], 
                       size    =  nsub,
                       replace =FALSE)
      }

    for(sub_idx in 1:length(file_IDs)){
       # Ceating the subject parameters with the default values
       parameters_subject = parameters
    
       # Now we overwrite those parameters specified in the dataset
       tmp_sub_records = sub_file_dataset[sub_file_dataset[[sub_file_ID_col]] == file_IDs[sub_idx], ]
       parameters_subject[,sub_file_p_found] = tmp_sub_records[1,sub_file_p_found]
    
       # Storing the subject in the data frame with the other subjects
       if(sub_idx == 1){
         p$subjects$parameters           = parameters_subject
       } else{
         p$subjects$parameters           = rbind(p$subjects$parameters,          parameters_subject)}
    
    
     # Soring the map between the ID in the file and the sampled subject id
     sub_file_ID_map   = rbind(sub_file_ID_map, 
                               data.frame(file_ID = file_IDs[sub_idx],
                                          sub_ID  = sub_idx))
    
    
    }
  }
  
  # Running simulations
  if(!ponly){
    vp(cfg, "Now running the simulations")
    # Initialzing progress bar
    # If we're running as a script we display this in the console
    # otherwise we initialize a shiny onject
    if(cfg$options$misc$operating_environment == 'script'){
      pb = txtProgressBar(min=0, max=1, width=12, style=3, char='.') 
      # JMH for parallel
      myprogress <- function(n) setTxtProgressBar(pb, n)
      }
  
    if(cfg$options$misc$operating_environment == 'gui'){
      pb <- shiny::Progress$new()
      # JMH how to parallelize 
      pb$set(message = progress_message, value = 0)
    }
  
    if("multicore" == cfg$options$simulation_options$parallel){
      #
      # Running simulations in parallel
      #

      # Setting up and starting the cluter
      cl <- makeCluster(cfg$options$simulation_options$compute_cores)
      registerDoParallel(cl)
      
      somall <- foreach(sub_idx=1:nsub,
                        .verbose = FALSE,
                        .errorhandling='pass',
                        .options.snow=list(progress = myprogress),
                        .packages=c("deSolve")) %dorng% {

        # If we're using the c-file we tell the spawned instances to load
        # them.
        if(cfg$options$simulation_options$integrate_with == "c-file"){
          dyn.load(paste("r_ode_model", .Platform$dynlib.ext, sep = ""))
        }
        source("library/r_general/ubiquity.r");
        source("transient/auto_rcomponents.r");
      
        # Pulling out subject level parameters
        parameters_subject = p$subjects$parameters[sub_idx,]

        # storing the cfg for the subject
        cfg_sub = cfg

        # If we're reading from a file and covariates were specified
        # then we have to apply those on a per subject basis
        if(!is.null(sub_file)){
          if(length(sub_file_cov_found) > 0){
            cfg_sub = 
            apply_sub_file_COV(tmpcfg       = cfg_sub, 
                               cov_found    = sub_file_cov_found, 
                               sub_dataset  = sub_file_dataset,
                               sub_ID_col   = sub_file_ID_col,
                               sub_TIME_col = sub_file_TIME_col,
                               file_ID      = sub_file_ID_map[sub_file_ID_map$sub_ID == sub_idx,]$file_ID)
          }
        }
      
        # Running either titration or normal simulation
        if(cfg$titration$titrate){
          exec.time = system.time((som = run_simulation_titrate(parameters_subject, cfg_sub)))
          #som = run_simulation_titrate(parameters_subject, cfg)
          }
        else{
          exec.time = system.time((som = run_simulation_ubiquity(parameters_subject, cfg_sub)))
          #som = run_simulation_ubiquity(parameters_subject, cfg)
          }
      
        # Storing the subject id
        som$sub_idx = sub_idx
      
        # saving the execution time
        som$exec.time = exec.time
      
     #  if(cfg$options$misc$operating_environment == 'gui'){
     #    pb$inc(1/nsub, detail = sprintf('%d/%d (%d %%)', sub_idx, nsub, floor(100*sub_idx/nsub))) }
      
        som }

      #
      # Stopping the cluster
      #
      stopCluster(cl)

    }
    else{
      #
      # Running simulations sequentially 
      #
      somall <- foreach(sub_idx=1:nsub) %do% {
      
        # Setting the seed based on the subject ID and the 
        # user specified seed: this applies to subject level 
        # measurement error 
        # set.seed(seed+sub_idx)
      
        # Pulling out subject level parameters
        parameters_subject = p$subjects$parameters[sub_idx,]

        cfg_sub = cfg

        # If we're reading from a file and covariates were specified
        # then we have to apply those on a per subject basis
        if(!is.null(sub_file)){
          if(length(sub_file_cov_found) > 0){
            cfg_sub = 
            apply_sub_file_COV(tmpcfg       = cfg_sub, 
                               cov_found    = sub_file_cov_found, 
                               sub_dataset  = sub_file_dataset,
                               sub_ID_col   = sub_file_ID_col,
                               sub_TIME_col = sub_file_TIME_col,
                               file_ID      = sub_file_ID_map[sub_file_ID_map$sub_ID == sub_idx,]$file_ID)
          }
        }
      
        # Running either titration or normal simulation
        if(cfg$titration$titrate){
          exec.time = system.time((som = run_simulation_titrate(parameters_subject, cfg_sub)))
          #som = run_simulation_titrate(parameters_subject, cfg)
          }
        else{
          exec.time = system.time((som = run_simulation_ubiquity(parameters_subject, cfg_sub)))
          #som = run_simulation_ubiquity(parameters_subject, cfg)
          }
      
        # Storing the subject id
        som$sub_idx = sub_idx
      
        # saving the execution time
        som$exec.time = exec.time
      
        # Updating progress indicators
        if(cfg$options$misc$operating_environment == 'script'){
          myprogress(sub_idx/nsub) }
      
        if(cfg$options$misc$operating_environment == 'gui'){
          pb$inc(1/nsub, detail = sprintf('%d/%d (%d %%)', sub_idx, nsub, floor(100*sub_idx/nsub))) }
      
        som }
    }


    # Pulling out the lengths of different things
    ntimes = length(somall[[1]]$simout$time)
    npsec  = length(names(cfg$options$ssp))

    # pulling out the first subject to use below:
    som    = somall[[1]]


    # Initializing states, outputs, and titration matrices 
    for(state_name   in state_names){
      p$states[[state_name]]            = matrix(0, nsub, ntimes) }
    for(output_name   in output_names){
      p$outputs[[output_name]]          = matrix(0, nsub, ntimes) }
    for(titration_name   in names(som$titration)){
      p$titration[[titration_name]]     = matrix(0, nsub, ntimes) }

    # Initializing the secondary parameters
    # Creating the data frame
    p$subjects$secondary_parameters  = as.data.frame(matrix(0, ncol = npsec, nrow=nsub))

    # putting the column names
    colnames( p$subjects$secondary_parameters) = names(cfg$options$ssp)

    # And storing the output times/timescales
    p$times    = som$simout["time"]
    # creating the time patch vectors for the different timescales
    for(timescale_name   in names(cfg$options$time_scales)){
     timescale_name = sprintf('ts.%s', timescale_name)
     p$times[[timescale_name]] = c(som$simout[[timescale_name]])
    }

    for(som in somall){
      sub_idx = som$sub_idx
    
      # storing the secondary parameters
      p$subjects$secondary_parameters[sub_idx,] = som$simout[1,names(cfg$options$ssp)]

      # Storing the states, outputs and titration information
      for(state_name   in state_names){
        p$states[[state_name]][sub_idx,] = som$simout[[state_name]] }
      
      for(output_name   in output_names){
        p$outputs[[output_name]][sub_idx,] = som$simout[[output_name]] }

      for(titration_name   in names(som$titration)){
        p$titration[[titration_name]][sub_idx,] = som$titration[[titration_name]]}
      sub_idx = sub_idx + 1;
    }

    # Cleaning up the progress bar objects
    if(cfg$options$misc$operating_environment == 'script'){
      close(pb)}
    if(cfg$options$misc$operating_environment == 'gui'){
        pb$close()}
    
  }

  
  #
  # summarizing the data into a data frame with means, medians, confidence intervals, etc.
  #
  if(!ponly){
    for(timescale_name   in names(cfg$options$time_scales)){
      if("tcsummary" %in% names(p)){
        eval(parse(text=sprintf('p$tcsummary[["ts.%s"]] = som$simout[["ts.%s"]]', timescale_name, timescale_name))) 
      }else{
        eval(parse(text=sprintf('p$tcsummary = data.frame(ts.%s =  som$simout[["ts.%s"]])', timescale_name, timescale_name))) 
      }
    }
    for(state_name   in names(p$states)){
      tc = timecourse_stats(p$states[[state_name]],ci)
      eval(parse(text=sprintf('p$tcsummary[["s.%s.lb_ci"]]   = tc$stats$lb_ci',   state_name))) 
      eval(parse(text=sprintf('p$tcsummary[["s.%s.ub_ci"]]   = tc$stats$ub_ci',   state_name))) 
      eval(parse(text=sprintf('p$tcsummary[["s.%s.mean"]]    = tc$stats$mean',    state_name))) 
      eval(parse(text=sprintf('p$tcsummary[["s.%s.median"]]  = tc$stats$median',  state_name))) 
      }
    for(output_name   in names(p$outputs)){
      tc = timecourse_stats(p$outputs[[output_name]],ci)
      eval(parse(text=sprintf('p$tcsummary[["o.%s.lb_ci"]]   = tc$stats$lb_ci',   output_name))) 
      eval(parse(text=sprintf('p$tcsummary[["o.%s.ub_ci"]]   = tc$stats$ub_ci',   output_name))) 
      eval(parse(text=sprintf('p$tcsummary[["o.%s.mean"]]    = tc$stats$mean',    output_name))) 
      eval(parse(text=sprintf('p$tcsummary[["o.%s.median"]]  = tc$stats$median',  output_name))) 
      }
  }

} else {
  cat("---------------------------------------------- \n");
  cat("  simulate_subjects()                          \n");
  cat("> Error:Trying to simulate subjects with       \n");
  cat(">    variability, but no variance/covariance   \n");
  cat(">    information was specified.                \n");
  cat(">                                              \n");
  cat(">    Modify the system.txt file to add the     \n");
  cat(">    IIV information using the following:      \n");
  cat(">     <IIV:?>      ?                           \n");
  cat(">     <IIV:?:?>    ?                           \n");
  cat(">     <IIVCOR:?:?> ?                           \n");
  cat("---------------------------------------------- \n");
}

return(p)
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
timecourse_stats = function (d, ci){
#
# Given a matrix (d) of time courses (each row is an individual and each column is
# a time point) and a confidence interval (ci) this will calculate the mean,
# median, confidence intervals and a vector of values for creating patches.
# 

tc = list();

myci = ci/100
dsorted = apply(d, 2, sort)
nsubs   = length(dsorted[,1]) 
lb_idx  = nsubs*(1-myci)/2 + 1;
ub_idx  = nsubs - nsubs*(1-myci)/2;

tc$stats$lb_ci  = apply(rbind(dsorted[floor(lb_idx),],  dsorted[ ceiling(lb_idx),]), 2, mean)
tc$stats$ub_ci  = apply(rbind(dsorted[floor(ub_idx),],  dsorted[ ceiling(ub_idx),]), 2, mean)

tc$stats$mean   = apply(dsorted, 2, mean)
tc$stats$median = apply(dsorted, 2, median)


tc$patch$ci  = c(tc$stats$ub_ci,  rev(tc$stats$lb_ci))

return(tc)

}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
apply_sub_file_COV = function (tmpcfg, cov_found, sub_dataset, sub_ID_col, sub_TIME_col, file_ID){
# This function is used when stochastic simulations are being performed using
# a data file for the subject level information. If the data file contains
# covariate information, this function will update the system for each subjects
# covariates. 

# Pulling all records for the current subject
sub_records = sub_dataset[sub_dataset[[sub_ID_col]] == file_ID,]

# Looping through each covariate and updating the cfg file
for(cov_name in cov_found){
  tmpcfg = system_set_covariate(tmpcfg, cov_name,          
                                        times  = sub_records[[sub_TIME_col]],
                                        values = sub_records[[cov_name]])
}

return(tmpcfg)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
generate_subject = function (parameters, cfg){
# function [subject] = generate_subject(parameters, cfg)
#
# Generates subject with variability specified using the <IIV:?> descriptor
# in the system.txt file
#
# Inputs:
#
# cfg - system configuration variable generated in the following manner:
#
# cfg = system_fetch_cfg()
# cfg = system_select_set(cfg, 'default')
#
# parameters - vector of typical parameter values. This can be obtained from
# the cfg variable:
#
# parameters = cfg$parameters$values
#
# This can be modified before subject generation
#
# Output:
#
# The data structure 'subject' will be generated with the following fields:
#
# subject$parameters  - parameters for a sample from a subject 
#

library("MASS") 

subject = list()
subject$parameters   = parameters;


#
# Generating the subject
#
#iiv_parameter_names = fieldnames(cfg.iiv.parameters);
# creating a temporary vector containing the typical values of all of the
# parameters:
TMP_parameters_all = parameters;

# defining the mean of the IIVs and the covariance matirx
covmatrix = cfg$iiv$values;
muzero    = matrix(0, nrow(covmatrix),1)

# Generating the normal sample:
iiv_sample = mvrnorm(n = 1, muzero, covmatrix, tol = 1e-6, empirical = FALSE, EISPACK = FALSE);

# now looping through each parameter with inter-individual variability
#names(cfg$iiv$iivs)
#names(cfg$iiv$parameters)
for(TMP_parameter_name in names(cfg$iiv$parameters)){

  # getting the typical value of the parameter
  TMP_parameter_value = parameters[TMP_parameter_name];

  # pulling out the distribution and IIV name
  eval(parse(text=paste(sprintf("TMP_equation     = cfg$iiv$parameters$%s$equation",    TMP_parameter_name))))
  eval(parse(text=paste(sprintf("TMP_iiv_name     = cfg$iiv$parameters$%s$iiv_name",    TMP_parameter_name))))

  # pulling out the random IIV value for the current iiv
  eval(parse(text=paste(sprintf("TMP_iiv_value = iiv_sample[cfg$options$mi$iiv$%s]",TMP_iiv_name))))

  TMP_subject_parameter_value = generate_parameter(parameters, cfg, TMP_parameter_value, TMP_iiv_value, TMP_equation);

  # Storing the sample in the vector with all parameters
  subject$parameters[TMP_parameter_name] = TMP_subject_parameter_value
}


return(subject)

}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
generate_parameter = function (SIMINT_parameters, SIMINT_cfg, SIMINT_PARAMETER_TV, SIMINT_IIV_VALUE, SIMINT_equation){
  # Defining the system parameters locally
  for(SIMINT_pname in names(SIMINT_cfg$options$mi$parameters)){
    eval(parse(text=paste(sprintf("%s = SIMINT_parameters[SIMINT_cfg$options$mi$parameters$%s]", SIMINT_pname, SIMINT_pname))))
  }

  # Evaluating the parameter with IIV
  return( eval(parse(text=paste(SIMINT_equation))))
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_log_init = function (cfg){
# initializes the log file then enables logging

  file.create(cfg$options$logging$file)
  cfg$options$logging$enabled = 'yes';
  system_log_entry(cfg, 'Ubiquity log init - R')

return(cfg)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_log_entry = function(cfg, entry){
#
# # Initialize the log file:
# # transient/ubiquity_log.txt
# cfg = system_log_init(cfg) 
#
# Add a log entry
# system_log_entry(cfg, "this is a log entry");
#


# if logging is disabled we don't do anything 
if(cfg$options$logging$enabled ==  "yes"){
  # If the log file doesn't exist we initialize it
  if(!file.exists(cfg$options$logging$file)){
   system_log_init(cfg);
  }
  # If the timestamp is enabled we prepend it to the
  # log message
  if(cfg$options$logging$timestamp == "yes"){
    entry = sprintf('%s %s',  format(Sys.time(), format=cfg$options$logging$ts_str), entry)
  }

  # Now we dump it to the log file:
  write(entry, file=cfg$options$logging$file, append=TRUE)
# fileConn<-file(cfg$options$logging$file)
# writeLines(c(entry), fileConn)
# close(fileConn)
  }
}


# The multiplot function taken from:
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
vp = function(cfg, str){
# function []=vp(cfg, str)
# vp -- verbose print
#
# Print out the message contained in 'str' if the verbose option in cfg is
# set. Example:
#
#  cfg$options$verbose = 'yes';
#
#  vp(cfg, 'Hellow world');
#

# logging string 
system_log_entry(cfg, str)

# printing if verbose is enabled
if('options' %in% names(cfg)){
if('verbose' %in% names(cfg$options)){
if('yes' == cfg$options$verbose){
  cat(sprintf('#> %s \n',toString(str)));
  }}}
}

GUI_log_entry <-function(cfg, text){
   system_log_entry(cfg, sprintf("RGUI %s", text))
}


# function [recs] = nm_select_records(cfg, values, filter)
#
# Takes a data set created using system_load_data with all or a subset of the data
# (derived from cfg$data$dsname$vaalues) in records, and filters the data according to
# the information specified in filter and returns that match. 
#
# Multiple filters are joined by a boolean AND. While multiple options for a
# given filter are combined using an OR.
#
# For example to extract only the observations (EVID=0) in cohort 2 of studies 1-3,
# we create the filter in the following way:
#  
#  
# myfilter$evid    = c(0)
# myfilter$cohorts = c(2)  
# myfilter$studies = c(1,2,3)
# 
# This creates the following boolean relationship:
# (evid = 0) and (chhorts = 2) and ((studies = 1) or (studies = 2) or (studies = 3))
#
#
# [obs] = nm_select_records(data, data.values, myfilter)
#
# This should display the first five rows of obs
# with the appropriate header on top

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
nm_select_records    <- function(cfg, values, filter){

  cols = names(filter) 
  if(length(cols) > 0){

    for(column_name in cols){
      #checking to see if the column exists in the dataset
      if(column_name %in% names(values)){
        # subsetting based on the current filter
        #values = values[values[[column_name]] == filter[[column_name]], ]
        values = values[values[[column_name]] %in% filter[[column_name]], ]
      } 
      else{
        vp(cfg, sprintf(' fieldname: %s not found ignoring this entry', column_name))
      }
    }
  }

  return(values)
}


#
# converts a time specified in a defined timescale (say weeks) to the
# timescale of the simulation (say hours if the rates are in 1/hr units)
#
  
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_ts_to_simtime <-function(cfg, tstime, ts){
   simtime = c()
   if(ts %in% names(cfg$options$time_scales)){
     simtime = tstime/cfg$options$time_scales[[ts]]
   }
   else{
    vp(cfg, sprintf('Unable to find timescale %s', ts)) }

    return(simtime)
}

#
# Removes all previously defined chorts
#
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_clear_cohorts  <- function(cfg){
  cfg$cohorts = c()
return(cfg)}

#
# Defines a cohort
#
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_define_cohort <- function(cfg, cohort){
  
 if('options' %in% names(cohort)){
   cohort$options = c() }

 defopts = c()
 defopts$marker_color   = 'black'           
 defopts$marker_shape   = 0           
 defopts$marker_line    = 1
 
 validopts = c('marker_color', 'marker_shape', 'marker_line')

 

 # Default values for control structures
 isgood      = TRUE
 datasetgood = TRUE 
 

 #
 # checking the cohort name
 #
 if('name' %in% names(cohort)){
  if(cohort$name %in% names(cfg$cohorts)){
    isgood = FALSE
    vp(cfg, sprintf('Error: cohort with name >%s< has already been defined', cohort$name))
  }
  else{
    name_check = ubiquity_name_check(cohort$name)

    cohort_name = cohort$name 
    # Checking the cohort name
    if(!name_check$isgood){
      isgood = FALSE
      vp(cfg, sprintf('Error: cohort with name >%s< is invalid', cohort$name))
      vp(cfg, sprintf('Problems: %s', name_check$msg))
      }
    }
 }
 else{
   isgood = FALSE 
   vp(cfg, 'Error: cohort name not specified')
   cohort_name = 'no name specified' 
 }

 #
 # checking the dataset details 
 #
 if('dataset' %in% names(cohort)){
   if(cohort$dataset %in% names(cfg$data)){
     # pulling the dataset out to test for fields below
     tmpdataset = cfg$data[[cohort$dataset]]
   }
   else{
     isgood      = FALSE 
     datasetgood = FALSE 
     vp(cfg, sprintf('Error: dataset >%s< not found, please load first', cohort$dataset))
   }
 }
 else{
   isgood      = FALSE 
   datasetgood = FALSE 
   vp(cfg, 'Error: dataset not specified for the cohort')
 }

 #
 # checking cohort-specific parameters
 # 
 if('cp' %in% names(cohort)){
   for(pname in names(cohort$cp)){
     if(!(pname %in% names(cfg$parameters$values))){
       isgood = FALSE
       vp(cfg, sprintf('Error: The parameter >%s< ', pname))
       vp(cfg, sprintf('       is not defined. Check the spelling'))
       vp(cfg, sprintf('       or define the parameter using <P> '))
     }
     else{
       if((pname %in% names(cfg$estimation$mi))){
         isgood = FALSE
         vp(cfg, sprintf('Error: The parameter >%s< ', pname))
         vp(cfg, sprintf('       is selected for estimation. It is ')) 
         vp(cfg, sprintf('       not possible to fix a parameter   ')) 
         vp(cfg, sprintf('       that is being estiamted.          ')) 
       }
     }
   }
 }


 #
 # checking cohort-filter columns against the dataset
 # 
 if(datasetgood){
   if('cf' %in% names(cohort)){
     for(cname in names(cohort$cf)){
       if(!(cname %in% names(cfg$data[[cohort$dataset]]$values))){
         isgood = FALSE
         vp(cfg, sprintf('Error: The column >%s< in the cohort filter ', cname)) 
         vp(cfg, sprintf('       was not found in the data set >%s< ', cohort$dataset)) 
       }
     }
   }
   else{
     cohort$cf = c()
     vp(cfg, sprintf('Warning: No cohort filter was specified.')) 
   }
 }


 #
 # checking inputs
 #
 if('inputs' %in% names(cohort)){
   # Bolus Inputs
   if('bolus' %in% names(cohort$inputs)){
     if('bolus' %in% names(cfg$options$inputs)){
       # processing each bolus input
       for(iname in names(cohort$inputs$bolus)){
         if(iname %in% names(cfg$options$inputs$bolus$species)){
           if('AMT'  %in% names(cohort$inputs$bolus[[iname]]) & 
              'TIME' %in% names(cohort$inputs$bolus[[iname]])){
             if(length(cohort$inputs$bolus[[iname]]$AMT) != length(cohort$inputs$bolus[[iname]]$TIME)){
               isgood = FALSE
               vp(cfg, sprintf('Error: For the bolus input >%s< the length of ', iname))
               vp(cfg, sprintf('       the AMT and TIME fields need to be the same'))
             }
           }
           else{
            isgood = FALSE
            vp(cfg, sprintf("Error: The bolus input >%s< needs an 'AMT' and a 'TIME' field", iname))
            vp(cfg, sprintf('       cohort$inputs$bolus$%s$AMT  = c()', iname))
            vp(cfg, sprintf('       cohort$inputs$bolus$%s$TIME = c()', iname))
           }
         }
         else{
          isgood = FALSE  
          vp(cfg, sprintf('Error: The bolus input >%s< has not been defined for this system', iname))
          vp(cfg, sprintf('       <B:times>;  %s  []; scale; units', pad_string('', nchar(iname))))
          vp(cfg, sprintf('       <B:events>; %s; []; scale; units', iname))
         }
       }
     }
     else{
      isgood = FALSE
      vp(cfg, sprintf('Error: An bolus input >%s< was specified for this cohort but'))
      vp(cfg, sprintf('       there are no bolus inputs defined in the system.txt file.'))
      vp(cfg, sprintf('       <B:times>;         []; scale; units'))
      vp(cfg, sprintf('       <B:events>; STATE; []; scale; units'))
     
     
     }
   }

   # Infusion rates
   if('infusion_rates' %in% names(cohort$inputs)){
     if('infusion_rates' %in% names(cfg$options$inputs)){
       # processing each infusion rate
       for(iname in names(cohort$inputs$infusion_rates)){
         if(iname %in% names(cfg$options$inputs$infusion_rates)){
           if('AMT'  %in% names(cohort$inputs$infusion_rates[[iname]]) & 
              'TIME' %in% names(cohort$inputs$infusion_rates[[iname]])){
             if(length(cohort$inputs$infusion_rates[[iname]]$AMT) != length(cohort$inputs$infusion_rates[[iname]]$TIME)){
               isgood = FALSE
               vp(cfg, sprintf('Error: For the infusion rate >%s< the length of ', iname))
               vp(cfg, sprintf('       the AMT and TIME fields need to be the same'))
             }
           }
           else{
            isgood = FALSE
            vp(cfg, sprintf("Error: The infusion rate >%s< needs an 'AMT' and a 'TIME' field", iname))
            vp(cfg, sprintf('       cohort$inputs$infusion_rates$%s$AMT  = c()', iname))
            vp(cfg, sprintf('       cohort$inputs$infusion_rates$%s$TIME = c()', iname))
           }
         }
         else{
          isgood = FALSE  
          vp(cfg, sprintf('Error: The infsuion rate >%s< has not been defined for this system', iname))
          vp(cfg, sprintf('       <R:%s>; times;   [];    scale; units ', iname))
          vp(cfg, sprintf('       <R:%s>; levels;  [];    scale; units ', iname))
         }
       }
     }
     else{  
      isgood = FALSE  
      vp(cfg, sprintf('Error: An infusion rate was specified for this cohort but')) 
      vp(cfg, sprintf('       there are no infusion rates defined in the system.txt file.')) 
      vp(cfg, sprintf('       <R:RNAME>; times;   [];    scale; units '))
      vp(cfg, sprintf('       <R:RNAME>; levels;  [];    scale; units '))
     }
   }


   # covariate       
  if('covariates' %in% names(cohort$inputs)){
    if('covariates' %in% names(cfg$options$inputs)){
      # processing each covariates
      for(iname in names(cohort$inputs$covariates)){
        if(iname %in% names(cfg$options$inputs$covariates)){
          if('AMT'  %in% names(cohort$inputs$covariates[[iname]]) & 
             'TIME' %in% names(cohort$inputs$covariates[[iname]])){
            if(length(cohort$inputs$covariates[[iname]]$AMT) != length(cohort$inputs$covariates[[iname]]$TIME)){
              isgood = FALSE
              vp(cfg, sprintf('Error: For the covariates >%s< the length of ', iname))
              vp(cfg, sprintf('       the AMT and TIME fields need to be the same'))
            }
          }
          else{
           isgood = FALSE
           vp(cfg, sprintf("Error: The covariates >%s< needs an 'AMT' and a 'TIME' field", iname))
           vp(cfg, sprintf('       cohort$inputs$covariates$%s$AMT  = c()', iname))
           vp(cfg, sprintf('       cohort$inputs$covariates$%s$TIME = c()', iname))
          }
        }
        else{
         isgood = FALSE  
         vp(cfg, sprintf('Error: The infsuion rate >%s< has not been defined for this system', iname))
         vp(cfg, sprintf('       <R:%s>; times;   [];   units ', iname))
         vp(cfg, sprintf('       <R:%s>; values;  [];   units ', iname))
        }
      }
    }
    else{  
     isgood = FALSE  
     vp(cfg, sprintf('Error: An covariates was specified for this cohort but')) 
     vp(cfg, sprintf('       there are no covariatess defined in the system.txt file.')) 
     vp(cfg, sprintf('       <CV:CNAME>; times;   []; units '))
     vp(cfg, sprintf('       <CV:CNAME>; values;  []; units '))
    }
  }
}


 #
 # checking outputs
 #

 if('outputs' %in% names(cohort)){
   # Looping through each output
   # and checking it 
   for(oname in names(cohort$outputs)){
     # This checks the user information against 
     # the information in the dataset
     if('obs' %in% names(cohort$outputs[[oname]])){
       # checking the TIME information 
       #  First that it's specified
       if('time' %in% names(cohort$outputs[[oname]]$obs)){
         # Next check to make sure the time column is in the dataset
         if(datasetgood){
          if(!(cohort$outputs[[oname]]$obs$time %in% names(cfg$data[[cohort$dataset]]$values))){
           isgood = FALSE  
           vp(cfg, sprintf('Error: For the output >%s< the specified observation time', oname))
           vp(cfg, sprintf('       column >%s< was not found in the dataset', cohort$outputs[[oname]]$obs$time))
          }
         }
       }
       else{
         isgood = FALSE 
         vp(cfg, sprintf('Error: For the output >%s<the column for the "time" must be specified', oname))
         vp(cfg, sprintf("       cohort$outputs$%s$obs$time  = 'name'; ", oname))
       }


       # checking the VALUE information 
       #  First that it's specified
       if('value' %in% names(cohort$outputs[[oname]]$obs)){
         # Next check to make sure the value column is in the dataset
         if(datasetgood){
          if(!(cohort$outputs[[oname]]$obs$value %in% names(cfg$data[[cohort$dataset]]$values))){
           isgood = FALSE  
           vp(cfg, sprintf('Error: For the output >%s< the specified observation value', oname))
           vp(cfg, sprintf('       column >%s< was not found in the dataset', cohort$outputs[[oname]]$obs$value))
          }
         }
       }
       else{
         isgood = FALSE 
         vp(cfg, sprintf('Error: For the output >%s<the column for the "value" must be specified', oname))
         vp(cfg, sprintf("       cohort$outputs$%s$obs$value  = 'name'; ", oname))
       }

     
     }
     else{
      isgood = FALSE 
      vp(cfg, sprintf('Error: For the output >%s< no observation information was specified', oname))
      vp(cfg, sprintf("       cohort$outputs$%s$obs$time  = 'name'; ", oname))
      vp(cfg, sprintf("       cohort$outputs$%s$obs$value = 'name'; ", oname))
     }

     # This checks the user information against 
     # the information in the model
     if('model' %in% names(cohort$outputs[[oname]])){
       #
       # Checking the times
       #
       if('time' %in% names(cohort$outputs[[oname]]$model)){
        # Making sure the time scale was defined
        if(!(cohort$outputs[[oname]]$model$time %in% names(cfg$options$time_scales))){
          isgood = FALSE 
          vp(cfg, sprintf('Error: For the output >%s< the specified model timescale >%s<', oname, cohort$outputs[[oname]]$model$time))
          vp(cfg, sprintf('       does not appear to have been defined in the system.txt file'))
          vp(cfg, sprintf('       <TS:%s> value ', cohort$outputs[[oname]]$model$time))
         }
       }
       else{
         isgood = FALSE 
         vp(cfg, sprintf('Error: For the output >%s<the model timescale must be specified', oname))
         vp(cfg, sprintf("       cohort$outputs$%s$model$time  = 'name'; ", oname))
       }

       #
       # Checking the values
       #
       if('value' %in% names(cohort$outputs[[oname]]$model)){
        # Making sure the output was defined
        if(!(cohort$outputs[[oname]]$model$value %in% names(cfg$options$mi$outputs))){
          isgood = FALSE 
          vp(cfg, sprintf('Error: For the output >%s< the specified model output >%s<', oname, cohort$outputs[[oname]]$model$value))
          vp(cfg, sprintf('       does not appear to have been defined in the system.txt file'))
          vp(cfg, sprintf('       <O> %s = value ', cohort$outputs[[oname]]$model$value))
         }
       }
       else{
         isgood = FALSE 
         vp(cfg, sprintf('Error: For the output >%s<the model output must be specified', oname))
         vp(cfg, sprintf("       cohort$outputs$%s$model$value  = 'name'; ", oname))
       }

       #
       # Checking the variance
       #
       if(!('variance' %in% names(cohort$outputs[[oname]]$model))){
       # JMH add logic' here
        isgood = FALSE 
        vp(cfg, sprintf('Error: For the output >%s< the model variance must be specified', oname))
        vp(cfg, sprintf("       cohort$outputs$%s$model$variance = 'PRED^2'; ", oname))
       }


     }
     else{
      isgood = FALSE 
      vp(cfg, sprintf('Error: For the output >%s< no model information was specified', oname))
      vp(cfg, sprintf("       cohort$outputs$%s$model$time  = 'name'; ", oname))
      vp(cfg, sprintf("       cohort$outputs$%s$model$value = 'name'; ", oname))
     }

    #
    # Checking the options. 
    #

    # setting output options to the default values
    output_options = defopts;

    if('options' %in% names(cohort$outputs[[oname]])){
     defoptnames = names(defopts)
     opoptnames  = names(cohort$outputs[[oname]]$options)

     # First we check to see if all of the specified options
     # are valid (e.g. they have default values). 
     for(optname in opoptnames){
       # if this option has been specified we overwrite it
       if(optname %in% defoptnames){
         output_options[[optname]] = cohort$outputs[[oname]]$options[[optname]]
       
       }
       else{
         vp(cfg, sprintf('Error: For output >%s< the specified option >%s< is invalid', oname, optname))
         vp(cfg, sprintf(' This option will be ignored')) }
     }
    
    }

    # overwriting options with the output specific options
    # determined above
    cohort$outputs[[oname]]$options = output_options
   }

 }
 else{
  isgood = FALSE
  vp(cfg, 'Error: No outputs were specified')
 }

 
# If everything checks out (dataset exists, columns specified for the
# outputs exists, etc.) If that's the case we extract the data from the datasets
if(isgood){
  # storing the cohort
  chvalues   = nm_select_records(cfg, tmpdataset$values, cohort$cf)
  choutput_times = c()
  
  #
  # We loop through each output and check the dataset for that output. We also
  # store the observation times/values as well as the corresponding simulation
  # times for performing estimation later
  #
  for(oname in names(cohort$outputs)){
    # if there is a filter for the current output then we apply it
    # otherwise we use all of the data for this output
    if('of' %in% names(cohort$outputs[[oname]])){
      opvalues   = nm_select_records(cfg, chvalues, cohort$outputs[[oname]]$of) }
    else{
      opvalues   = chvalues}

    # if the data for the given output is empty
    # then we notify the user
    if(length(opvalues[,1])==0){
     vp(cfg, sprintf('Unable to fetch observations:'));
     vp(cfg, sprintf('Cohort: %s',cohort$name));
     vp(cfg, sprintf('Output: %s',oname));
     vp(cfg, sprintf('Check the filters (cf, of), See:'));
     vp(cfg, sprintf('help system_define_cohort'));
     vp(cfg, sprintf('for more information'));
    }

    # pulling out all of the times and observations for 
    # this cohort/output combination
    tmpop = c()
    tmpop$time = opvalues[[cohort$outputs[[oname]]$obs$time]]
    tmpop$obs  = opvalues[[cohort$outputs[[oname]]$obs$value]]

    # Now we look at the data in the data file, first we check to make sure
    # it's numeric. If it is not numeric, we attempt to convert it to numeric
    # data and see if there are any NA values. If there are not, we just take
    # the numeric data forward. If there are NA variables we flip the isgood
    # flag.
    if(!is.numeric(tmpop$time)){
      if(any(is.na(as.numeric(as.character(tmpop$time))))){
        vp(cfg, 'Error the times (time) for the')
        vp(cfg, sprintf('Cohort: %s, Output: %s',cohort$name, oname))
        vp(cfg, 'Does not appear to be numeric, and attempts')
        vp(cfg, 'covert to numeric values have failed. This' )
        vp(cfg, 'cohort will not be added')
        isgood = FALSE
      } else{
       tmpop$time = as.numeric(as.character(tmpop$time))
      }
    }

    if(!is.numeric(tmpop$obs)){
      if(any(is.na(as.numeric(as.character(tmpop$obs))))){
        vp(cfg, 'Error the observations (obs) for the')
        vp(cfg, sprintf('Cohort: %s, Output: %s',cohort$name, oname))
        vp(cfg, 'Does not appear to be numeric, and attempts')
        vp(cfg, 'covert to numeric values have failed. This' )
        vp(cfg, 'cohort will not be added')
        isgood = FALSE
      } else{
       tmpop$obs = as.numeric(as.character(tmpop$obs))
      }
    }

    # if there are missing observations we exclude them here
    if('missing' %in% names(cohort$outputs[[oname]])){
       tmpop$time = tmpop$time[tmpop$obs != cohort$outputs[[oname]]$missing]
       tmpop$obs  =  tmpop$obs[tmpop$obs != cohort$outputs[[oname]]$missing]
    }

    # now we convert the time to the simulation timescale
    tmpop$simtime = system_ts_to_simtime(cfg, tmpop$time, cohort$outputs[[oname]]$model$time)

    # adding the observation times to the smooth output times
    choutput_times = unique(sort(c(tmpop$simtime, choutput_times)))

    # storing the data for the cohort/output 
    cohort$outputs[[oname]]$data = tmpop;
  }

  # storing all of the observation times for the cohort
  cohort$observation_simtimes = choutput_times
}


if(isgood){
  cohort$name = NULL
  cfg$cohorts[[cohort_name]] = cohort
}
else{
  vp(cfg, 'system_define_cohort()')
  vp(cfg, sprintf('Cohort name: >%s<', cohort_name))
  vp(cfg, 'There was an error and the cohort information was not set.')
}

  return(cfg)
  
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_od_general <- function(pest, cfg, estimation=TRUE){

od     = c()
odall  = c()
odpred = c() 

# If estimation is TRUE then we output a matrix 
# of observation details of the format:
#
#  od$pred  = [TIME, OBS, PRED, VAR, OUTPUT, COHORT]
#
#  The values are the observed (OBS) data, predicted
#  values (PRED) and variance (VAR) at the given time. The columns OUTPUT and
#  COHORT can be used for sorting. These should be unique numbers.
#
# When estimation is FALSE we output od$pred is a data frame with the
# following headings:
#
#  od$pred  = [TIME, OBS, PRED, VAR, SMOOTH, OUTPUT, COHORT]
#
#  The TIME, OBS, PRED and VAR are the same as those listed above. The SMOOTH
#  variable is FALSE for rows that correspond to records in the dataset and
#  TRUE when the PRED represents the smooth predictions. The OUTPUT and COHORT
#  columns here are text values used when defining the cohorts.
# 
# 
# Also the od$all list item is created with all of the simulation information
# stored for each cohort:
# 
#  od$all = [ts.time, ts.ts1, ... ts.tsn, pred, name, cohort]
#  
#  tstime             = timescale of the system
#  ts.ts1, ... ts.tsn = timescales defined in the system
#  pred               = smooth prediction
#  name               = state or output name corresponding to the prediction
#  cohort             = name of the cohort for these predicitons


chidx = 1
for(cohort_name in names(cfg$cohorts)){

  # Making a local cohort-specific copy of cfg
  chcfg = cfg 

  # pulling out the current cohort
  cohort =  cfg$cohorts[[cohort_name]]

  # Smooth output times
  # By default the output times will be those for the simulation
  choutput_times = cfg$options$simulation_options$output_times

  # If this cohort has a different set of output times then 
  # we overwrite the defaults
  if("output_times" %in% names(cohort$options)){
    choutput_times = cohort$options$output_times
  }

  # Adding all of the observation times to the output times to make sure the
  # simulations evaluate at these times
  choutput_times = sort(unique(c(choutput_times, cohort$observation_simtimes)))

  # Setting times to give a smooth profile, this will include the cohort
  # output times as well 
  chcfg=system_set_option(chcfg, group  = "simulation", 
                                 option = "output_times", 
                                 choutput_times)

  # Getting the full parameter vector
  chparameters = fetch_full_parameters(pest, chcfg) 

  # Overwriting cohort specific parameters
  if("cp" %in% names(cohort)){
    for(pname in names(cohort$cp)){
     chparameters =  system_set_parameter(cfg, chparameters, pname=pname, value = cohort$cp[[pname]])
    }
  }
  #
  # Setting up the inputs  
  #
  # zeroing out all events
  chcfg=system_zero_inputs(chcfg) 

  # Bolus inputs:
  if("bolus" %in% names(cohort$inputs)){
    for(bname in names(cohort$inputs$bolus)){
        chcfg=system_set_bolus(cfg    = chcfg,
                               state  = bname, 
                               times  = cohort$inputs$bolus[[bname]]$TIME,
                               values = cohort$inputs$bolus[[bname]]$AMT)
    }
  }

  # Infusion rates
  if("infusion_rates" %in% names(cohort$inputs)){
    for(iname in names(cohort$inputs$infusion_rates)){
        chcfg=system_set_rate( cfg    = chcfg,
                               rate   = iname, 
                               times  = cohort$inputs$infusion_rates[[iname]]$TIME,
                               levels = cohort$inputs$infusion_rates[[iname]]$AMT)
    }
  
  }

  # Covariates
  if("covariates" %in% names(cohort$inputs)){
    for(cname in names(cohort$inputs$covariates)){
        chcfg=system_set_covaraite( cfg       = chcfg,
                                    covariate = cname, 
                                    times     = cohort$inputs$covariates[[cname]]$TIME,
                                    values    = cohort$inputs$covariates[[cname]]$AMT)
    }
  }

  # Simulating the cohort  
  som = run_simulation_ubiquity(chparameters, chcfg, SIMINT_dropfirst=FALSE) 



  # sampling the different outputs for this cohort
  opidx = 1
  for(output in names(cohort$outputs)){


    # pulling out the timescale and output name for the current cohort/output
    output_ts   = cohort$outputs[[output]]$model$time
    output_name = cohort$outputs[[output]]$model$value


    odchunk      = list()

    # Stripping out missing vlaue
    if("missing" %in% names(cohort$outputs[[output]]$obs)){
      odchunk$TIME = cohort$outputs[[output]]$data$time[cohort$outputs[[output]]$data$obs  != cohort$outputs[[output]]$obs$missing]
      odchunk$OBS  = cohort$outputs[[output]]$data$obs [cohort$outputs[[output]]$data$obs  != cohort$outputs[[output]]$obs$missing]
    } else {
      odchunk$TIME = cohort$outputs[[output]]$data$time
      odchunk$OBS  = cohort$outputs[[output]]$data$obs
    }


    # sampling the model prediction at the times where we have observations
    odchunk$PRED = approx( x      = som$simout[[sprintf("ts.%s", output_ts)]], 
                           y      = som$simout[[output_name]], 
                           xout   = odchunk$TIME, 
                           method = "linear")$y

    # calculating the variance
    odchunk$VAR = calculate_variance(SIMINT_parameters = chparameters, 
                                     SIMINT_varstr     = cohort$outputs[[output]]$model$variance, 
                                     SIMINT_odchunk    = odchunk, 
                                     SIMINT_cfg        = chcfg)


    if(estimation){
       # For estimation we just create a matrix for the observations
       odchunk$output  = rep(opidx, length(odchunk$TIME))
       odchunk$cohort  = rep(chidx, length(odchunk$TIME))

      od_current       = cbind(odchunk$TIME, odchunk$OBS, odchunk$PRED, odchunk$VAR, odchunk$output, odchunk$cohort) 
      if(is.null(odpred)){
        odpred = od_current
      } else{
        odpred = rbind(odpred, od_current)
      }
    } else {
      # If estimation is valse we create a data frame with both the
      # observations and the predictions

      # Creating the rows for the observations
      odchunk$output  = rep(output, length(odchunk$TIME))
      odchunk$cohort  = rep(cohort_name, length(odchunk$TIME))
      od_current      = data.frame(TIME   = odchunk$TIME,
                                   OBS    = odchunk$OBS ,
                                   PRED   = odchunk$PRED,
                                   VAR    = odchunk$VAR, 
                                   SMOOTH = rep(FALSE, length(odchunk$TIME)),
                                   OUTPUT = odchunk$output,
                                   COHORT = odchunk$cohort)
      if(is.null(odpred)){
        odpred = od_current
      } else{
        odpred = rbind(odpred, od_current)
      }

      # Creating the rows for the smooth predictions
      od_current      = data.frame(TIME   = som$simout[[sprintf("ts.%s", output_ts)]], 
                                   OBS    = rep(-1,          length(som$simout[[sprintf("ts.%s", output_ts)]])), 
                                   PRED   = som$simout[[output_name]], 
                                   VAR    = rep(-1,          length(som$simout[[sprintf("ts.%s", output_ts)]])),
                                   SMOOTH = rep(TRUE,        length(som$simout[[sprintf("ts.%s", output_ts)]])),
                                   OUTPUT = rep(output,      length(som$simout[[sprintf("ts.%s", output_ts)]])),
                                   COHORT = rep(cohort_name, length(som$simout[[sprintf("ts.%s", output_ts)]])))

      odpred = rbind(odpred, od_current)
    }
  
  opidx = opidx + 1
  }

  # storing the smooth profiles for all of timescale, states and outputs
  if(!estimation){

    # for the current cohort we start with 
    # an empty list
    odall_cohort = som_to_df(cfg, som)

    # adding the cohort name
    odall_cohort$cohort = rep(cohort_name, length(odall_cohort[,1]))

      
    if(is.null(odall)){
      odall = odall_cohort 
    } else{
      odall = rbind(odall, odall_cohort )
    }
  }

chidx = chidx + 1
}

od$pred = odpred
od$all  = odall


return(od)

}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
fetch_full_parameters <- function(pest, cfg){
#
#  function [parameters_full] = fetch_full_parameters(parameters_subset, cfg) 
#
# This function is used to build a full parameter set from a subset, and is
# normally used during parameter estimation in the observation details
# function when the entire parameter vector is needed to simulate the system.
#
# The function select_set pulls out a parameter set and can optionally select
# only a subset for estimation:
#
#    pnames = c('Vp', 'CL')
#    cfg = system_select_set(cfg, "default", pnames)
#
# The default values of this subset can be accessed in the following way:
#
#    parameters_subset = cfg$estimation$parameters$guess 
#
# The estimation routines will work with this reduced parameter set, but to
# run simulations the full set is needed. The full values can be retrieved 
# using the following: 
#
# parameters_full = fetch_full_parameters(parameters_subset, cfg) 
#


parameters_full = cfg$parameters$values


for(pname in names(pest)){
   parameters_full[[pname]] = pest[[pname]] 
}

return(parameters_full)
}



#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_set_parameter <- function(cfg, parameters, pname, value){
# [parameters] = system_set_parameter(cfg, parameters, pname, value)
#
#  parameters = full parameter vector  obtained using the following:
#
#  pname = name of the parameter to set
#  value = value of the parameter
#
#
#  To set the parameter Vc to a value of 3, the following would be used:
#
#  parameters = system_fetch_parameters(cfg) 
#  parameters = system_set_parameter(cfg, parameters, pname = 'Vc', value = 3) 


if( pname %in% names(cfg$parameters$values)){
  parameters[[pname]] = value
} else {
  vp(cfg, 'system_set_parameter()') 
  vp(cfg, sprintf('parameter name (%s) not found', pname)) 
}

return(parameters)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
calculate_variance <- function(SIMINT_parameters, SIMINT_varstr, SIMINT_odchunk, SIMINT_cfg){

  SIMINT_var = c()
  if(SIMINT_varstr == "1"){
    SIMINT_var = rep(1, length(SIMINT_odchunk$OBS))
  
  } else{

    # Defining the parameters locally
    for(SIMINT_pname in names(SIMINT_parameters)){
      eval(parse(text=sprintf('%s = SIMINT_parameters[["%s"]] ', SIMINT_pname, SIMINT_pname)))
    }

    PRED       = SIMINT_odchunk$PRED
    OBS        = SIMINT_odchunk$OBS
    TIME       = SIMINT_odchunk$TIME
    SIMINT_var = eval(parse(text=SIMINT_varstr))
  
  }
  return(SIMINT_var)
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
run_simulation_ubiquity = function(SIMINT_parameters,SIMINT_cfg, SIMINT_dropfirst=TRUE){
# This runs a simulation for a model created in the system.txt format
#  
# # compile   the system to make sure the 
# # latest changes have been committed. 
# system('perl build_system.pl')
# 
# See the generated file:
#   transient/auto_simulation_driver.r
# for examples on how to control different aspects of the simulation. 

SIMINT_simulation_options = c()
# default simulation options 
SIMINT_simulation_options$solver                         = "lsoda"
SIMINT_simulation_options$output_times                   = seq(0,100,1)
SIMINT_simulation_options$include_important_output_times = "yes"
SIMINT_simulation_options$integrate_with                 = "r-file"
SIMINT_simulation_options$solver_opts$rtol               = 1e-6
SIMINT_simulation_options$initial_conditions             = NA  
SIMINT_simulation_options$parallel                       = "no"
SIMINT_simulation_options$compute_cores                  = 1

SIMINT_solver_opts = ""
if(length(SIMINT_cfg$options$simulation_options$solver_opts)>0){
  for(SIMINT_option in names(SIMINT_cfg$options$simulation_options$solver_opts)){
    SIMINT_solver_opts = sprintf("%s, %s=SIMINT_cfg$options$simulation_options$solver_opts$%s",SIMINT_solver_opts, SIMINT_option,SIMINT_option)
  }
}

# overriding the default simulation options
for(SIMINT_option in names(SIMINT_cfg$options$simulation_options)){
  if(is.null(SIMINT_simulation_options[[SIMINT_option]])){
    print(paste("Unknown simulation option", SIMINT_option))}
  else{
    SIMINT_simulation_options[[SIMINT_option]] = SIMINT_cfg$options$simulation_options[[SIMINT_option]] }
}


# It can be important to force the solver to evaluate 
# the system at specific times to make sure all events 
# are observed. The way bolus values are handled means 
# the system will be evaluated at each bolus event. However
# other events must be accounted for explicitly. This includes 
# the time varying inputs like infusion_rates and timevarying.
# The times these events occur are stored in the 
# important_times variable
SIMINT_important_times = SIMINT_simulation_options$output_times

# placing the parameters vector into cfg 
# because cfg is passed into the odes
SIMINT_cfg$parameters$values =  SIMINT_parameters

# setting up the nonzero initial conditions
# if the IC overide hasn't been specified then we set it using the system_IC
# function:
if(is.na(SIMINT_cfg$options$simulation_options$initial_conditions[1])){
  SIMINT_IC = system_IC(SIMINT_cfg, SIMINT_parameters) }
else{
  # otherwise we use the IC override 
  SIMINT_IC = SIMINT_cfg$options$simulation_options$initial_conditions }

# defining the parameters
for(SIMINT_parameter_names in names(SIMINT_parameters)){
  eval(parse(text=sprintf("%s = SIMINT_parameters$%s", SIMINT_parameter_names, SIMINT_parameter_names)))
}


# all forcing functions will be stored in SIMINT_forces
# this will be used with the compiled option
SIMINT_forces = c()

SIMINT_force_times = c()

# processing infusion rates
for(SIMINT_rate_name in names(SIMINT_cfg$options$inputs$infusion_rates)){
  # Looping through each infusion rate 
  # plucking out the rate name
  SIMINT_my_rate = SIMINT_cfg$options$inputs$infusion_rates[[SIMINT_rate_name]]



  SIMINT_rate_time_scale   = eval(parse(text=SIMINT_my_rate$times$scale))
  SIMINT_rate_values_scale = eval(parse(text=SIMINT_my_rate$levels$scale))

  # Adding times to the force_times vector to ensure state resets at these values
  SIMINT_force_times = c(SIMINT_force_times, SIMINT_my_rate$times$values*SIMINT_rate_time_scale)


  SIMINT_my_ff = make_forcing_function(SIMINT_my_rate$times$values*SIMINT_rate_time_scale,
                                       SIMINT_my_rate$levels$values*SIMINT_rate_values_scale,
                                       "step", SIMINT_rate_name,  
                                       SIMINT_simulation_options$output_times)
  
  eval(parse(text=sprintf("SIMINT_forces$%s = SIMINT_my_ff", SIMINT_rate_name)))

  # adding the time values to important times
  SIMINT_important_times =   c(SIMINT_my_ff[,1], SIMINT_important_times)
  
}


# processing covariates    
# JMH add force times for the covaraites
for(SIMINT_cv_name in names(SIMINT_cfg$options$inputs$covariates)){
  # Looping through each infusion rate 
  # plucking out the rate name
  SIMINT_my_cv = SIMINT_cfg$options$inputs$covariates[[SIMINT_cv_name]]

  # the full covariate (time varying component)
  SIMINT_my_ff = make_forcing_function(SIMINT_my_cv$times$values,
                                       SIMINT_my_cv$values$values,
                                       SIMINT_my_cv$cv_type, 
                                       SIMINT_cv_name,  
                                       SIMINT_simulation_options$output_times)
  eval(parse(text=sprintf("SIMINT_forces$%s = SIMINT_my_ff", SIMINT_cv_name)))
  # adding the time values to important times
  SIMINT_important_times =   c(SIMINT_my_ff[,1], SIMINT_important_times)

  # covariate evaluated at the initial condition and carried forward
  SIMINT_my_ff = make_forcing_function(SIMINT_my_cv$times$values[1],
                                       SIMINT_my_cv$values$values[1],
                                       SIMINT_my_cv$cv_type, 
                                       'step',  
                                       SIMINT_simulation_options$output_times)
  eval(parse(text=sprintf("SIMINT_forces$SIMINT_CVIC_%s = SIMINT_my_ff", SIMINT_cv_name)))
  # adding the time values to important times
  SIMINT_important_times =   c(SIMINT_my_ff[,1], SIMINT_important_times)

  # Adding times to the force_times vector to ensure state resets at these values
  SIMINT_force_times = c(SIMINT_force_times, SIMINT_my_cv$times$values)
  
}


# creating the bolus inputs
SIMINT_eventdata = system_prepare_inputs(SIMINT_cfg, SIMINT_parameters, SIMINT_force_times)
 
# adding sample times around the bolus times to the important times
SIMINT_important_times =   c(sample_around(SIMINT_eventdata$time, SIMINT_simulation_options$output_times), SIMINT_important_times)
 

# If important times were selected to be included then we set the output times
# equal to that vector (bounded on either end by the min and max of the
# selected simulation times).
if("yes" == SIMINT_simulation_options$include_important_output_times){
  SIMINT_important_times = SIMINT_important_times[(SIMINT_important_times >= min(SIMINT_simulation_options$output_times))  
                                                & (SIMINT_important_times <= max(SIMINT_simulation_options$output_times))]
  SIMINT_output_times_actual = sort(unique(SIMINT_important_times))
} else {
  SIMINT_output_times_actual = SIMINT_simulation_options$output_times}


# constructing the simulation command depending on the integrate_with option

if("r-file" == SIMINT_simulation_options$integrate_with){
# simulating the system using R
SIMINT_simcommand = 'SIMINT_simout = ode(SIMINT_IC, 
                                         SIMINT_output_times_actual,
                                         system_DYDT, SIMINT_cfg, 
                                         method=SIMINT_simulation_options$solver, 
                                         events=list(data=SIMINT_eventdata)'
SIMINT_simcommand = sprintf('%s %s)', SIMINT_simcommand, SIMINT_solver_opts)

#   tryCatch(
#    { 
#   eval(parse(text=SIMINT_simcommand))
#    },
#     warning = function(w) {
#     # place warning stuff here
#    },
#     error = function(e) {
#     browser()
#    })
#

                    
} else if("c-file" == SIMINT_simulation_options$integrate_with){

SIMINT_simcommand = ' SIMINT_simout <- ode(SIMINT_IC, SIMINT_output_times_actual, 
                                           func     = "derivs", 
                                           parms    = unlist(SIMINT_parameters),
                                           jacfunc  = NULL, 
                                           dllname  = "r_ode_model",
                                           initfunc = "initparams", 
                                           initforc = "initforcs",
                                           forcings = SIMINT_forces, 
                                           method   = SIMINT_simulation_options$solver, 
                                           nout     = length(names(SIMINT_cfg$options$mi$odes)), 
                                           events   = list(data=SIMINT_eventdata), 
                                           outnames = names(SIMINT_cfg$options$mi$odes)'
SIMINT_simcommand = sprintf('%s %s)', SIMINT_simcommand, SIMINT_solver_opts)
#eval(parse(text=SIMINT_simcommand))
#SIMINT_simout_mapped = system_map_output(SIMINT_cfg, SIMINT_simout, SIMINT_parameters, "c", SIMINT_eventdata)
}

# simulating the system
SIMINT_SIM_tic = proc.time()
eval(parse(text=SIMINT_simcommand))
SIMINT_SIM_toc = proc.time()

SIMINT_simout_mapped = list()

# In C all of the outputs are defined, for the r-file we have to define the
# outputs separately:
if("r-file" == SIMINT_simulation_options$integrate_with){
  SIMINT_MAP_tic = proc.time()
  SIMINT_simout  = system_map_output(SIMINT_cfg, SIMINT_simout, SIMINT_parameters, SIMINT_eventdata)
  SIMINT_MAP_toc = proc.time()
  # Adding the timing for the mapping
  SIMINT_simout_mapped$timing$output_mapping = SIMINT_MAP_toc - SIMINT_MAP_tic
} 
# Adding the timing for the simulations
SIMINT_simout_mapped$timing$simulation = SIMINT_SIM_toc - SIMINT_SIM_tic


# When R has a bolus applied at a certain time the state has the value before
# the bolus is applied. This means that a dose applied at time zero has serum
# levels that start at start at zero. We compensate for this by setting the
# reported state value to the value in the second time point. This shouldn't
# be a problem if 'include_important_output_times' is set to yes (default)
# because the system will automatically be sampled just _after_ each bolus.
#
# This is only done if the first output time corresponds to the first event
# time and can be overwritten if SIMINT_dropfirst is set to FALSE
if(SIMINT_dropfirst){
  if(SIMINT_simout[1,"time"] ==  SIMINT_eventdata[1,"time"]){
   SIMINT_simout=SIMINT_simout[-1,]
  }
}


# adding error to the output
SIMINT_ERR_tic = proc.time()
SIMINT_simout  = add_observation_errors(SIMINT_simout, SIMINT_parameters, SIMINT_cfg);
SIMINT_ERR_toc = proc.time()

SIMINT_simout_mapped$timing$adding_error   = SIMINT_ERR_toc - SIMINT_ERR_tic


# Adding the simout to the mapped output. Up until now all calculations and
# modifications should have been done in a matrix to speed things up
# and lastly we convert it to a data frame
SIMINT_simout_mapped$simout = as.data.frame(SIMINT_simout)

return(SIMINT_simout_mapped) } 

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
som_to_df  <- function(cfg, som){
# Takes the simulation output mapped from run_simulation_ubiquity  or
# simulate_subjects and converts it into a data frame of the formats:
#
#  Individual simulation (run_simulation_ubiquity)
#
#  somdf  = [ts.time, ts.ts1, ... ts.tsn, pred, tt.ti1.x, ..., name]
#
#  ts.time            = timescale of the system
#  ts.ts1, ... ts.tsn = timescales defined in the system
#  pred               = smooth prediction
#  tt.ti1.x           = titration event information
#  name               = state or output name corresponding to the prediction
#
#  Stochastic simulation (simulate_subjects)

df = c() 

  # We process things differently if it's an individual vs stochastic
  # simulation. First we check to see if som has a simout field (individual)
  # or a subjects field (stochastic)
  if("simout" %in% names(som)){
    # indivudal simulation

    # names of the outputs and the states in the system
    os_names = c(names(cfg$options$mi$outputs), names(cfg$options$mi$states))

    # Covariate: cfg$options$inputs$covariates
    #   -> timecourse        (CVNAME)
    #   -> initial condition (SIMINT_CVIC_CVNAME)
    # cfg$options$inputs$infusion_rates
    #   -> infusion rate     (RNAME)
    # dynamic secondary parameters: cfg$options$dsp
    # static secondary parameters:  cfg$options$ssp



    
    # pulling out the timescales, these are the columns that begin with 'ts.'
    ts_names = names(som$simout)
    ts_names = ts_names[grep('^ts.', ts_names)]
    
    dfos_str = 'dfos = data.frame('
    
    # defining the timescales
    for(ts in ts_names){
      dfos_str = sprintf('%s%s=som$simout$%s,', dfos_str, ts, ts)
    }
    dfos_str = sprintf('%spred=som$simout$SIMINTNAME,', dfos_str)

    # adding in titration information
    if("titration" %in% names(som)){
      for(tt in names(som$titration)){
        dfos_str = sprintf('%s%s=som$titration$%s,', dfos_str, tt, tt)
      }
    }
    dfos_str = sprintf('%sname=rep("SIMINTNAME", length(som$simout$time)))', dfos_str)
    
    for(os in os_names){
      dfos_eval = dfos_str
      dfos_eval = gsub('SIMINTNAME', os, dfos_eval)
      eval(parse(text=dfos_eval))
      if(is.null(df)){
        df = dfos
      } else {
        df = rbind(df, dfos)
      }
    }
    }
  else if("subjects" %in% names(som)){
    # population simulation
    
    # determining the number of subjects and the number of samples
    nsub = length(som$subjects$parameters[,1])
    nsam = length(som$times$time)


    # pulling out the timescale names
    ts_names = names(som$times)
    ts_names = ts_names[grep('^ts.', ts_names)]

    os_names = c(names(som$states), names(som$outputs))
    
    for(sub_idx in 1:nsub){

      # subjects parameters
      sub_p  = som$subjects$parameters[sub_idx, ]
      sub_sp = som$subjects$secondary_parameters[sub_idx, ]


      # For each output and state the subject has
      # we create a data frame and then stack it on
      # the main data frame
      for(os in os_names){
        dfos_str = 'dfos = data.frame('


        # Adding the subject ID
        dfos_str = sprintf('%sID= rep(sub_idx, nsam),', dfos_str)

        # Adding the timescales
        for(ts in ts_names){
          dfos_str = sprintf('%s%s=som$times$%s,', dfos_str, ts, ts)
          }

        # Adding the titration information
        if("titration" %in% names(som)){
          for(tt in names(som$titration)){
            dfos_str = sprintf('%s%s=som$titration$%s[sub_idx,],', dfos_str, tt, tt)
            }
          }

        # Adding the parameters
        for(sysp in names(sub_p)){
          dfos_str = sprintf('%s%s= rep(sub_p[["%s"]], nsam),', dfos_str, sysp, sysp)
          }

        # Adding the pred column
        if(os %in% names(cfg$options$mi$outputs)){
            dfos_str = sprintf('%spred=som$outputs$%s[sub_idx,],', dfos_str,os)
          }
        else if(os %in% names(cfg$options$mi$states)){
            dfos_str = sprintf('%spred=som$states$%s[sub_idx,],', dfos_str,os)
          }

          #dfos_str = sprintf('%s%s=som$titration$%s[sub_idx,],', dfos_str, tt, tt)

          # Adding the names column
          dfos_str = sprintf('%snames= rep("%s", nsam))', dfos_str, os)

          # Creating the data frame
          eval(parse(text=dfos_str))

          # Appending the df to the return df
          if(is.null(df)){
            df = dfos }
          else{
            df = rbind(df, dfos) }
        }
      }
    }

return(df)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
calculate_objective_pso <- function(pvect, cfg){
# calculate_objective takes the parameters as a list, so we take the vector
# provided by psoptim when it calls the objective function and repackage it as
# a named list.

  plist = list()
  pidx  = 1

  # coverting the vector into a list
  for(pname in names(cfg$estimation$parameters$guess)){
    plist[[pname]] = pvect[pidx]
    pidx = pidx +1
  }
  obj = calculate_objective(plist, cfg, estimation=TRUE)
  return(obj)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
calculate_objective_ga  <- function(pvect, cfg){
# calculate_objective takes the parameters as a list, so we take the vector
# provided by psoptim when it calls the objective function and repackage it as
# a named list.

  plist = list()
  pidx  = 1

  # coverting the vector into a list
  for(pname in names(cfg$estimation$parameters$guess)){
    plist[[pname]] = pvect[pidx]
    pidx = pidx +1
  }
  obj = calculate_objective(plist, cfg, estimation=TRUE)

  # Multiply by -1 because ga does maximization 
  return(-1*obj)
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
calculate_objective <- function(parameters, cfg, estimation=TRUE){


  errorflag = FALSE
  # We default to Inf in case the observation function below fails. This way 
  # if we move into a bad parameter space, the Inf will push it away from that
  # parameter space
  value     = Inf 

  # Bounding the parameters
  bv    = bound_parameters(pv = parameters, cfg = cfg)
  # pulling out the bounded parameters and objective multiplier
  parameters = bv$pv
  objmult   = bv$objmult



  # Trying to pull out the observations
  # if we fail we throw an error and flip the error flag
  tryCatch(
   { 
      eval(parse(text=sprintf('od = %s(parameters, cfg)', cfg$estimation$options$observation_function)))
   },
    warning = function(w) {
    # place warning stuff here
      eval(parse(text=sprintf('od = %s(parameters, cfg)', cfg$estimation$options$observation_function)))
   },
    error = function(e) {
    vp(cfg, sprintf(' -> unable to retrieve observations'))
    vp(cfg, sprintf(' -> possible causes:')) 
    vp(cfg, sprintf('      o cfg$estimation$options$options$observation_function is not defined'))
    vp(cfg, sprintf('      o odd parameter combinations sent to the'))
    vp(cfg, sprintf('        objective function during estimation '))
    vp(cfg, sprintf('        is causing problems '))
    vp(cfg, sprintf(' Error: %s ', e$message))
    errorflag = TRUE
    value = Inf 
   })
  
  # Sometimes the eval above fails and it doesn't trigger the error block
  # but when run outside of try catch it does work. 
  if(!exists('od') & !errorflag){
  eval(parse(text=sprintf('od = %s(parameters, cfg)', cfg$estimation$options$observation_function)))
  }


  if(!errorflag){


  tCcode     = '
    yobs = od$pred[,2]
    ypred= od$pred[,3]
    yvar = od$pred[,4]
  
    if(cfg$estimation$objective_type == "wls"){
      value = sum((ypred-yobs)^2*1/yvar)
    } else if(cfg$estimation$objective_type == "ml"){
      value = 1/2*sum((ypred-yobs)^2*1/yvar) + sum(log(yvar))

      # Constant portion of the negative log likelihood objective
      value = value + length(yobs)*log(2*pi)/2
    }
   
    # Parameter bounds, are handled by increasing the objective function due
    # to the parameter bounds being crossed.
    value = value*(1+objmult)

    # if the objective function is inf or NA we throw the error flag
    if(is.na(value) | is.infinite(value)){
      errorflag = TRUE } '



  tryCatch(
   { 
   eval(parse(text=tCcode))
   },
    warning = function(w) {
    # place warning stuff here
   eval(parse(text=tCcode))
   },
    error = function(e) {
    vp(cfg, sprintf(' Error: %s ', e$message))
    errorflag = TRUE
    value = Inf 
   })
  }

  # If we're in the estimation we return the objective function value
  # otherwise we return a structured output and any relevant errors
  if(estimation){
    return(value)
  }else{
    of = list()
    of$value = value 
    of$isgood = !errorflag
    if(errorflag){
      vp(cfg, 'calculate_objective failed')
      vp(cfg, sprintf('   Obj: %s ', toString(value)))
    }
    # Looking to see if there are any variance values that are NA:
    if(any(is.na(od$pred[,4]))){
      vp(cfg, '   Warning: variance values of NA')
    }

    # For those that are not NA we see if they are negative:
    if(any(od$pred[!(is.na(od$pred[,4])),4] < 0)){
      vp(cfg, '   Warning: variance values <= 0 ')
    }
    return(of)
  }


}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
bound_parameters  <- function(pv, cfg){
objmult = 0.0
  for(pname in names(pv)){
    # pulling out the bounds for the current parameter
    ub = cfg$estimation$parameters$matrix[cfg$estimation$parameters$matrix$name == pname, ]$upper_bound
    lb = cfg$estimation$parameters$matrix[cfg$estimation$parameters$matrix$name == pname, ]$lower_bound
    if(pv[[pname]] >  ub){
      # adding the multiplier
      objmult = objmult + 10*exp(abs(pv[[pname]] - ub))
      # fixing the parameter to the bound
      pv[[pname]] = ub
    } else if(pv[[pname]] < lb ) {
      # adding the multiplier
      objmult = objmult + 10*exp(abs(pv[[pname]] - lb))
      # fixing the parameter to the bound
      pv[[pname]] = lb
    }
  
  }

  # storing the bounds and objective multiplier
  # for bound violations
  bv = c()
  bv$objmult = objmult
  bv$pv = pv

  return(bv)
}


# system_estimate_parameters - controls the estimation process
# flowctl - controls the flow of the estimation process with the
#           following options:
#           'plot previous estimate'
#           'previous estimate as guess'
#           'estimate'
#           'plot guess'
# analysis_name - name used for archinving results
# archive_results - boolean variable to indicate if results should be saved
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_estimate_parameters <- function(cfg, 
                                       flowctl         = "plot guess",
                                       analysis_name   = "my_analysis", 
                                       archive_results = TRUE){

  if((flowctl == "estimate") | (flowctl == "previous estimate as guess")){
    # Checking the analysis_name
    name_check = ubiquity_name_check(analysis_name)
    if(!name_check$isgood){
      vp(cfg, sprintf('system_plot_cohorts()'))
      vp(cfg, sprintf('Error: the analyssis name >%s< is invalid', analysis_name))
      vp(cfg, sprintf('Problems: %s', name_check$msg))
      analysis_name = 'analysis'
      vp(cfg, sprintf('Instead Using: %s', analysis_name))
      }
  
    #loading the previous estimate and setting that as a guess
    if(flowctl == "previous estimate as guess"){
      load(file=sprintf('output%s%s.RData', .Platform$file.sep, analysis_name))
      vp(cfg, "Setting initial guess to previous solution")
      for(pname in names(cfg$estimation$parameters$guess)){
        cfg = system_set_guess(cfg, pname=pname, value=pest[[pname]]) }
    }
  
    # performing the estimation
    pe   = estimate_parameters(cfg)
    pest = pe$estimate
    save(pe, pest, file=sprintf('output%s%s.RData', .Platform$file.sep, analysis_name))
    if(archive_results){
      archive_estimation(analysis_name, cfg)
    }
  } else if(flowctl == "plot guess"){
    pest = system_fetch_guess(cfg)
  } else if(flowctl == "plot previous estimate"){
    vp(cfg, "Loading the previous solution")
    load(file=sprintf('output%s%s.RData', .Platform$file.sep, analysis_name))
  }

return(pest)}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
estimate_parameters <- function(cfg){

pest = c()

# calling calculate_ojective outside of the estimation scope to make sure 
# it is working properly
odtest = calculate_objective(cfg$estimation$parameters$guess, cfg, estimation=FALSE)

  if(odtest$isgood){
      vp(cfg,'------------------------------------------')
      vp(cfg,'Starting Estimation ')
      vp(cfg, sprintf('Parmaeters:          %s', paste(names(cfg$estimation$mi), collapse=", ")))
      vp(cfg, sprintf('Optimizer:           %s', cfg$estimation$options$optimizer))
      vp(cfg, sprintf('Method:              %s', cfg$estimation$options$method))
      vp(cfg, sprintf('Observation Detials: %s', cfg$estimation$options$observation_function))
      vp(cfg, sprintf('Integrating with:    %s', cfg$options$simulation_options$integrate_with))

      #
      # We perform the estimation depending on the optimizer selected 
      #
      if(cfg$estimation$options$optimizer %in% c('optim', 'optimx')){
        eval(parse(text=sprintf('p = %s(cfg$estimation$parameters$guess, 
                                        calculate_objective, 
                                        cfg     = cfg, 
                                        method  = cfg$estimation$options$method, 
                                        control = cfg$estimation$options$control)', 
                                        cfg$estimation$options$optimizer)))
      }
      else if(cfg$estimation$options$optimizer %in% c('pso')){
        p = psoptim(par     = as.vector(cfg$estimation$parameters$guess),
                    fn      = calculate_objective_pso, 
                    cfg     = cfg, 
                    lower   = cfg$estimation$parameters$matrix$lower_bound,
                    upper   = cfg$estimation$parameters$matrix$upper_bound,
                    control = cfg$estimation$options$control)
      
      }
      else if(cfg$estimation$options$optimizer %in% c('ga')){
        # par     = as.vector(cfg$estimation$parameters$guess),

        # This is a string of the control variables that the user passed on.
        # By default we have none (empty string):
        ctl_list = c(" ")

        # now we loop through each option and construct cs 
        if(!is.null(cfg$estimation$options$control)){
          for(cname in names(cfg$estimation$options$control)){
             ctl_list = c(ctl_list, sprintf("%s=cfg$estimation$options$control$%s", cname, cname))
            }
            ctl_str = paste(ctl_list, collapse=",\n ")
        }
        else{
          ctl_str  = "" }
        
          eval(parse(text=sprintf('p = ga(type    = "real-valued",
                                          fitness = calculate_objective_ga , 
                                          cfg     = cfg, 
                                          min     = cfg$estimation$parameters$matrix$lower_bound,
                                          max     = cfg$estimation$parameters$matrix$upper_bound%s)', ctl_str)))

      }

                        sprintf('p = %s(cfg$estimation$parameters$guess, 
                                        calculate_objective, 
                                        cfg     = cfg, 
                                        method  = cfg$estimation$options$method, 
                                        control = cfg$estimation$options$control)', 
                                        cfg$estimation$options$optimizer)
      
      vp(cfg,'Estimation Complete ')
      vp(cfg,'------------------------------------------')



    # because each optimizer returns solutions in a different format
    # we collect them here in a common structure
    # First we keep the 'raw' data
    pest$raw = p

    # estimate_ub is the unbound estimate
    if(cfg$estimation$options$optimizer == "optim"){
      pest$estimate_ub = p$par 
      pest$obj      = p$value
    } 
    else if(cfg$estimation$options$optimizer == "optimx"){
      pest$obj               = p$value
      for(pname in names(cfg$estimation$parameters$guess)){
        pest$estimate_ub[[pname]] = p[[pname]]
      }

    } 
    # Particle swarm (pso) 
    else if(cfg$estimation$options$optimizer %in% c("pso")){
      # Pso returns the parameters as a vector so we 
      # have to put it back into a list for the other functions
      pest$obj      = p$value
      pest$estimate_ub = list()
      pidx = 1
      for(pname in names(cfg$estimation$parameters$guess)){
        pest$estimate_ub[[pname]] = p$par[pidx]
        pidx = pidx+1
      }
    } 
    # Genetic algorithm (ga) output
    else if(cfg$estimation$options$optimizer %in% c("ga")){
       pest$obj = p@fitnessValue
       pest$estimation_ub = structure(rep(-1, length(cfg$estimation$parameters$guess)), 
                                      names=names(cfg$estimation$parameters$guess))
       pidx = 1
       for(pname in names(cfg$estimation$parameters$guess)){
         pest$estimate_ub[[pname]] = p@solution[pidx]
         pidx = pidx+1
       }
    }

    # Applying bound to estimate
    pest_bound    = bound_parameters(pv = pest$estimate_ub, cfg = cfg)
    pest$estimate = pest_bound$pv

   tCcode = '
      vp(cfg, "-----------------------------------")
      vp(cfg, "Calculating solution statistics. Be")
      vp(cfg, "patient this can take a while when ")
      vp(cfg, "there are many parameters          ")
      vp(cfg, "-----------------------------------")
      # Generating the solution statistics and writing the results to a file
      pest$statistics_est = solution_statistics(pest$estimate, cfg)
      files = generate_report(pest$estimate, pest$statistics_est, cfg)
      cat(files$report_file_contents, sep="\n")
      
      vp(cfg, "If you are happy with the results, the following")
      vp(cfg, "can be used to update system.txt file. Just copy, ")
      vp(cfg, "paste, and delete the previous entries")'

   tryCatch(
    { 
      eval(parse(text=tCcode))
    },
      warning = function(w) {
      eval(parse(text=tCcode))
    },
      error = function(e) {
        vp(cfg, "Solution statistics calculation failed")
        vp(cfg, "This can happen when you have a parameter")
        vp(cfg, "set that makes the system stiff.")
        vp(cfg, "The final parameter estimates are:")
    })




    # writing the system components to the screen
    # pstr = sprintf('%s%s', pstr, '\n')
      #name          value           lower  upper    units  editable grouping
      #                              bound  bound
      # <P> name       1.0            eps    inf      ???    yes      System

    for(pname in names(pest$estimate)){
      pindex = cfg$parameters$matrix$name == pname
      ptmp = c()
      ptmp$set_name  = cfg$parameters$current_set
      ptmp$value     = var2string(maxlength=12, nsig_f=5, nsig_e=5, var=pest$estimate[[pname]])
      ptmp$ptype     = toString(cfg$parameters$matrix$ptype[pindex])
      ptmp$type      = toString(cfg$parameters$matrix$type[pindex])
      ptmp$units     = toString(cfg$parameters$matrix$units[pindex])
      ptmp$lb_number =          cfg$parameters$matrix$lower_bound[pindex]
      ptmp$ub_number =          cfg$parameters$matrix$upper_bound[pindex]
      ptmp$editable  = toString(cfg$parameters$matrix$editable[pindex])

      #
      # setting the bounds 
      #
      if(ptmp$lb_number == .Machine$double.eps){
        ptmp$lb_text = 'eps' 
      }else if(ptmp$lb_number == -.Machine$double.xmax) {
        ptmp$lb_text = '-inf' 
      } else {
        ptmp$lb_text = toString(ptmp$lb_number)
      }

      if(ptmp$ub_number == .Machine$double.xmax){
        ptmp$ub_text = 'inf' 
      }else if(ptmp$ub_number == -.Machine$double.eps) {
        ptmp$ub_text = '-eps'  
      } else {
        ptmp$ub_text = toString(ptmp$ub_number)
      }



      if((ptmp$ptype == 'variance') | (ptmp$set_name == 'default') ){
        if(cfg$parameters$matrix$ptype[pindex] == 'variance'){
          pstr =  '<VP> '
        } else{
          pstr =  '<P> '
        }

        pstr = sprintf('%s %s', pstr, pad_string(pname, maxlength=20, location="end"))
        pstr = sprintf('%s%s', pstr, ptmp$value)
        pstr = sprintf('%s %s', pstr, pad_string(ptmp$lb_text,    maxlength=15))
        pstr = sprintf('%s %s', pstr, pad_string(ptmp$ub_text,    maxlength=15))
        pstr = sprintf('%s %s', pstr, pad_string(ptmp$units,      maxlength=10))
        pstr = sprintf('%s %s', pstr, pad_string(ptmp$editable,   maxlength=5))
        pstr = sprintf('%s %s', pstr, pad_string(ptmp$type,       maxlength=5))
      }else{
        pstr =  sprintf('<PSET:%s:%s> %s', ptmp$set_name, pname,  ptmp$value)
      
      }
    cat(pstr, "\n")
    }

  } else {
    vp(cfg, sprintf('The estimation was terminated. We were unable to   '))
    vp(cfg, sprintf('calculate the objective at the initial guess.'))
  }

  return(pest)
}

#-----------------------------------------------------------
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_simulate_estimation_results <- function(pest, cfg){
 eval(parse(text=sprintf('observations = %s(pest, cfg, estimation=FALSE)', cfg$estimation$options$observation_function)))
 #return(system_od_general(pest=pest, cfg=cfg, estimation=FALSE))
 return(observations)
}

#-----------------------------------------------------------
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_fetch_guess <- function(cfg){
  return(cfg$estimation$parameters$guess)
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_plot_cohorts <- function(erp, plot_opts=c(), cfg, prefix='analysis'){


# list of graphics objects to return
grobs = list()

def = c() 
def$yscale = "linear"
def$xlim  = NULL
def$ylim  = NULL

# These are the dimensions of the timecourse (tc) and observed vs predicted
# (op) figures that are generated
if(!is.null(plot_opts$tc$width)){
  def$dim$tc$width = plot_opts$tc$width
} else {
  def$dim$tc$width = 10 }

if(!is.null(plot_opts$tc$height)){
  def$dim$tc$height = plot_opts$tc$height
} else {
  def$dim$tc$height = 5.5 }

if(!is.null(plot_opts$op$width)){
  def$dim$op$width = plot_opts$op$width
} else {
  def$dim$op$width = 10 }

if(!is.null(plot_opts$op$height)){
  def$dim$op$height = plot_opts$op$height
} else {
  def$dim$op$height = 8.0 }

#def$dim$tc$height= 5.5
#def$dim$op$width = 10
#def$dim$op$height= 8


for(output in levels(erp$pred$OUTPUT)){

  if(is.null(plot_opts$outputs[[output]]$yscale)){
   plot_opts$outputs[[output]]$yscale = def$yscale }

  if(is.null(plot_opts$outputs[[output]]$ylim)){
   plot_opts$outputs[[output]]$ylim   = def$ylim }

  if(is.null(plot_opts$outputs[[output]]$xlim)){
   plot_opts$outputs[[output]]$xlim   = def$xlim }

  if(is.null(plot_opts$outputs[[output]]$xlabel)){
   plot_opts$outputs[[output]]$xlabel   = NULL }

  if(is.null(plot_opts$outputs[[output]]$ylabel)){
   plot_opts$outputs[[output]]$ylabel   = output }
}



#
# plotting each output on the same axis
#
for(output in levels(erp$pred$OUTPUT)){
  p = ggplot()
  color_string = c()
  output_scale = plot_opts$outputs[[output]]$yscale

  for(cohort in levels(erp$pred$COHORT)){
    # temporary dataset with the output and cohort
    tds = erp$pred[erp$pred$OUTPUT == output & erp$pred$COHORT == cohort, ]

    # we only want to plot if the output/cohort combination has data
    if(length(tds$TIME) > 0){
      
      # Separating out the sampled and smooth data
      SAMPLE= tds[!tds$SMOOTH, ]
      SMOOTH= tds[ tds$SMOOTH, ]
      
      # if we're operating on a log scale then we remove all the values
      # that are less than or equal to zero
      if(output_scale == "log"){
        SAMPLE = SAMPLE[SAMPLE$OBS  > 0,]
        SMOOTH = SMOOTH[SMOOTH$PRED > 0,]
      }
      
      co_options  = cfg$cohorts[[cohort]]$outputs[[output]]$options
      eval(parse(text = sprintf('p = p + geom_point(data=SAMPLE, aes(x=TIME, y=OBS), color="%s", shape=co_options$marker_shape,   size=2.0)', co_options$marker_color)))
      eval(parse(text = sprintf('p = p + geom_line( data=SMOOTH, aes(x=TIME, y=PRED, color="%s"), linetype=co_options$marker_line, size=0.9)',cohort)))
      
      if(is.null(color_string)){
        color_string = sprintf('"%s"="%s"', cohort, co_options$marker_color)
      } else{
        color_string = sprintf('%s, "%s"="%s"', color_string, cohort, co_options$marker_color)
      }
    }

  }

  # axis labels
  p = p+ ylab( plot_opts$outputs[[output]]$ylabel)
  if(is.null(plot_opts$outputs[[output]]$xlabel)){
    p = p + xlab(cfg$cohorts[[cohort]]$outputs[[output]]$model$time)
  } else {
    p = p + xlab(plot_opts$outputs[[output]]$xlabel) }

  # making the figure pretty
  p = prepare_figure(p, purpose="present")

  # x-axis limits
  if(!is.null(plot_opts$outputs[[output]]$xlim)){
    p = p + xlim(plot_opts$outputs[[output]]$xlim)
  }

  # assigning the colors
  eval(parse(text=sprintf('p = p + scale_colour_manual(values=c(%s))', color_string)))
  p = p + theme(legend.title = element_blank()) 
  p = p + theme(legend.position = 'bottom')     

  # Y scale
  if(output_scale == "log"){
    if(!is.null(plot_opts$outputs[[output]]$ylim)){
     #  p = p + scale_y_log10(limits=plot_opts$outputs[[output]]$ylim)
        p =  gg_log10_yaxis(fo       = p,
                            ylim_min = min(plot_opts$outputs[[output]]$ylim),
                            ylim_max = max(plot_opts$outputs[[output]]$ylim))
    } else {
     #  p = scale_y_log10()
       p = gg_log10_yaxis(p)

    }
  } else {
    # if the yscale isn't log and there are ylim specified
    if(!is.null(plot_opts$outputs[[output]]$ylim)){
      p = p + ylim(plot_opts$outputs[[output]]$ylim) } 
  }





  fname_pdf = sprintf('output%s%s_timecourse_%s.pdf', .Platform$file.sep, prefix, output )
  ggsave(fname_pdf, plot=p, height=def$dim$tc$height, width=def$dim$tc$width)
  vp(cfg, sprintf('Figure written: %s', fname_pdf))

  fname_png = sprintf('output%s%s_timecourse_%s.png', .Platform$file.sep, prefix, output )
  ggsave(fname_png, plot=p, height=def$dim$tc$height, width=def$dim$tc$width)
  vp(cfg, sprintf('Figure written: %s', fname_png))

  # storing the plot object to be returned to the user
  eval(parse(text=sprintf('grobs$timecourse$%s     = p',         output)))
  eval(parse(text=sprintf('grobs$timecourse$%s_png = fname_png', output)))
  eval(parse(text=sprintf('grobs$timecourse$%s_pdf = fname_pdf', output)))
}

#
# creating the observed vs predicted plot
#

for(output in levels(erp$pred$OUTPUT)){
  p = ggplot()
  color_string = c()
  output_scale = plot_opts$outputs[[output]]$yscale


  for(cohort in levels(erp$pred$COHORT)){
    # temporary dataset with the output and cohort
    tds = erp$pred[erp$pred$OUTPUT == output & erp$pred$COHORT == cohort, ]

    nrow(tds)
    if(length(tds$TIME) > 0){
      # getting the sample data
      SAMPLE= tds[!tds$SMOOTH, ]
      
      # if we're operating on a log scale then we remove all the values
      # that are less than or equal to zero
      if(output_scale == "log"){
        SAMPLE = SAMPLE[SAMPLE$OBS  > 0,]
      }
      
      co_options  = cfg$cohorts[[cohort]]$outputs[[output]]$options
      eval(parse(text = sprintf('p = p + geom_point( data=SAMPLE, aes(x=PRED, y=OBS, color="%s"), shape=co_options$marker_shape, size=2.0)',cohort)))

      if(is.null(color_string)){
        color_string = sprintf('"%s"="%s"', cohort, co_options$marker_color)
      } else{
        color_string = sprintf('%s, "%s"="%s"', color_string, cohort, co_options$marker_color)
      }
    }

  }

  # getting all of the observation data (not smooth) for the output
  opds = erp$pred[(erp$pred$OUTPUT == output) & !erp$pred$SMOOTH, ]
  # if we're working on a log scale we strip out all of the nonzero values
  if(output_scale == "log"){
     opds = opds[opds$PRED>0 & opds$OBS>0,] }
  
  # overlaying the line of identity
  LIDENT = data.frame(OBS  = c(min(opds$PRED, opds$OBS), max(opds$PRED, opds$OBS)),
                      PRED = c(min(opds$PRED, opds$OBS), max(opds$PRED, opds$OBS)))
  p = p + geom_line(data=LIDENT, aes(x=PRED, y=OBS), color='black')

  
  # setting the title to the output label
  p = p+ ggtitle(plot_opts$outputs[[output]]$ylabel)

  # moving the legend to the bottom
  p = p + theme(legend.title = element_blank()) 
  p = p + theme(legend.position = 'bottom')     

  if(output_scale == "log"){
    p = p + scale_x_log10()
    p = p + scale_y_log10()}

  p = prepare_figure(p, purpose="present")
  eval(parse(text=sprintf('p = p + scale_colour_manual(values=c(%s))', color_string)))

  fname_pdf = sprintf('output%s%s_obs_pred_%s.pdf', .Platform$file.sep, prefix, output )
  ggsave(fname_pdf, plot=p, height=def$dim$op$height, width=def$dim$op$width)
  vp(cfg, sprintf('Figure written: %s', fname_pdf))

  fname_png = sprintf('output%s%s_obs_pred_%s.png', .Platform$file.sep, prefix, output )
  ggsave(fname_png, plot=p, height=def$dim$op$height, width=def$dim$op$width)
  vp(cfg, sprintf('Figure written: %s', fname_png))


  # storing the plot object to be returned to the user
  eval(parse(text=sprintf('grobs$obs_pred$%s     = p',         output)))
  eval(parse(text=sprintf('grobs$obs_pred$%s_png = fname_png', output)))
  eval(parse(text=sprintf('grobs$obs_pred$%s_pdf = fname_pdf', output)))

}


return(grobs)

}

#'@export
#'@title Alter Initial Guess and Parameter Bounds
#'@description
#'
#' Default values for parameters are taken from the \code{system.txt} file
#' either when the parameter was defined (\code{<P>}) or when it was reassigned
#' for a parameter set (\code{<PSET:?:?>?}). These can be altered at the
#' scripting level using this function.
#'
#'@param cfg ubiquity system object    
#'@param pname name of parameter to set
#'@param value value to assign
#'@param lb optionally change the lower bound
#'@param ub optionally change the upper bound
#'
#'@return cfg - ubiquity system object    
#'
#' @details 
#'
#' When performing a parameter estimation, the initial guess will be the value
#' specified in the \code{system.txt} file for the currently selected parameter set. The
#' following command can be used after the parameter set has been selected to
#' specify the value (\code{VALUE}) of the parameter \code{PNAME} and optionally the lower (\code{lb})
#' and upper (\code{ub}) bounds:
#' 
#' \preformatted{
#' cfg =
#' system_set_guess(cfg, pname="PNAME", value=VALUE, lb=NULL, ub=NULL)
#' }
#'
#' To set the initial guess for the parameter Vc to a value of 3, the following
#' would be used:
#' 
#' \preformatted{
#' cfg = system_set_guess(cfg, "Vc", value=3)
#' }
#'
#' To specify the guess and overwrite the upper bound on Vc and set it to 5
#'
#' \preformatted{
#' cfg = system_set_guess(cfg, "Vc", value=3, ub=5)
#' }
#'
#'@examples
#' # Examples
system_set_guess <- function(cfg, pname, value, lb=NULL, ub=NULL){
#  cfg = system_set_guess(cfg,  pname, value, lb, ub)
#
#  pname = name of the parameter to set
#  value = value of the guess for the parameter
#  lb = lower bound (optional)
#  ub = upper bound (optional)
#
#  To set the initial guess for the parameter Vc to a value of 3, the
#  following would be used:
#
#  cfg = system_set_guess(cfg, pname='Vc', value=3);
#
#  To specify the guess and overwrite the upper bound on Vc and set it to 5
#
#  cfg = system_set_guess(cfg, pname='Vc', value=3, ub=5);
#

isgood = TRUE


if(pname %in% names(cfg$parameters$values)){
  if(pname %in% names(cfg$estimation$parameters$guess)){
    # setting the guess
    cfg$estimation$parameters$guess[[pname]] = value
    # setting the bounds as well
    if(!is.null(lb)){
      cfg$estimation$parameters$matrix[cfg$estimation$parameters$matrix$name == pname, "lower_bound"] = lb }
    if(!is.null(ub)){
      cfg$estimation$parameters$matrix[cfg$estimation$parameters$matrix$name == pname, "upper_bound"] = ub }
  } else {
    isgood = FALSE
    vp(cfg, sprintf('parameter name (%s) was not selected for estimation', pname))
    vp(cfg, sprintf('see help for system_select_set '))
  }

} else{
  isgood = FALSE
  vp(cfg, sprintf('parameter name (%s) not found', pname))
}

if(isgood == FALSE){
  vp(cfg, 'system_set_guess()')
}

return(cfg)

}



#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
generate_report  <- function( parameters, ss, cfg){


parameters_full = fetch_full_parameters(cfg=cfg, pest=parameters)

report_file         = sprintf('output%sreport.txt', .Platform$file.sep) 
parameters_all_file = sprintf('output%sparameters_all.csv',.Platform$file.sep )
parameters_est_file = sprintf('output%sparameters_est.csv', .Platform$file.sep) 

#p_all    c('pname' 'guess'  'estimate' 'cvpct' 'cilb' 'ciub' 'units' 'notes')


notes_str = 'F=Fixed parameter, L=estimate at/near lower bound, U=estimate at/near upper bound'; 
cn =  c('pname', 'guess',  'estimate', 'cvpct', 'cilb', 'ciub', 'units', 'notes')

p_all = matrix(data=0, nrow= length(cfg$parameters$values)+1, ncol=8)
p_est = matrix(data=0, nrow= length(parameters)+1, ncol=8)
colnames(p_est) =  cn
colnames(p_all) =  cn

#
# making p_est
#
pidx = 1
for(pname in names(parameters)){

  guess    = cfg$estimation$parameters$guess[pname]
  estimate = var2string(parameters[[pname]], 1)
  cvpct    = var2string(ss$coefficient_of_variation[[pname]],1)
  cilb     = var2string(ss$confidence_interval$lower_bound[[pname]],1) 
  ciub     = var2string(ss$confidence_interval$upper_bound[[pname]],1) 
  units    = toString(cfg$parameters$matrix$units[cfg$parameters$matrix$name == pname])
  notes    = compare_estimate(cfg, parameters, pname);
  p_est[pidx, ] = c(pname, guess, estimate, cvpct, cilb , ciub, units, notes)

pidx = pidx+1
}
p_est[pidx, ] = c(notes_str, '', '','', '','', '','')



#
# making p_all
#
pidx = 1
for(pname in names(parameters_full)){

  units    = toString(cfg$parameters$matrix$units[cfg$parameters$matrix$name == pname])

  if(pname %in% names(parameters)){
    guess    = cfg$estimation$parameters$guess[pname]
    estimate = var2string(parameters[[pname]], 1)
    cvpct    = var2string(ss$coefficient_of_variation[[pname]],1)
    cilb     = var2string(ss$confidence_interval$lower_bound[[pname]],1) 
    ciub     = var2string(ss$confidence_interval$upper_bound[[pname]],1) 
    notes    = compare_estimate(cfg, parameters, pname);
  } else {
    guess    =  cfg$parameters$values[[pname]]
    estimate = '---'
    cvpct    = '---'
    cilb     = '---'
    ciub     = '---'
    notes    = 'F'
  
  }
  p_all[pidx, ] = c(pname, guess, estimate, cvpct, cilb , ciub, units, notes)

pidx = pidx+1
}
p_all[pidx, ] = c(notes_str, '', '','', '','', '','')



#
# making report
#                          v                                                                        
#                1         2         3         4         5         6         7         8         9
#       1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
rl = c('                       Estimate     95 % Confidence Interval     Coeff. of Var  Notes ', 
       '                                  Lower Bound    Upper Bound      (Percent)')     
# Creating parameter entries
for(pname in names(cfg$parameters$values)){
  
  pstr = pad_string(maxlength=20, str=pname)
  if(pname %in% names(parameters)){
    pstr = sprintf('%s%s    ',   pstr, var2string(maxlength=10, var=parameters_full[[pname]]))
    pstr = sprintf('%s%s    ',   pstr, var2string(maxlength=11, var=ss$confidence_interval$lower_bound[[pname]]))
    pstr = sprintf('%s%s      ', pstr, var2string(maxlength=11, var=ss$confidence_interval$upper_bound[[pname]]))
    pstr = sprintf('%s%s     ',  pstr, var2string(maxlength=8,  var= ss$coefficient_of_variation[[pname]]))
    pstr = sprintf('%s%s',       pstr, pad_string(maxlength=3,  str=compare_estimate(cfg, parameters, pname))) 
  } else {
    pstr = sprintf('%s%s', pstr, pad_string(maxlength=10, str=toString(parameters_full[[pname]])))
    pstr = sprintf('%s%s', pstr, pad_string(maxlength=52, str='F'))
  }

rl = c(rl, pstr)
}
rl = c(rl, '---', notes_str)
rl = c(rl, '', '', '',
       'Variance -- Covariance Matrix ', '')

row_str = pad_string(maxlength=20, str='')
for(pname_c in names(parameters)){
  row_str = sprintf('%s%s', row_str, pad_string(maxlength=20, str=pname_c))
  }
rl = c(rl, row_str)


ridx = 1;
for(pname_r in names(parameters)){
  cidx = 1
  row_str = pad_string(maxlength=20, str=pname_r)
  for(pname_c in names(parameters)){
    if(ridx >= cidx){
      row_str = sprintf('%s%s', row_str, var2string(maxlength=20, var=ss$covariance[ridx,cidx]))
    }
    cidx = cidx+1
  }
  rl = c(rl, row_str)
  ridx = ridx+1
}
# Creating variance /covariance matrix


rl = c(rl, '', '', '',
       'Misc Information')
rl = c(rl, sprintf('OBJ = %s', var2string(maxlength=1, var=ss$objective)))
rl = c(rl, sprintf('AIC = %s', var2string(maxlength=1, var=ss$aic)))
rl = c(rl, sprintf('BIC = %s', var2string(maxlength=1, var=ss$bic)))

fileConn<-file(report_file)
writeLines(rl, fileConn)
close(fileConn)

# writing csv files
write.csv(p_all, file=parameters_all_file, row.names=FALSE)
write.csv(p_est, file=parameters_est_file, row.names=FALSE)

vp(cfg,         'Report generated and placed in: ')
vp(cfg, sprintf('   %s', report_file))
vp(cfg,         'Estimated parameter information ')
vp(cfg,         'summarized in CSV format: ')
vp(cfg, sprintf('   %s', parameters_est_file))
vp(cfg,         'All parameter information ')
vp(cfg,         'summarized in CSV format: ')
vp(cfg, sprintf('   %s', parameters_all_file))



files                     = list()
files$report_file         = report_file         
files$report_file_contents= rl
files$parameters_all_file = parameters_all_file 
files$parameters_est_file = parameters_est_file 


return(files)

}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
archive_estimation <- function(name, cfg){
#
#  archive_estimation(name, cfg)
#
#  Archives the estimation results by moving the output files to the same file
#  names with 'name' prepended to them. This prevents them from being
#  overwritten in a different analysis script the following files are
#  archived:
#
#   output/parameters_all.csv             
#   output/parameters_est.csv             
#   output/report.txt                     
#
#  Example:
#   archive_estimation('mysoln', cfg)
#
#   Would rename the files above 
#   output/mysoln-parameters_all.csv             
#   output/mysoln-parameters_est.csv             
#   output/mysoln-report.txt                     
#


f.source      = c()
f.destination = c()


f.source      = c(f.source,      sprintf('output%sparameters_all.csv',   .Platform$file.sep))
f.source      = c(f.source,      sprintf('output%sparameters_est.csv',   .Platform$file.sep))
f.source      = c(f.source,      sprintf('output%sreport.txt'        ,   .Platform$file.sep))

f.destination = c(f.destination, sprintf('output%s%s-parameters_all.csv', .Platform$file.sep, name))
f.destination = c(f.destination, sprintf('output%s%s-parameters_est.csv', .Platform$file.sep, name))
f.destination = c(f.destination, sprintf('output%s%s-report.txt'        , .Platform$file.sep, name))

vp(cfg, "Archiving the estimation results")
for(fidx in 1:length(f.source)){ 
   file.copy(f.source[fidx], f.destination[fidx], overwrite=TRUE)
   vp(cfg, sprintf('%s --> %s', f.source[fidx], f.destination[fidx]))
}
}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
compare_estimate <- function(cfg, parameters, pname){
#
# ceecking to see if the estimated parameter pname with the value in the
# parameters vector is close to the upper or lower bounds in
# cfg.parameters.upper_bound or cfg.parameters.lower_bound)
#

  notes = ''

  pvalue      = parameters[[pname]]
  lower_bound = cfg$estimation$parameters$matrix$lower_bound[cfg$estimation$parameters$matrix$name == pname]
  upper_bound = cfg$estimation$parameters$matrix$upper_bound[cfg$estimation$parameters$matrix$name == pname]

  lower_diff = abs(lower_bound - pvalue)
  upper_diff = abs(upper_bound - pvalue)
  
  if(is.finite(lower_bound)){
    if(lower_diff/lower_bound  <0.05){
      notes = 'L'
    }
  }

  if(is.finite(upper_bound)){
    if(upper_diff/upper_bound  <0.05){
      notes = 'U'
    }
  }

  
return(notes)
}



#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
solution_statistics <- function(parameters, cfg){

  RelTol = 1e-5;
  AbsTol = 1e-8;
  
  # solution statistics
  s = list()
  # calculating the perturbations to the parameters
  perturbation = list()
  for(pname in names(parameters)){
    perturbation[[pname]] = max(c(abs(parameters[[pname]]), AbsTol))*RelTol
  }

  
  # Getting the observations at the estimate
  eval(parse(text=sprintf("observations = %s(parameters, cfg)", cfg$estimation$options$observation_function)))
  
  # Getting the objective at the estimate
  objective=calculate_objective(parameters,cfg)
  
  perturbations_plus = list()
  perturbations_minus= list()
  
  for(pname in names(parameters)){
  
    # Creating a vector of parameters with _only_ the current parameter
    # (pname) perturbated in the positive direction and calculating the
    # observations with that perturbation
    perturbations_plus[[pname]]$parameters            = parameters
    perturbations_plus[[pname]]$parameters[[pname]]   = perturbations_plus[[pname]]$parameters[[pname]] + perturbation[[pname]]
    eval(parse(text=sprintf("perturbations_plus[[pname]]$observations = %s(perturbations_plus[[pname]]$parameters,cfg)",cfg$estimation$options$observation_function)))
  
    # doing the same thing for the minus
    perturbations_minus[[pname]]$parameters           = parameters
    perturbations_minus[[pname]]$parameters[[pname]]  = perturbations_minus[[pname]]$parameters[[pname]] - perturbation[[pname]]
    eval(parse(text=sprintf("perturbations_minus[[pname]]$observations = %s(perturbations_minus[[pname]]$parameters,cfg)",cfg$estimation$options$observation_function)))
  
  }



  # observatinos$pred format:
  #  1     2    3     4    5       6    
  # [TIME, OBS, PRED, VAR, OUTPUT, COHORT] 

  num_observations = nrow(observations$pred)
  num_parameters   = length(parameters)
  all_outputs      = unique(observations$pred[,5])
  all_cohorts      = unique(observations$pred[,6])

  # general statistics
  s$num_observations       = num_observations
  s$objective              = objective
  s$degrees_of_freedom     = num_observations - num_parameters
  if('wls' == cfg$estimation$objective_type){
    s$aic                    = num_observations*log(objective) + 2.0*num_parameters
    s$bic                    = num_observations*log(objective) + log(num_observations)*num_parameters
  }
  else if('ml' == cfg$estimation$objective_type){
    s$aic                    = 2.0*objective + 2.0*num_parameters
    s$bic                    = 2.0*objective + log(num_observations)*(num_parameters)
  }

  if('wls' == cfg$estimation$objective_type){
    # Calculating the weighted least squares solution statistics 
    # temporary variables to contain the Jacobian (P), matrix of weights (W),
    # and the error variance matrix (G)
    P = matrix(data=0, nrow=num_observations, ncol=num_parameters)
    # W = matrix(data=0, nrow=num_observations, ncol=1)
    W = as.matrix(1/observations$pred[,4])
    G = matrix(data=0, nrow=num_observations, ncol=1)

    # Populating the Jacobian
    pctr = 1;
    for(pname in names(parameters)){
     deltaoutput_plus  = perturbations_plus[[pname]]$observations$pred[,3]
     deltaoutput_minus = perturbations_minus[[pname]]$observations$pred[,3]
     partiald = (deltaoutput_plus - deltaoutput_minus)/(2*perturbation[[pname]])
     P[,pctr] = partiald
     pctr = pctr+1
    }

    # Going through each output and calculating the 
    # variance matrix components (G)
    for(octr in 1:length(all_outputs)){
      current_output = all_outputs[octr]

      # index of the current outputs
      oidx = observations$pred[,5] == current_output

      # calculating the number of degrees of freedom (dfi)
      # Equation 3.26
      # df = mi - num_p/num_l 
      #    mi     = number of nonzero observations for the current output
      #    num_p  = number of parameters being estimated
      #    num_l  = number of outputs in the model
      mi        = sum(as.integer(oidx))
      num_p     = length(names(parameters)) 
      num_l     = length(all_outputs)
      df_output = mi - num_p/num_l

      # Calculating the variance for output i 
      # (sigma^2_i from Equation 3.26)
      weight_output   = observations$pred[oidx, 4]
      obs_output      = observations$pred[oidx, 2]
      pred_output     = observations$pred[oidx, 3]

      # calculating the variance for the given output
      variance_output = sum((obs_output-pred_output)^2/weight_output)/df_output

      G[oidx,1] = variance_output
    }

    # Creating a diagional matrix from W and G
    # For example:
    #
    #                        | 1     0     0  |
    # W = [1 2 3]  ----> W = | 0     2     0  |
    #                        | 0     0     3  |
    G = diag(as.numeric(G))
    W = diag(as.numeric(W))

    s$wls$jacobian           = P
    s$wls$weights            = W
    s$wls$error_variance     = G
    s$covariance             = solve(t(P)%*%W%*%P)%*%(t(P)%*%W%*%G%*%W%*%P)%*%solve(t(P)%*%W%*%P)

  
  } else if('ml' == cfg$estimation$objective_type){
    # M has the following structure:
    #     _                                 _    
    #    |                     ^             |        
    #    |                     :             |        
    #    |                     :             |        
    #    |         MI        p :     MIII    |        
    #    |                     :             |        
    #    |                     :             |        
    #    |       p             v             |        
    #    |<...................>.<...........>|        
    #    |                     ^      q      |        
    #    |                     :             |        
    #    |         MIII        :q    MII     |        
    #    |                     :             |        
    #    |_                    v            _|        
    #                                                     
    #    Where p is the number of system parameters and 
    #    q is the number of variance parameters
    #                                                     
    #    With the three block components MI, MII and MIII     
    M     = matrix(data=0, nrow=num_parameters, ncol=num_parameters)
    dim.p = cfg$estimation$parameters$system
    dim.q = num_parameters - dim.p

        # model variance
    outg = observations$pred[,4]
    jidx = 1
    for(pname_j in names(parameters)){
      kidx = 1
      for(pname_k in names(parameters)){

        # model prediction
        # partial y/partial theta_j
        partials.yj = (perturbations_plus[[pname_j]]$observations$pred[,3] -  perturbations_minus[[pname_j]]$observations$pred[,3])/(2*perturbation[[pname_j]])
        partials.yk = (perturbations_plus[[pname_k]]$observations$pred[,3] -  perturbations_minus[[pname_k]]$observations$pred[,3])/(2*perturbation[[pname_k]])
      
        # model variance
        # partial g/partial theta_j   
        partials.gj = (perturbations_plus[[pname_j]]$observations$pred[,4] -  perturbations_minus[[pname_j]]$observations$pred[,4])/(2*perturbation[[pname_j]])
        partials.gk = (perturbations_plus[[pname_k]]$observations$pred[,4] -  perturbations_minus[[pname_k]]$observations$pred[,4])/(2*perturbation[[pname_k]])
        if( (jidx <= dim.p)& (kidx <=dim.p)){
          #                                                     
          # Section MI
          #                                                     
          M[jidx,kidx] = M[jidx,kidx] + 1/2*sum(partials.gj*partials.gk/outg^2);
          M[jidx,kidx] = M[jidx,kidx] +     sum(partials.yj*partials.yk/outg);
        } else{
          #                                                    
          # Sections MII and MIII 
          #                                                    
          M[jidx,kidx] = M[jidx,kidx] + 1/2*sum(partials.gj*partials.gk/(outg)^2);
        }
        kidx = kidx+1
      }
      jidx = jidx+1
    }

    s$ml$M       = M
    s$covariance = solve(M)

  } else {
    vp(cfg, sprintf("Unknown objective_type '%s' ", cfg$estimation$objective_type))
    vp(cfg,         "Valid types are 'wls' - weighted least squares  ")
    vp(cfg,         "                'ml'  - maximum likelihood      ")
    return()
  
  }


  # Calculating the cv% and confidence intervals
  pctr = 1;
  for(pname in names(parameters)){
    s$coefficient_of_variation[[pname]] = 100*sqrt(s$covariance[pctr, pctr])/as.numeric(parameters[pname])
    s$confidence_interval$lower_bound[[pname]] = parameters[[pname]] - sqrt(s$covariance[pctr, pctr])*qt(.975,s$degrees_of_freedom)
    s$confidence_interval$upper_bound[[pname]] = parameters[[pname]] + sqrt(s$covariance[pctr, pctr])*qt(.975,s$degrees_of_freedom)
    
    pctr = pctr + 1
  }

  return(s)
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
check_steady_state  <- function(cfg, som){ 

  offset_found = FALSE

  for(sname in names(cfg$options$mi$states)){
     state = som$simout[[sname]]

     state_max = max(abs(state))
     
     # if the state has a value other than zero 
     # we look at it a little more closely
     if(state_max > 0){
       offset = abs(range(state)[2]-range(state)[1])
       if( offset/state_max > 100*.Machine$double.eps){
         if(!offset_found){
           vp(cfg, sprintf('#> Possible steady state offset'))
           vp(cfg, sprintf('#> range       |             | state'))
           vp(cfg, sprintf('#> (max-min)   | max(abs(s)) | name '))
           vp(cfg, sprintf('#>------------------------------------'))
           offset_found = TRUE  
        }
        vp(cfg, sprintf('#> %.3e   | %.3e   | %s', offset, state_max, sname))
       
       }
     }
  }

}


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
prepare_figure = function(purpose, fo,
                          y_tick_minor = FALSE,
                          y_tick_major = FALSE,
                          x_tick_minor = FALSE,
                          x_tick_major = FALSE){
#
# Takes a ggplot figure object and removes some of the accoutrements and
# adjusts line thicknesses and what not to make it more appropriate for
# different outputs
#= element_line(color = "gray60", size = 0.8)


  mj_tick_color = "gray85"
  mj_tick_size  = 0.4

  mn_tick_color = "gray80"
  mn_tick_size  = 0.1

  # general things like the axis color and grids
  fo = fo +
  theme(axis.line = element_line(colour = "black"),
        axis.text          = element_text(color="black"), 
        panel.border       = element_rect(colour = "black", fill=NA, size=1),
        panel.background   = element_blank()) 

  # Setting up the ticks
  if(x_tick_major){
    fo = fo+ theme( panel.grid.major.x = element_line(color = mj_tick_color, size = mj_tick_size))
  } else {
    fo = fo+ theme( panel.grid.major.x = element_blank())
  }
  if(x_tick_minor){
    fo = fo+ theme( panel.grid.minor.x = element_line(color = mn_tick_color, size = mn_tick_size))
  } else {
    fo = fo+ theme( panel.grid.minor.x = element_blank())
  }

  if(y_tick_major){
    fo = fo+ theme( panel.grid.major.y = element_line(color = mj_tick_color, size = mj_tick_size))
  } else {
    fo = fo+ theme( panel.grid.major.y = element_blank())
  }
  if(y_tick_minor){
    fo = fo+ theme( panel.grid.minor.y = element_line(color = mn_tick_color, size = mn_tick_size))
  } else {
    fo = fo+ theme( panel.grid.minor.y = element_blank())
  }

  # setting line thickness and font size for the specific output type
  if(purpose == "present"){
  fo = fo + 
       theme( axis.text.x  = element_text(size=16), 
              axis.title.x = element_text(size=16), 
              axis.title.y = element_text(size=16), 
              legend.text  = element_text(size=16), 
              title        = element_text(size=16), 
              plot.title   = element_text(size=16), 
              axis.text.y  = element_text(size=16)) 
  } else if (purpose == "shiny") {
       theme( axis.text.x  = element_text(size=14), 
              axis.title.x = element_text(size=14), 
              axis.title.y = element_text(size=14), 
              legend.text  = element_text(size=14), 
              title        = element_text(size=14), 
              text         = element_text(size=14), 
              plot.title   = element_text(size=14), 
              axis.text.y  = element_text(size=14)) 
  } else if (purpose == "print") {
       theme( axis.text.x  = element_text(size=10), 
              axis.title.x = element_text(size=10), 
              axis.title.y = element_text(size=10), 
              legend.text  = element_text(size=10), 
              title        = element_text(size=10), 
              text         = element_text(size=10), 
              plot.title   = element_text(size=10), 
              axis.text.y  = element_text(size=10)) 
  }


# Removing boxes and stuff from around the elgend
fo = fo + theme(legend.key = element_blank()) 

return(fo)
}


#---------------------------------------------------------------------------
# This function is used to format axis and ticks. 
#   It can be used to put a "pretty" log10 axis x and y axis on a ggplot
#   figure
#
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
gg_axis  = function(fo, 
                     yaxis_scale  = TRUE,
                     xaxis_scale  = TRUE,
                     ylim_min     = NULL, 
                     ylim_max     = NULL, 
                     xlim_min     = NULL, 
                     xlim_max     = NULL, 
                     x_tick_label = TRUE,
                     y_tick_label = TRUE){
  # If any of the limits are null we build out the figure object so we can
  # pull the limits from that object

  if(any(is.null(ylim_min),is.null(ylim_min),is.null(xlim_min), is.null(xlim_max))){
    fob = ggplot_build(fo) }



  #
  # Finding the xlim values
  #
  if(any(is.null(xlim_min), is.null(xlim_max))){
    fob = ggplot_build(fo)
    # looping through the figure object and pulling out all of the y data
    # to get the bounds on the y data
    xdata = c()
    for(didx in 1:length(fob$data)){
      xdata = c(xdata, fob$data[[didx]]$x)
    }

    # Getting only thge positive x data
    xdata = xdata[xdata> 0]

    if(is.null(xlim_min)){
      xlim_min = max(min(xdata), 0)
    }
    if(is.null(xlim_max)){
      xlim_max = max(xdata)
    }
    
  
  }

  if(xaxis_scale){
    xlim_min = 10^floor(log10(xlim_min))
    xlim_max = 10^ceiling(log10(xlim_max))
  }

  data_xlim = c(xlim_min, xlim_max)

  #
  # Finding the ylim values
  #
  if(any(is.null(ylim_min), is.null(ylim_max))){
    # looping through the figure object and pulling out all of the y data
    # to get the bounds on the y data
    ydata = c()
    for(didx in 1:length(fob$data)){
      ydata = c(ydata, fob$data[[didx]]$y)
    }
 
    # Getting only thge positive y data
    ydata = ydata[ydata> 0]
 
    if(is.null(ylim_min)){
      ylim_min = max(min(ydata), 0)
    }
    if(is.null(ylim_max)){
      ylim_max = max(ydata)
    }
  }

  if(yaxis_scale){
    ylim_min = 10^floor(log10(ylim_min))
    ylim_max = 10^ceiling(log10(ylim_max))
  }
 
  data_ylim = c(ylim_min, ylim_max)



  #
  # Formatting the y axis
  #
  if(yaxis_scale){
    if(!is.null(data_ylim)){
    
      # Creating the major ticks
      ytick_major =  10^(log10(data_ylim[1]):log10(data_ylim[2]))
     
      # Expanding the major tick labels beyond the current axis to make sure the
      # minor tick labels get filled out.
      ytick_major = c(min(ytick_major)/10, ytick_major, max(ytick_major)*10)
     
     
      # defining the axis limits
      myylim = 10^(c(data_ylim))
     
      if(!is.null(ylim_min)){
          myylim[1] = ylim_min
      }
     
      if(!is.null(ylim_max)){
          myylim[2] = ylim_max
      }
     
     
      # Creating the minor ticks between the major ticks
      ytick_minor = c()
      for(yt in 1:length(ytick_major)-1){
        ytick_minor = c(ytick_minor, 10^log10(ytick_major[yt])*2:9)
      }
     
     
      if(y_tick_label){
        fo = fo + scale_y_continuous(breaks       = ytick_major,
                                     minor_breaks = ytick_minor,
                                     trans        = 'log10',
                                     limits       = myylim,
                                     labels       = scales::trans_format("log10", scales::math_format(10^.x)))
      }
      else{
        fo = fo + scale_y_continuous(breaks       = ytick_major,
                                     minor_breaks = ytick_minor,
                                     trans        = 'log10',
                                     limits       = myylim,
                                     labels       = NULL)
      }
    }
    fo = fo + annotation_logticks(sides='lr') 
    
    # Left aligning the y tick lables
    fo = fo + theme(axis.text.y = element_text(hjust = 0))

  }
  
  #
  # Formatting the x axis
  #
  if(xaxis_scale){
    if(!is.null(data_xlim)){
      # Creating the major ticks
      xtick_major =  10^(log10(data_xlim[1]):log10(data_xlim[2]))

      # Expanding the major tick labels beyond the current axis to make sure the
      # minor tick labels get filled out.
      xtick_major = c(min(xtick_major)/10, xtick_major, max(xtick_major)*10)


      # defining the axis limits
      myxlim = 10^(c(data_xlim))

      if(!is.null(xlim_min)){
          myxlim[1] = xlim_min
      }

      if(!is.null(xlim_max)){
          myxlim[2] = xlim_max
      }


      # Creating the minor ticks between the major ticks
      xtick_minor = c()
      for(xt in 1:length(xtick_major)-1){
        xtick_minor = c(xtick_minor, 10^log10(xtick_major[xt])*2:9)
      }

      if(x_tick_label){
        fo = fo + scale_x_continuous(breaks       = xtick_major,
                                     minor_breaks = xtick_minor,
                                     trans        = 'log10',
                                     limits       = myxlim,
                                     labels       = scales::trans_format("log10", scales::math_format(10^.x)))
      }
      else{
        fo = fo + scale_x_continuous(breaks       = xtick_major,
                                     minor_breaks = xtick_minor,
                                     trans        = 'log10',
                                     limits       = myxlim,
                                     labels       = NULL)
      }
    }
    fo = fo + annotation_logticks(sides='tb') 
  }





fo}
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# Shortcut to gg_axis to make the y axis log10
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
gg_log10_yaxis = function(fo, 
                          ylim_min     = NULL, 
                          ylim_max     = NULL, 
                          y_tick_label = TRUE,
                          x_tick_label = TRUE){


 fo =  gg_axis(fo=fo,
               yaxis_scale  = TRUE,
               xaxis_scale  = FALSE,
               ylim_min     = ylim_min,
               ylim_max     = ylim_max,
               xlim_min     = NULL, 
               xlim_max     = NULL, 
               y_tick_label = y_tick_label,
               x_tick_label = TRUE) 


fo}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Shortcut to gg_axis to make the x axis log10
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
gg_log10_xaxis = function(fo, 
                          xlim_min     = NULL, 
                          xlim_max     = NULL, 
                          y_tick_label = TRUE,
                          x_tick_label = TRUE){

 fo =  gg_axis(fo=fo,
               yaxis_scale  = FALSE,
               xaxis_scale  = TRUE,  
               ylim_min     = NULL,
               ylim_max     = NULL,
               xlim_min     = xlim_min, 
               xlim_max     = xlim_max, 
               x_tick_label = x_tick_label,
               y_tick_label = TRUE)


fo}
#---------------------------------------------------------------------------


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
ubiquity_name_check = function(test_name){
#
# Error checking function to make sure the test_name 
# matches the following rules:
#
#  - starts with a letter
#  - only conatins letters, numbers, and _
#

  chkres = list()
  chkres$isgood = TRUE

  chkres$msgs = c()

  # Making sure it starts with a letter
  if(!grepl('^[a-z,A-Z]', test_name)){
     chkres$msgs    = c(chkres$msgs, 'Does not begin with a letter') }


  # now we remove all of the allowed characters and see what's left
  # there should be nothing left :)
  test_name_trim = gsub('[a-z,A-Z,0-9,_]', '', test_name)

  if(nchar(test_name_trim) > 0){
     chkres$msgs    = c(chkres$msgs,'Should only contain letters, numbers and _') }
  

  # If there are any messages we flip the isgood to 
  # false and concatenate them together

  if(length(chkres$msgs) > 0){
     chkres$isgood = FALSE
     chkres$msg    = paste(chkres$msg, collapse=', ')
  
  }

 return(chkres) 

}

# Recreating the matlab linspace and log space funcitons
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
linspace = function(a, b, n=100){
   step = (b-a)/(n-1)
   return(seq(a,b,step))
}

#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
logspace = function(a, b, n=100){
   step = (b-a)/(n-1)
   linseq = seq(a,b,step)
   return(10^linseq)
}

# -------------------------------------------------------------------------
# system_define_cohorts_nm  -  Defining cohorts from a NONMEM dataset
# 
#  DS - name of databsed defined using system_load_dataset
# 
# Column names:
#  col_ID   - unique subject identifier 
#  col_DV   - observations
#  col_TIME - system time
#  col_AMT  - infusion/dose amounts
#              - these need to be in the same units specified in the system.txt file
#  col_RATE - rate of infusion or . for bolus
#  col_EVID - evid (0 - observation, 1 dose)
# 
# 
# Include all records in the dataset
#  filter = NULL
# 
# Include only records matching the following filter
#  filter = list()
#  filter$COLNAME = c()
# 
# Mapping information: 
# 
# Inputs:
#  INPUTMAP = list()
#  INPUTMAP$bolus$SPECIES$CMT_NUM            =  1
#  INPUTMAP$infusion_rates$RATE$CMT_NUM      =  1
#  INPUTMAP$covariates$CVNAME$col_COV        = 'CNAME'
#
# Outputs
#  OBSMAP = list()
#  OBSMAP$ONAME=list(variance     = 'PRED^2',
#                    CMT          =  1,
#                    output       = '<O>',
#                    missing      =  NULL )
#
#  system_define_cohorts_nm(cfg, 
#                           DS        = 'DSNAME',
#                           col_ID    = 'ID',
#                           col_CMT   = 'CMT',
#                           col_DV    = 'DV',
#                           col_TIME  = 'TIME',
#                           col_AMT   = 'AMT',
#                           col_RATE  = 'RATE',
#                           col_EVID  = 'EVID',
#                           col_GROUP =  NULL,  
#                           filter    =  NULL,
#                           INPUTS    =  INPUTMAP,
#                           OBS       =  OBSMAP,
#                           group     =  FALSE)
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_define_cohorts_nm = function(cfg, 
                                    DS        = 'DSNAME',
                                    col_ID    = 'ID',
                                    col_CMT   = 'CMT',
                                    col_DV    = 'DV',
                                    col_TIME  = 'TIME',
                                    col_AMT   = 'AMT',
                                    col_RATE  = 'RATE',
                                    col_EVID  = 'EVID',
                                    col_GROUP =  NULL,
                                    filter    =  NULL,
                                    INPUTS    =  NULL,
                                    OBS       =  NULL,
                                    group     =  FALSE){

vp(cfg,'------------------------------------------')
vp(cfg, sprintf('Defining cohorts from NONMEM dataset'))
#
# Checking the nonmem dataset
#
cr = system_nm_check_ds(cfg       =  cfg,             
                        DS        =  DS,              
                        col_ID    =  col_ID,          
                        col_CMT   =  col_CMT,         
                        col_AMT   =  col_AMT,         
                        col_DV    =  col_DV,          
                        col_RATE  =  col_RATE,      
                        col_EVID  =  col_EVID,      
                        col_TIME  =  col_TIME,       
                        col_GROUP =  col_GROUP,
                        filter    =  filter, 
                        INPUTS    =  INPUTS,  
                        OBS       =  OBS)
  # default to true and flip this below if we encounter any problems
  isgood = TRUE


  
  if(cr$isgood){
  
    # Setting up the plotting colors
    if(!is.null(col_GROUP)){
      mycolors   = c('blue', 'green', 'orange', 'red')
      myshapes   = c(    16,      17,       18,    15)
      mygroups   = unique(cr$dsraw[[col_GROUP]])
      mygroupg_str = c()
      grp_colors = rep(x=mycolors, length.out=length(mygroups))
      grp_shapes = rep(x=myshapes, length.out=length(mygroups))

      colmap = list() 
      grpidx = 1
      for(cg in mygroups){
        cgs = sprintf('GRP_%s', toString(cg))
        mygroupg_str = c(mygroupg_str, cgs)
        colmap[[cgs]]$color = grp_colors[grpidx]
        colmap[[cgs]]$shape = grp_shapes[grpidx]
        grpidx = grpidx + 1
        }

      }


    # ALLSUBS is a summary of all subjects
    ALLSUBS = list()
    for(sid in cr$sids){
      # By default the subject is good:
      subisgood = TRUE
      subinputs = list()

      # String to be associated with the subject
      sidstr = sprintf('sub_%d', sid)

      # pulling out the all of the subjects records (sar) the subjects input
      # records (sir) and the subjects output records (sor)
      sar = cr$dsraw[cr$dsraw[[col_ID]] == sid, ]
      sir = cr$input_records[cr$input_records[[col_ID]] == sid, ]
      sor = cr$obs_records[cr$obs_records[[col_ID]] == sid, ]
      
      INPUT_RATE     = as.numeric(as.character(sir[[col_RATE]]))
      INPUT_AMT      = as.numeric(as.character(sir[[col_AMT]]))
      INPUT_CMT      = as.numeric(as.character(sir[[col_CMT]]))
      INPUT_TIME_SYS = as.numeric(as.character(sir[[col_TIME]]))

      # Check to make sure there is at least one observation record
      # for the current subject

      # ocmts is all of the observation compartments
      ocmts = c()
      # orecs is all of the observation records for the current subject
      orecs = NULL
      for(oname in names(OBS)){
        # We check the current observation CMT and see 
        # if its present in the current subejcts records
        if(any(sor[[col_CMT]] == OBS[[oname]]$CMT)){
          if(is.null(orecs)){
            orecs = sor[sor[[col_CMT]] == OBS[[oname]]$CMT, ]
          } else {
            orecs = rbind( orecs , sor[sor[[col_CMT]] == OBS[[oname]]$CMT, ])
          }
        } 
        ocmts = c(ocmts, OBS[[oname]]$CMT)
      }

      # Now we check the records for this subject
      if(length(orecs[[col_DV]]) > 0){
        # This subject has observations so we make sure that they are not null
        subobs = as.numeric(as.character(orecs[[col_DV]]))

        # If any of these values are NA then we give the user an error
        if(any(is.na(subobs))){
          subisgood = FALSE
          vp(cfg, sprintf("Warning: Subject >%s< has observations that are NA", toString(sid)              ))
        }
      
      } else {
        # This subject has no observations:
        subisgood = FALSE
        vp(cfg, sprintf("Warning: Subject >%s< has no output observations", toString(sid)              ))
        vp(cfg, sprintf("         For compartments %s                    ", paste(ocmts, collapse=", ")))
      }

      # Bolus
      if("bolus" %in% names(INPUTS)){
        for(name in names(INPUTS$bolus)){
          # Pulling the compartment for the current bolus
          BOLUS_CMT  = INPUTS$bolus[[name]]$CMT_NUM

          # Keeping all of the all of the indices that have an input rate of
          # NA and the specified bolus compartment nimber 
          INDEX_KEEP = is.na(INPUT_RATE) & (INPUT_CMT == BOLUS_CMT)
          BOLUS_AMTS      = INPUT_AMT[INDEX_KEEP]
          BOLUS_TIME_SYS  = INPUT_TIME_SYS[INDEX_KEEP]

          # If the subject has bolus inputs we store those, otherwise we push
          # a warning to the user
          if(length(BOLUS_AMTS) > 0){
            eval(parse(text=sprintf('BOLUS_TIME_SCALE  = BOLUS_TIME_SYS/(%s)', cfg$options$inputs$bolus$times$scale)))
            subinputs$bolus[[name]]$TIME = BOLUS_TIME_SCALE
            subinputs$bolus[[name]]$AMT  = BOLUS_AMTS
          } else {
            vp(cfg, sprintf("Warning: Subject >%s< bolus compartment >%s< no inputs found in dataset", toString(sid), name ))
          }
        }
      }

      # Infusions
      if("infusion_rates" %in% names(INPUTS)){

        for(name in names(INPUTS$infusion_rates)){
          # Pulling the compartment number for the current infusion rate
          RATE_CMT = INPUTS$infusion_rates[[name]]$CMT_NUM

          # Keeping all of the indices that have input rates that are not NA
          # and where the input CMT is equal to that of the current infusion
          # rate
          INDEX_KEEP = !is.na(INPUT_RATE) & (INPUT_CMT == RATE_CMT)
          RATE_AMTS      = INPUT_AMT[INDEX_KEEP]
          RATE_RATES_SYS = INPUT_RATE[INDEX_KEEP]
          RATE_TIME_SYS  = INPUT_TIME_SYS[INDEX_KEEP]

          if(length(RATE_AMTS) > 0){

            # Converting the rates times from system times to the input time scale
            eval(parse(text=sprintf('RATE_TIME_SCALE  = RATE_TIME_SYS/(%s)', cfg$options$inputs$infusion_rates[[name]]$times$scale)))
            eval(parse(text=sprintf('RATE_RATES_SCALE = RATE_RATES_SYS*(%s)', cfg$options$inputs$infusion_rates[[name]]$times$scale)))

            RATE_VECT = NULL
            #
            #  RATE = mass/time
            #  AMT  = mass
            #
            #  Infusion duration = AMT/RATE
            #

            for(ridx  in 1:length(RATE_TIME_SCALE)){
               
              RATE_RATES_SYS[ridx] 
              RATE_AMTS[ridx]
              RATE_TIME_SCALE[ridx]
              STOP_TIME = RATE_RATES_SYS[ridx]/RATE_AMTS[ridx]
              RATE_AMTS[ridx]/RATE_RATES_SYS[ridx]

              # JMH what happens in NONMEM when infusions go from one level to
              # another? like 10 mg/min to 50 mg/min?
              
              ISTART = RATE_TIME_SCALE[ridx]
              IDUR   = RATE_AMTS[ridx]/RATE_RATES_SYS[ridx]
              ISTOP  = ISTART + IDUR

               if(is.null(RATE_VECT)){
                 RATE_VECT = list()
                 RATE_VECT$TIME = c(              ISTART,   ISTOP)
                 RATE_VECT$AMT  = c(RATE_RATES_SYS[ridx],     0.0)
               
               } else {
                 RATE_VECT$TIME = c(RATE_VECT$TIME,               ISTART, ISTOP)
                 RATE_VECT$AMT  = c( RATE_VECT$AMT, RATE_RATES_SYS[ridx],   0.0)
               }
            
            }
          } else {
            vp(cfg, sprintf("Warning: Subject >%s< rate >%s< no inputs found in dataset", toString(sid), name ))
          }
          
        # Adding the rate for he current subject to the subinputs rate 
        subinputs$infusion_rates[[name]] = RATE_VECT
        }
      }
      

      # Covariates
      if("covariates" %in% names(INPUTS)){
        for(name in names(INPUTS$covariates)){
          cv_time = as.numeric(as.character(sar[[col_TIME]]))
          cv_val  = as.numeric(as.character(sar[[INPUTS$covariates[[name]]$col_COV]]))

          # As long as the times and cv columns have numeric values we're good
          if(!any(is.na(cv_time)) & !any(is.na(cv_time))){
            subinputs$covariates[[name]]$TIME = cv_time 
            subinputs$covariates[[name]]$AMT  = cv_val
          }
           
          # If not we send the user some messages and we flag this subject to
          # be ignored
          if(any(is.na(cv_time))){
            subisgood = FALSE
            vp(cfg, sprintf("Warning: Subject >%s< covariate >%s< time column has NA values", toString(sid), name ))
          } 
          if(any(is.na(cv_val))){
            subisgood = FALSE
            vp(cfg, sprintf("Warning: Subject >%s< covariate >%s< column >%s< has NA values", toString(sid), name, INPUTS$covariates[[name]]$col_COV))
          } 
        }
      }



      # After parsing the information we add the subject 
      # if it passes all of the tests above
      if(subisgood){
        ALLSUBS[[sidstr]]$subinputs = subinputs
        ALLSUBS[[sidstr]]$sid       = sid
        ALLSUBS[[sidstr]]$sar       = sar
        
      } else {
        # If subisgood is false then we're skipping this subject
        vp(cfg, sprintf("Skipping Subject >%s< see messages aboves", toString(sid)              ))
      }
    }

    vp(cfg, 'Subjects parsed, adding cohorts')
    for(sidstr in names(ALLSUBS)){
      cohort = c()
      cohort$name                                 = sidstr

      # defining the dataset
      cohort$dataset = DS

      # Filtering the dataset
      cohort$cf = list()
      if(!is.null(filter)){
        for(cname in names(filter)){
          cohort$cf[[cname]] = filter[[cname]]
        }
      }
      # only observations
      cohort$cf[[col_EVID]] = c(0)
      # current subject
      cohort$cf[[col_ID]]   = ALLSUBS[[sidstr]]$sid
      # defining the inputs
      cohort$inputs = ALLSUBS[[sidstr]]$subinputs

      # looping through the outputs and adding the relevant 
      # fields 
      for(output in names(OBS)){
        # Filtering to the compartment for that individual
        cohort$outputs[[output]]$of[[col_CMT]]       = OBS[[output]]$CMT
        cohort$outputs[[output]]$obs$missing         = OBS[[output]]$missing  
        cohort$outputs[[output]]$obs$time            = col_TIME
        cohort$outputs[[output]]$obs$value           = col_DV
        cohort$outputs[[output]]$model$variance      = OBS[[output]]$variance
        cohort$outputs[[output]]$model$time          = cr$TSsys
        cohort$outputs[[output]]$model$value         = OBS[[output]]$output
        
        if(!is.null(col_GROUP)){
          SUB_GRP = unique(ALLSUBS[[sidstr]]$sar[[col_GROUP]])
          if(length(SUB_GRP) == 1){
            SUB_GRP_STR = sprintf('GRP_%s', toString(SUB_GRP))
            cohort$outputs[[output]]$options$marker_color   = colmap[[SUB_GRP_STR]]$color
            cohort$outputs[[output]]$options$marker_shape   = colmap[[SUB_GRP_STR]]$shape
          } else {
            vp(cfg, sprintf('Warning: Grouping column >%s< for subject >%s< has more', col_GROUP, sidstr))
            vp(cfg, sprintf('         than one value. Groping was not applied for this subject'))
          }
           
        }
      }


     # Adding the cohort
     cfg = system_define_cohort(cfg, cohort)
    
    }

  } else {
    isgood = FALSE
  }

  
  if(!isgood){
    vp(cfg, 'system_define_cohorts_nm()') }


vp(cfg,'------------------------------------------')
cfg}
# /system_define_cohorts_nm 
# -------------------------------------------------------------------------


#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_fetch_TSsys = function(cfg){
# Pulling the timescales 
time_scales = names(cfg$options$time_scales)
time_scales = time_scales[time_scales != "time" ]
TSsys   = NULL
for(TS in time_scales){
  if(cfg$options$time_scales[[TS]] == 1){
    TSsys = TS
  }
}

TSsys}

# -------------------------------------------------------------------------
# system_nm_check_ds - Takes mapping information from a NONMEM dataset and
# checks it with specifications in the system.txt file
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_nm_check_ds = function(cfg, 
                              DS        = 'DSNAME',
                              col_ID    = 'ID',
                              col_CMT   = 'CMT',
                              col_DV    = 'DV',
                              col_TIME  = 'TIME',
                              col_AMT   = 'AMT',
                              col_RATE  = 'RATE',
                              col_EVID  = 'EVID',
                              col_GROUP =  NULL,
                              filter    =  NULL,
                              INPUTS    =  NULL,
                              OBS       =  NULL){
                                
isgood    = TRUE
mywarning = FALSE

TSsys = system_fetch_TSsys(cfg)

if(is.null(TSsys)){
 isgood = FALSE
 vp(cfg, 'Error: Unable to determine the system timscale. This needs ')
 vp(cfg, '       to be specified in the system.txt file. For example ')
 vp(cfg, '       if the timescale is days the following would be used:')
 vp(cfg, '       <TS:days>  1.0                                    ')
}


# Checking the dataset to make sure it exists
if((DS %in% names(cfg$data))){
  vp(cfg, sprintf('Checking NONMEM dataset >%s<',DS))
}
else{
  isgood = FALSE
  vp(cfg, sprintf('Unable to find NONMEM dataset >%s<',DS))
}

if(isgood){
  # Checking the required columns to make sure they exist in the dataset
  col_vars = c('col_ID', 'col_CMT', 'col_DV', 'col_TIME', 'col_AMT', 'col_RATE', 'col_EVID')
  for(col_var in col_vars){
    eval(parse(text=sprintf('col_val = %s', col_var))) 
    if(!(col_val %in% names(cfg$data[[DS]]$values))){
     isgood = FALSE
     vp(cfg, sprintf('Error: Unable to find %s (%s)',col_var, col_val))
    }
  }

  # Next we check the gruping column. If it's not null we see if it's in the
  # dataset, if not we throw an error to the user
  if(!is.null(col_GROUP)){
    if(!(col_GROUP %in% names(cfg$data[[DS]]$values))){
      isgood = FALSE
      vp(cfg, sprintf('Error: The grouping column >%s< was not found in the dataset.', col_GROUP))
    }
  } 


  if(isgood){
    vp(cfg, sprintf('Dataset looks good'))
    vp(cfg, sprintf('Time column (%s) should have units of %s', col_TIME, TSsys))
  }
}

#---------------------------------------------------------------
# Checking the inputs
#
if(is.null(INPUTS)){
  if(!is.null(names(cfg$options$inputs))){
    vp(cfg, 'Warning: No input mapping information was specified')
    vp(cfg, '         but there are inputs in the system file'   )
    mywarning = TRUE
  }
} else {
  if(is.null(names(cfg$options$inputs))){
    vp(cfg, 'Warning: Input mapping was specified but the system' )
    vp(cfg, '         file has no inputs specified'   )
    isgood = FALSE
  }
  else{
    #
    # Checking bolus inputs
    #
    if("bolus" %in% names(INPUTS)){
      # is the species in cfg in INPUTS
      for(name in names(cfg$options$inputs$bolus$species)){
        if(!(name %in% names(INPUTS$bolus))){
          vp(cfg, sprintf('Warning: %s - bolus defined in system but ', name))
          vp(cfg, sprintf('         there is no input mapping defined'))
          mywarning = TRUE
        }
      }
    } else {
      # Check to make sure there are inputs
      if("bolus" %in% names(cfg$options$inputs)){
        vp(cfg, 'Warning: No bolus input mapping was specified but ')
        vp(cfg, '         bolus information was specified in the system file')
        mywarning = TRUE
      }
    }

    #
    # Checking infusion rates
    #
    if("infusion_rates" %in% names(INPUTS)){
      # is the rate in cfg in INPUTS
      for(name in names(cfg$options$inputs$infusion_rates)){
        if(!(name %in% names(INPUTS$infusion_rates))){
          vp(cfg, sprintf('Warning: %s - rate defined in system but ', name))
          vp(cfg, sprintf('         there is no input mapping defined'))
          mywarning = TRUE
        }
      }
    } else {
      # Check to make sure there are inputs
      if("infusion_rates" %in% names(cfg$options$inputs)){
        vp(cfg, 'Warning: No infusion rate mapping was specified but ')
        vp(cfg, '         infusion rate information was specified in ')
        vp(cfg, '         the system file')
        mywarning = TRUE
      }
    }

    #
    # Checking covariates
    #
    if("covariates" %in% names(INPUTS)){
      # Checking for system covariates to see if there 
      # is an input mapping defined in INPUTS
      for(name in names(cfg$options$inputs$covariates)){
        if(!(name %in% names(INPUTS$covariates))){
          vp(cfg, sprintf('Warning: %s - covariate defined in system but ', name))
          vp(cfg, sprintf('         there is no input mapping defined'))
          mywarning = TRUE
        }
      }
      # Checking each covariate in INPUTS 
      for(name in names(INPUTS$covariates)){
        # making sure col_COV was specified
        if("col_COV" %in% names(INPUTS$covariates[[name]])){
          # making sure the specified column was in the database
          if(!(INPUTS$covariates[[name]]$col_COV %in% names(cfg$data[[DS]]$values))){
            isgood = FALSE   
            vp(cfg, sprintf('Error: %s - covariate column >%s<', name, INPUTS$covariates[[name]]$col_COV))
            vp(cfg, sprintf('       does not exist in dataset'))
          }
        } else {
          isgood = FALSE   
          vp(cfg, sprintf('Error: %s - covariate does not have column mapping', name))
          vp(cfg, sprintf(" INPUTS$covariates$%s$col_COV = 'COLNAME'         ", name))
        }
      }
    } else {
      # Check to make sure there are inputs
      if("covariates" %in% names(cfg$options$inputs)){
        vp(cfg, 'Warning: No covariates mapping was specified but ')
        vp(cfg, '         covariates information was specified in ')
        vp(cfg, '         the system file')
        mywarning = TRUE
      }
    }
  }
}
#---------------------------------------------------------------

#---------------------------------------------------------------
# Checking the outputs
if(is.null(OBS)){
  vp(cfg, 'Error: No observation mapping information was specified')
  isgood = FALSE
} else {

  # Looping through each output and checking 
  for(name in names(OBS)){

    # making sure the output field exits
    if(is.null(OBS[[name]]$output)){
      vp(cfg, sprintf('Error: output mapping error for >%s<', name))
      vp(cfg, sprintf('       no output field specified'))
      vp(cfg, sprintf(' OBSMAP$%s$output    = "VALUE"', name))
      isgood = FALSE
    } else {
      # Making sure the output has been defined in the system.txt file
      if(!(OBS[[name]]$output %in% names(cfg$options$mi$outputs))){
        vp(cfg, sprintf('Error: output mapping error for >%s<', name))
        vp(cfg, sprintf('       the specified output >%s< ',OBS[[name]]$output))
        vp(cfg, sprintf('       does not appear to have been defined in the system.txt file'))
        vp(cfg, sprintf('       <O> %s = value ',OBS[[name]]$output))
        isgood = FALSE
      }
    }
  }
}

#---------------------------------------------------------------

# creating the result
result = list()
result$isgood    = isgood
result$mywarning = mywarning


# Everythign checks out so far, so we start to add the cohorts
if(isgood){
  # Pulling out the raw data 
  dsraw         = cfg$data[[DS]]$values

  # If a filter has been specified we filter dsraw down 
  if(!is.null(filter)){
    dsraw      = nm_select_records(cfg, dsraw, filter) }

  input_records = dsraw[dsraw[[col_EVID]] == 1, ]
  obs_records   = dsraw[dsraw[[col_EVID]] == 0, ]

  sids   = sort(unique(dsraw[[col_ID]]))

  # Packing everything up together
  result$dsraw         = dsraw
  result$input_records = input_records
  result$obs_records   = obs_records
  result$sids          = sids
  result$TSsys         = TSsys
}

  if(!isgood | mywarning){
    vp(cfg, 'system_nm_check_ds()')
  }
result}
# /system_nm_check_ds 
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# system_report_view_layout
# rpt = system_report_view_layout(cfg, 
#                                 rptname       = "default",
#                                 output_file   = "layout.pptx")
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_view_layout = function(cfg,
                                     rptname     = "default",
                                     output_file = "layout.pptx"){

# New document from the template
ppt = read_pptx(cfg$reporting$reports[[rptname]]$template)

# Pulling out all of the layouts stored in the template
lay_sum = layout_summary(ppt)

# Looping through each layout
for(lidx in 1:length(lay_sum[,1])){

   # Pulling out the layout properties
   layout = lay_sum[lidx, 1]
   master = lay_sum[lidx, 2]
   lp = layout_properties ( x = ppt, layout = layout, master = master)

   # Adding a slide for the current layout
   ppt =  add_slide(x=ppt, layout = layout, master = master) 

   # Blank slides have nothing
   if(length(lp[,1] > 0)){

     # Now we go through each placholder
     for(pidx in 1:length(lp[,1])){

        # If it's a text placeholder "body" or "title" we add text indicating
        # the type and index. If it's title we put the layout and master
        # information in there as well.
        if(lp[pidx, ]$type == "body"){
          textstr = sprintf('type="body", index =%d', pidx)
          ppt =ph_with_text(x=ppt, type="body", index=pidx, str=textstr)  
        } 
        if(lp[pidx, ]$type %in% c("title", "ctrTitle", "subTitle")){
          textstr = sprintf('layout ="%s", master = "%s", type="%s", index =%d', layout, master, lp[pidx, ]$type,  pidx)
          ppt =ph_with_text(x=ppt, type=lp[pidx, ]$type, str=textstr)  
        }
     }
   } 
}

 # If an output file name has been specified we dump that here
 if(!is.null(output_file)){
  print(ppt, output_file)
  vp(cfg, "--------------------------------")
  vp(cfg, sprintf("Generating annotated layout for a report template"))
  vp(cfg, sprintf("Name:             %s", rptname))
  vp(cfg, sprintf("Template:         %s", cfg$reporting$reports[[rptname]]$template))
  vp(cfg, sprintf("Annotated layout: %s", output_file))
  vp(cfg, "--------------------------------")

 }
return(ppt)}
# /system_report_view_layout
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# system_report_fetch
# rpt = system_report_fetch(cfg, 
#     rptname       =  "default")
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_fetch = function (cfg,
                               rptname     = "default"){

  rpt = NULL

  if(cfg$reporting$enabled){
    if(rptname %in% names(cfg$reporting$reports)){
      rpt = cfg$reporting$reports[[rptname]]$report
    } else {
      vp(cfg, sprintf("system_report_fetch()"))
      vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
    }
  }
return(rpt)}
# /system_report_fetch
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# system_report_set  
# cfg = system_report_set(cfg, 
#     rptname =  "default",
#     rpt     = NULL)
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_set = function (cfg,
                              rptname     = "default",
                              rpt         = NULL){

  isgood = TRUE

  if(cfg$reporting$enabled){
    if(rptname %in% names(cfg$reporting$reports)){
      if(is.null(rpt)){
        isgood = FALSE
        vp(cfg, sprintf("Error: The report is NULL, need to specify an officer object"))
      } else {
        rpt = cfg$reporting$reports[[rptname]]$report
      }
    } else {
      isgood = FALSE
      vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
    }
  }

  if(!isgood){
    vp(cfg, sprintf("system_report_set()")) }
return(cfg)}
# /system_report_fetch
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# system_report_save 
# system_report_save(cfg, 
#     rptname       =  "default",
#     output_file   = "myreport.pptx")
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_save = function (cfg,
                               rptname     = "default",
                               output_file = "myreport.pptx"){
  if(cfg$reporting$enabled){
    if(rptname %in% names(cfg$reporting$reports)){
      print(cfg$reporting$reports[[rptname]]$report, output_file)
    } else {
      vp(cfg, sprintf("system_report_save()"))
      vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
    }
  }
}
# /system_report_save 
# -------------------------------------------------------------------------
# system_report_init 
# cfg = system_report_init(cfg, 
#     template = file.path("library", "templates", "report.pptx"),
#     rptname  = "default",
#     meta     = NULL)
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_init = function (cfg,
                               template = file.path("library", "templates", "report.pptx") ,
                               rptname  = "default",
                               meta     = NULL){
isgood = TRUE

vp(cfg, "--------------------------------")
  if(require('officer')){
    cfg$reporting$enabled = TRUE
    # Checking to see if the template file exists
    if(file.exists(template)){
      # if no meta data has been specified then we pull the default meta data,
      # otherwise we store the meta data provided
      name_check = ubiquity_name_check(rptname)

      if(name_check$isgood){
        if(is.null(meta)){
          cfg$reporting$reports[[rptname]]$meta  = cfg$reporting$meta
        } else {
          cfg$reporting$reports[[rptname]]$meta  = meta
        }
        # Storing the original template location and creating the empty report
        cfg$reporting$reports[[rptname]]$template = template
        cfg$reporting$reports[[rptname]]$report   = read_pptx(path=template)
        vp(cfg, sprintf("Report initialized:"))
        vp(cfg, sprintf("Name:     %s", rptname))
        vp(cfg, sprintf("Template: %s", template))
      } else {
        isgood = FALSE
        vp(cfg, sprintf('Error: report name >%s< is invalid', cohort$name))
      }
    
    } else {
      isgood = FALSE
      vp(cfg, sprintf("Unable to find template file >%s<. ", template))
    }
  
  } else {
    vp(cfg, "Reporting is done through the 'officer' package. Unable to load ")
    vp(cfg, "this package. Reporting will be disabled.")
    cfg$reporting$enabled = FALSE
  }

  if(!isgood){
    vp(cfg, "system_report_init()")
    vp(cfg, sprintf("Report >%s< initialization failed.", rptname)) }
vp(cfg, "--------------------------------")
return(cfg)
}
# /system_report_init 
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# system_report_slide_content
# cfg = system_report_slide_content(cfg,
#        title          = "Title",
#        sub_title      = NULL,    
#        rptname        = "default",
#        content_type   = "text", 
#        content        = "Text")
# Content dimensions:
# ggsave(filename=imgfile, plot=p, height=5.15, width=9, units="in")
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_slide_content = function (cfg,
                               title                  = "Title",      
                               sub_title              = NULL, 
                               rptname                = "default",
                               content_type           = 'text', 
                               content                = 'Text'
                               ){

  # We only process this if reporting is enabled
  if(cfg$reporting$enabled){
    # checking to make sure the user has initialized the report
    if(rptname %in% names(cfg$reporting$reports)){
      # Pulling out the meta data for the report template
      meta = cfg$reporting$reports[[rptname]]$meta 

      # Pulling out the report to make it easier to deal with
      tmprpt  = cfg$reporting$reports[[rptname]]$report

      # Adding the slide
      if(content_type %in% c("text", "imagefile", "ggplot", "table", "flextable")){
        tmprpt = add_slide(x      = tmprpt, 
                           layout = meta$content$layout$general,
                           master = meta$content$master$general)
        body_index      = meta$content$indices$content_body
        sub_title_index = meta$content$indices$content_sub_title
      }
      else if(content_type == "list"){
        tmprpt = add_slide(x      = tmprpt, 
                           layout = meta$content$layout$list,
                           master = meta$content$master$list)
        body_index      = meta$content$indices$list_body
        sub_title_index = meta$content$indices$list_sub_title
      }

      # Adding Slide title/subtitle information
      if(!is.null(title)){
        tmprpt = ph_with_text(x=tmprpt, type='title', str=title) } 
      if(!is.null(sub_title_index) & !is.null(sub_title)){
        tmprpt = ph_with_text(x=tmprpt, type='body', index=sub_title_index, str=sub_title) } 

      # Adding the content
      type   = "body"
      tmprpt = system_report_ph_content(cfg          = cfg,          
                                        rpt          = tmprpt, 
                                        content_type = content_type, 
                                        content      = content, 
                                        type         = type,         
                                        index        = body_index)
  
      # Putting the report back into cfg
      cfg$reporting$reports[[rptname]]$report = tmprpt
    } else {
      vp(cfg, sprintf("system_report_slide_content_col() "))
      vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
      vp(cfg, sprintf("       Slide not added"               ))
    }
  
  }
return(cfg)}
#/system_report_slide_content
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# system_report_slide_two_col
# Content dimensions:
# Without header: units = inches, height = 5.08, width = 4.65
# With header:    units = inches, height = 4.41, width = 4.65
# cfg = system_report_slide_two_col = function (cfg,
#        title                     = "Title",      
#        sub_title                 = NULL, 
#        rptname                   = "default",
#        content_type              = 'text', 
#        left_content              =  NULL,
#        left_content_type         =  NULL, 
#        right_content             =  NULL,
#        right_content_type        =  NULL, 
#        left_content_header       =  NULL,  
#        left_content_header_type  = 'text', 
#        right_content_header      =  NULL,
#        right_content_header_type = 'text')
# 
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_slide_two_col = function (cfg,
                               title                       = "Title",      
                               sub_title                   = NULL, 
                               rptname                     = "default",
                               content_type                = 'text', 
                               left_content                =  NULL,
                               left_content_type           =  NULL, 
                               right_content               =  NULL,
                               right_content_type          =  NULL, 
                               left_content_header         =  NULL,  
                               left_content_header_type    = 'text', 
                               right_content_header        =  NULL,
                               right_content_header_type   = 'text'){

  # Making sure reporting is enabled
    # checking to make sure the user has initialized the report
  # Making sure reporting is enabled
  if(cfg$reporting$enabled){
    # checking to make sure the user has initialized the report
    if(rptname %in% names(cfg$reporting$reports)){
      # Pulling out the meta data for the report template
      meta = cfg$reporting$reports[[rptname]]$meta 
      # Pulling out the report to make it easier to deal with
      tmprpt  = cfg$reporting$reports[[rptname]]$report

      #-------------------------------------------------------
      #  here we initialize the correct slide and 
      #  define the indices
      #
      if(content_type %in% c('text')){
        if(is.null(left_content_header) & is.null(left_content_header)){
          # Text without headers
          tmprpt = add_slide(x      = tmprpt, 
                             layout = meta$two_col$layout$text,
                             master = meta$two_col$master$text)

          left_index         = meta$two_col$indices$text_left
          right_index        = meta$two_col$indices$text_right
          left_title_index   = NULL
          right_title_index  = NULL
          sub_title_index    = meta$two_col$indices$text_sub_title
        } else {
          # Text with headers
          tmprpt = add_slide(x      = tmprpt, 
                             layout = meta$two_col$layout$text_head,
                             master = meta$two_col$master$text_head)

          left_index         = meta$two_col$indices$text_head_left
          right_index        = meta$two_col$indices$text_head_right
          left_title_index   = meta$two_col$indices$text_head_left_title
          right_title_index  = meta$two_col$indices$text_head_right_title
          sub_title_index    = meta$two_col$indices$text_head_sub_title
        }
      }else if(content_type %in% c('list')){
        if(is.null(left_content_header) & is.null(left_content_header)){
          # List without headers
          tmprpt = add_slide(x      = tmprpt, 
                             layout = meta$two_col$layout$list,
                             master = meta$two_col$master$list)

          left_index         = meta$two_col$indices$list_left
          right_index        = meta$two_col$indices$list_right
          left_title_index   = NULL
          right_title_index  = NULL
          sub_title_index    = meta$two_col$indices$list_sub_title
        
        } else {
          # List with headers
          tmprpt = add_slide(x      = tmprpt, 
                             layout = meta$two_col$layout$list_head,
                             master = meta$two_col$master$list_head)

          left_index         = meta$two_col$indices$list_head_left
          right_index        = meta$two_col$indices$list_head_right
          left_title_index   = meta$two_col$indices$list_head_left_title
          right_title_index  = meta$two_col$indices$list_head_right_title
          sub_title_index    = meta$two_col$indices$list_head_sub_title
        }
      }

      # If the content type hasn't been set then they inheret 
      # the content type of the main slide
      if(is.null(left_content_type)){
        left_content_type = content_type }
      if(is.null(right_content_type)){
        right_content_type = content_type }
      #-------------------------------------------------------

      # Adding Slide title/subtitle information
      if(!is.null(title)){
        tmprpt = ph_with_text(x=tmprpt, type='title', str=title) } 
      if(!is.null(sub_title_index) & !is.null(sub_title)){
        tmprpt = ph_with_text(x=tmprpt, type='body', index=sub_title_index, str=sub_title) } 

      # browser()

      #
      # Creating the headers
      #
      if(!is.null(left_content_header)){
        tmprpt = system_report_ph_content(cfg          = cfg,          
                                          rpt          = tmprpt, 
                                          content_type = left_content_header_type, 
                                          content      = left_content_header, 
                                          type         = "body",         
                                          index        = left_title_index)
      }
      if(!is.null(right_content_header)){
        tmprpt = system_report_ph_content(cfg          = cfg,          
                                          rpt          = tmprpt, 
                                          content_type = right_content_header_type, 
                                          content      = right_content_header, 
                                          type         = "body",         
                                          index        = right_title_index)
      }

      #
      # Creating the main content
      #
      if(!is.null(left_content)){
        tmprpt = system_report_ph_content(cfg          = cfg,          
                                          rpt          = tmprpt, 
                                          content_type = left_content_type, 
                                          content      = left_content, 
                                          type         = "body",         
                                          index        = left_index)
      }
      if(!is.null(right_content)){
        tmprpt = system_report_ph_content(cfg          = cfg,          
                                          rpt          = tmprpt, 
                                          content_type = right_content_type, 
                                          content      = right_content, 
                                          type         = "body",         
                                          index        = right_index)
      }


      # Putting the report back into cfg
      cfg$reporting$reports[[rptname]]$report = tmprpt
    } else {
      vp(cfg, sprintf("system_report_slide_two_col() "))
      vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
      vp(cfg, sprintf("       Slide not added"               ))
    }
  }


return(cfg)}
#/system_report_slide_two_col
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# system_report_slide_section
# Content dimensions:
# units = inches, 
# cfg = system_report_slide_section = function (cfg,
#        title      = "Title",      
#        sub_title  = NULL, 
#        rptname    = "default")
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_slide_section = function (cfg,
                               title                  = "Title",      
                               sub_title              = NULL, 
                               rptname                = "default"){

  isgood = TRUE
  # We only process this if reporting is enabled
  if(cfg$reporting$enabled){
    # checking to make sure the user has initialized the report
    if(rptname %in% names(cfg$reporting$reports)){
      # Pulling out the meta data for the report template
      meta = cfg$reporting$reports[[rptname]]$meta 
      # Pulling out the report to make it easier to deal with
      tmprpt  = cfg$reporting$reports[[rptname]]$report

      # Adding the title slide
      tmprpt = add_slide(x      = tmprpt, 
                         layout = meta$section$layout$general,
                         master = meta$section$master$general)


      # Adding Slide title/subtitle information
      if(!is.null(title)){
        if(meta$section$type$title == "ctrTitle"){
          tmprpt = ph_with_text(x=tmprpt, type='ctrTitle', str=title) 
         } else if (meta$section$type$title == "body" & !is.null(meta$section$indices$title)) {
          tmprpt = ph_with_text(x=tmprpt, type='body', index = meta$section$indices$title, str=title) 
         } else {
           isgood = FALSE
         }
       } 
      if(!is.null(sub_title)){
        if(meta$section$type$sub_title == "subTitle"){
          tmprpt = ph_with_text(x=tmprpt, type="subTitle", str=sub_title) 
         } else if (meta$section$type$sub_title == "body" & !is.null(meta$section$indices$sub_title)) {
          tmprpt = ph_with_text(x=tmprpt, type='body', index = meta$section$indices$sub_title, str=sub_title) 
         } else {
           isgood = FALSE
         }
       }

      # Putting the report back into cfg
      cfg$reporting$reports[[rptname]]$report = tmprpt
    } else {
      isgood = FALSE
    }
  }

  if(!isgood){
    vp(cfg, sprintf("system_report_slide_section  () "))
    vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
    vp(cfg, sprintf("       Slide not added"               ))
  
  }


return(cfg)}
#/system_report_slide_section
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# system_report_slide_title
# Content dimensions:
# units = inches, 
# cfg = system_report_slide_title   = function (cfg,
#         title     = "Title",      
#         sub_title = NULL, 
#         rptname   = "default")
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_slide_title   = function (cfg,
                               title                  = "Title",      
                               sub_title              = NULL, 
                               rptname                = "default"){

  # We only process this if reporting is enabled
  if(cfg$reporting$enabled){
    # checking to make sure the user has initialized the report
    if(rptname %in% names(cfg$reporting$reports)){
      # Pulling out the meta data for the report template
      meta = cfg$reporting$reports[[rptname]]$meta 
      # Pulling out the report to make it easier to deal with
      tmprpt  = cfg$reporting$reports[[rptname]]$report

      # Adding the title slide
      tmprpt = add_slide(x      = tmprpt, 
                         layout = meta$title$layout$general,
                         master = meta$title$master$general)

      # Adding Slide title/subtitle information
      if(meta$title$type$title == "ctrTitle"){
        tmprpt = ph_with_text(x=tmprpt, type="ctrTitle", str=title) 
       } else if (meta$title$type$title == "body" & !is.null(meta$title$indices$title)) {
        tmprpt = ph_with_text(x=tmprpt, type='body', index = meta$title$indices$title, str=title) 
       } else {
         isgood = FALSE
       }
      if(!is.null(sub_title)){
        if(meta$title$type$sub_title == "subTitle"){
          tmprpt = ph_with_text(x=tmprpt, type="subTitle", str=sub_title) 
         } else if (meta$title$type$sub_title == "body" & !is.null(meta$title$indices$sub_title)) {
          tmprpt = ph_with_text(x=tmprpt, type='body', index = meta$title$indices$sub_title, str=sub_title) 
         } else {
           isgood = FALSE
         }
       }

      # Putting the report back into cfg
      cfg$reporting$reports[[rptname]]$report = tmprpt
    } else {
      vp(cfg, sprintf("system_report_slide_title  () "))
      vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
      vp(cfg, sprintf("       Slide not added"               ))
    }
  }


return(cfg)}
#/system_report_slide_title  
# -------------------------------------------------------------------------

#  # -------------------------------------------------------------------------
#  # system_report_slide_xxx
#  # Content dimensions:
#  # units = inches, 
#  system_report_slide_xxx     = function (cfg,
#                                 title                  = "Title",      
#                                 sub_title              = NULL, 
#                                 rptname                = "default",
#                                 content_type           = 'text', 
#                                 left_content           = 'Text',
#                                 right_content          = 'Text',
#                                 left_content_header    =  NULL,  
#                                 right_content_header   =  NULL){
#  
#    # We only process this if reporting is enabled
#    if(cfg$reporting$enabled){
#      # checking to make sure the user has initialized the report
#      if(rptname %in% names(cfg$reporting$reports)){
#        # Pulling out the meta data for the report template
#        meta = cfg$reporting$reports[[rptname]]$meta 
#        # Pulling out the report to make it easier to deal with
#        tmprpt  = cfg$reporting$reports[[rptname]]$report
#  
#        browser()
#        # Adding the slide
#        tmprpt = add_slide(x      = tmprpt, 
#                           layout = meta$xxx$layout$general,
#                           master = meta$xxx$master$general)
#  
#  
#        # Adding Slide title/subtitle information
#        if(!is.null(title)){
#          tmprpt = ph_with_text(x=tmprpt, type='ctrTitle', str=title) } 
#        if(!is.null(sub_title_index) & !is.null(sub_title)){
#          tmprpt = ph_with_text(x=tmprpt, type='subTitle', str=sub_title) } 
#        # Populate with content
#  
#
#        # Putting the report back into cfg
#        cfg$reporting$reports[[rptname]]$report = tmprpt
#      } else {
#        vp(cfg, sprintf("system_report_slide_xxx    () "))
#        vp(cfg, sprintf("Error: The report name >%s< not found", rptname))
#        vp(cfg, sprintf("       Slide not added"               ))
#      }
#    }
#  
#  
#  return(cfg)}
#  #/system_report_slide_xxx    
#  # -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#'@export
#'@title title
#' Description 
#'
#'@param p1
#'
#'@return r1
#'
#'@examples
#' # Examples
system_report_ph_content = function (cfg, rpt, content_type, content, type, index){

    if(content_type == "text"){
      rpt = ph_with_text(x=rpt, type=type, index=index, str=content)
    }
    else if(content_type == "list"){
      mcontent = matrix(data = content, ncol=2, byrow=TRUE)

      
      # Initializing the placeholder
      rpt   = ph_empty(x=rpt, type=type, index=index)

      # Getting the id_chr 
      ss    = slide_summary(rpt)
      id_chr= ss[length(ss[,1]),]$id
      # Adding the bullets
      for(lidx in 1:length(mcontent[,1])){
        rpt = ph_add_par( x=rpt, id_chr= id_chr, level = as.numeric(mcontent[lidx, 1]))
        rpt = ph_add_text(x=rpt, id_chr= id_chr, str   = mcontent[lidx, 2]) 
      }
    }
    else if(content_type == "imagefile"){
      rpt = ph_with_img(x=rpt, type = type, index = index, src = content)   
    }
    else if(content_type == "ggplot"){
      rpt = ph_with_gg(x=rpt, type = type, index = index, value = content)   
    }
    else if(content_type == "table"){
      if('header' %in% names(content)){
        header = content$header
      } else {header = TRUE}
      if('first_row' %in% names(content)){
        first_row = content$first_row
      } else {first_row = TRUE}
      rpt = ph_with_table(x         = rpt,       type   = type, 
                          index     = index,     header = header,
                          first_row = first_row, value = content$table)   
    }
    else if(content_type == "flextable"){
      # These are the default table options
      # and they can be over written by specifying 
      # the same fields of the content list
      header_top            = NULL
      header_middle         = NULL
      header_bottom         = NULL
      merge_header          = TRUE
      table_body_alignment  ="center"
      table_header_alignment  ="center"
      table_autofit         = TRUE
      table_theme           ="theme_vanilla"
      cwidth                = 0.75
      cheight               = 0.25

      ftops = c("header_top",             "header_middle",    "header_bottom", 
                "merge_header",           "table_theme",      "table_body_alignment",
                "table_header_alignment", "table_autofit",    "cwidth", 
                "cheight")


      # Defining the user specified flextable options:
      for(ftop in ftops){
        if(!is.null(content[[ftop]])){
          eval(parse(text=sprintf('%s = content[[ftop]]', ftop)))
        }
      }

      # Creating the table
      ft = regulartable(content$table,  cwidth = cwidth, cheight=cheight)
      
      # Adding headers
      header_types = c("header_top", "header_middle", "header_bottom")
      first_header = TRUE
      for(header_type in header_types){
       
        if(!is.null(eval(parse(text=header_type)))){
          eval(parse(text=sprintf("header =  %s", header_type)))
          # Creating the header
          if(!is.null(header)){
            if(length(names(header)) > 0){
              if(first_header){
                first_header = FALSE
                shstr = ' ft = set_header_labels(ft'
              } else {
                shstr = ' ft = add_header(ft'
              }
              for(hname in names(header)){
                shstr = sprintf('%s, %s="%s"', shstr, hname, header[[hname]])
              }
              shstr = sprintf('%s, top=FALSE)', shstr)
              eval(parse(text=shstr))
            }
          }
        }
      }
      

     # Setting the theme
     if(!is.null(table_theme)){
       eval(parse(text=sprintf('ft = %s(ft)', table_theme))) }
     
     # Merging headers
     if(merge_header){
       ft = merge_h(ft, part="header") }

     if(table_autofit){
       ft = autofit(ft) }

     # Applying the aligment
     ft = align(ft, align=table_header_alignment, part="header")
     ft = align(ft, align=table_body_alignment,   part="body"  )

     rpt = ph_with_flextable(x         = rpt,       type   = type, 
                             index     = index,     value  = ft)

    }
   


return(rpt)}
# -------------------------------------------------------------------------
