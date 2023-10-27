#'@import cli     
#'@import deSolve
#'@import doParallel
#'@import foreach
#'@import ggplot2
#'@import knitr
#'@import optimx
#'@import onbrand
#'@import pso
#'@import rmarkdown
#'@import rhandsontable
#'@import stringr
#'@importFrom digest digest
#'@importFrom dplyr  all_of select
#'@importFrom flextable add_header add_footer align as_chunk as_paragraph autofit body_add_flextable delete_part merge_h 
#'@importFrom flextable regulartable set_header_labels theme_alafoli theme_box theme_tron_legacy 
#'@importFrom flextable theme_vanilla theme_booktabs theme_tron theme_vader theme_zebra
#'@importFrom parallel stopCluster makeCluster
#'@importFrom readxl read_xls read_xlsx
#'@importFrom magrittr "%>%"
#'@importFrom PKNCA PKNCA.options PKNCAconc PKNCAdose PKNCAdata pk.nca get.interval.cols
#'@importFrom utils capture.output read.csv read.delim txtProgressBar setTxtProgressBar write.csv tail packageVersion sessionInfo
#'@importFrom stats median qt var sd
#'@importFrom scales trans_format  math_format squish_infinite
#'@importFrom MASS mvrnorm


# These were either pulled out because they are used in the shiny app or
# because they were used in reporting and are now only used in onbrand
#   #'@import rstudioapi
#   #'@importFrom grid pushViewport viewport grid.newpage grid.layout
#   #'@importFrom gridExtra grid.arrange
#   #'@importFrom officer add_slide annotate_base body_add_break body_add_fpar body_add_par body_add_gg body_add_img 
#   #'@importFrom officer body_add_table body_add_toc body_bookmark body_end_section_continuous 
#   #'@importFrom officer body_end_section_landscape body_end_section_portrait body_replace_all_text external_img 
#   #'@importFrom officer footers_replace_all_text headers_replace_all_text layout_properties layout_summary ph_location_type 
#   #'@importFrom officer ph_location_label ph_with read_pptx read_docx shortcuts  slip_in_seqfield slip_in_text 
#   #'@importFrom officer styles_info unordered_list



#'@export
#'@title Build the System File
#'@description  Builds the specified system file creating the targets for R and other languages as well as the templates for performing simulations and estimations. 
#'
#'@param system_file name of the file defining the system in the \href{https://ubiquity.tools}{ubiquity} format (default = 'system.txt'), if the file does not exist a template will be created and compiled.
#'@param distribution indicates weather you are using a \code{'package'} or a \code{'stand alone'}
#' distribution of ubiquity. If set to \code{'automatic'} the build script will first 
#' look to see if the ubiquity R package is installed. If it is installed it
#' will use the package. Otherwise, it will assume a \code{"sand alone"} distribution.
#'@param perlcmd system command to run perl ("perl")
#'@param output_directory location to store analysis outputs (\code{file.path(".", "output")})
#'@param temporary_directory location to templates and otehr files after building the system (\code{file.path(".", "transient")})
#'@param verbose enable verbose messaging   (\code{TRUE})
#'@param ubiquity_app set to \code{TRUE} when building the system to be used with the ubiquty App (\code{FALSE})
#'@param debug Boolean variable indicating if debugging information should be displayed (\code{TRUE})
#'@return initialized ubiquity system object
#'@examples
#' \donttest{
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'}
build_system <- function(system_file          = "system.txt",
                         distribution         = "automatic",
                         perlcmd              = "perl",
                         output_directory     = file.path(".", "output"),
                         temporary_directory  = file.path(".", "transient"),
                         verbose              = TRUE,
                         ubiquity_app         = FALSE,
                         debug                = TRUE){

# If we cannot find a system file we create an empty one 
if(!file.exists(system_file)){
  cli::cli_alert_warning(paste("Unable to find system file >",system_file, "<", sep=""))
  cli::cli_alert_warning("Creating an empty template")
  sys_new_res = system_new(system_file="template", file_name=system_file)
}

 
# model base file used for the c library
system_file_full = normalizePath(system_file, winslash = "/")
if(ubiquity_app){
  system_checksum = "app_base"
} else {
  system_checksum = as.character(digest::digest(system_file_full, algo=c("md5")))
}

c_libfile_base    =  paste("ubiquity_", system_checksum, sep="")
c_libfile_base_c  =  paste("ubiquity_", system_checksum, ".c", sep="")
c_libfile_base_o  =  paste("ubiquity_", system_checksum, ".o", sep="")

temporary_directory = normalizePath(temporary_directory, winslash="/")
temp_directory  = file.path(temporary_directory, system_checksum)

# if the temporary directory does not exist we create it
if(!dir.exists(temp_directory)){
  dir.create(temp_directory, recursive=TRUE)
}

# If the distribution is set to automatic we see if the package is loaded
# If not we see if the stand alone library file is present, lastly we try to
# load the package
if(distribution == "automatic"){
  if("ubiquity" %in% (.packages())){
    distribution = "package"
  } else if(file.exists(file.path('library', 'r_general', 'ubiquity.R'))){
    source(file.path('library', 'r_general', 'ubiquity.R'))
    distribution = "stand alone"
  } 
} else if(distribution == "package"){
  # If it's set to package we make sure the package is installed and
  # if ti's not we default to stand alone
  if(system.file(package="ubiquity") == ""){
    cli_alert_warning("Warning: package selected but not found")
    distribution = "stand alone" }
}


# Checking for perl
if(as.character(Sys.which(perlcmd )) == ""){
  stop("No perl interpreter found")
}


pkgs = c("deSolve", "ggplot2", "readxl", "cli")
invisible(system_req(pkgs))

# For stand alone distributions we just use the default template and transient
# directory
if(distribution == "stand alone"){
  templates       = file.path(getwd(), "library", "templates")
  build_script_pl = "build_system.pl"
}

# For the package we pull the package install location to point to files
# needed to build the system
if(distribution == "package"){
  package_dir     = system.file("", package="ubiquity")
  templates       = file.path(package_dir, "ubinc", "templates")
  build_script_pl = file.path(package_dir, "ubinc", "perl",  "build_system.pl")
}


cfg = list()


if(file.exists(system_file)){
  if(verbose == TRUE){
    cli::cli_h1(paste("Building the system: ", system_file, sep=""))
    cli::cli_alert(c("ubiquity:     ", cli::col_blue(style_underline(style_bold("https://r.ubiquity.tools")))))
    if(distribution == "package"){
      cli::cli_alert(c("Distribution: ",  cli::col_blue(style_underline(paste0(distribution, " (", packageVersion("ubiquity"), ")", sep="")))))
    } else {
      cli::cli_alert(c("Distribution: ",  cli::col_blue(style_underline(paste0(distribution)))))
    }
  }

  
  build_command = sprintf('%s "%s" "%s" "%s" "%s" "%s" "%s"', 
                          perlcmd, build_script_pl, system_file_full, 
                          temp_directory, templates, distribution, c_libfile_base)
  output = system(build_command, intern=TRUE)
  
  # CFILE is used to indicate if we have compiled and loaded the CFILE successfully 
  # We defalut to TRUE and then set it to false below if there are any problems encountered.
  CFILE = TRUE
  
  if(length(output) > 0){
    cli::cli_alert_warning("Build reported errors and")
    cli::cli_alert_warning("may have failed, see below:")
    for(line in output){
      cli::cli_alert_warning(line)
    }
    rm('line')
  }
  
  #
  # Cleaning up any older versions of the C file
  #
  # if it's loaded we remove it from memory:
  if((c_libfile_base %in% names(getLoadedDLLs()))){
    dyn.unload(getLoadedDLLs()[[c_libfile_base]][["path"]])
    }
  
  # making the output directory to store generated information
  if(!file.exists(output_directory)){
    if(verbose == TRUE){
      cli::cli_alert("Creating output directory")
    }
    dir.create(output_directory, recursive=TRUE)
  }
  
  #next we remove any files to make sure we start from scratch
  if(file.exists(file.path(temp_directory, paste(c_libfile_base, .Platform$dynlib.ext, sep = "")))){
     file.remove(file.path(temp_directory, paste(c_libfile_base, .Platform$dynlib.ext, sep = ""))) }
  if(file.exists(file.path(temp_directory, c_libfile_base_o))){
     file.remove(file.path(temp_directory, c_libfile_base_o)) }
  
  # Now we compile the C file
  if(verbose == TRUE){
    cli::cli_alert("Compiling C version of system")
  }
  # Command used to compile C version of the model:
  compile_cmd =  paste(file.path(R.home("bin"), "R"), ' CMD SHLIB "', file.path(temp_directory, c_libfile_base_c), '"', sep="")

  if(file.exists(file.path(temp_directory, 'r_ode_model.c'))){
    # storing the working directory and 
    # changing the working directory to the
    # temp directory to avoid weird issues
    # with spaces in file names and paths
    # Copying the generated c file to the checksummed base file name
    file.copy(from =file.path(temp_directory, "r_ode_model.c"), 
              to   =file.path(temp_directory, c_libfile_base_c), 
              overwrite=TRUE)
    # Compling the C file
    output =  system(compile_cmd, intern=TRUE) 
    if("status" %in% names(attributes(output))){
      if(verbose == TRUE){
        if(debug == TRUE){
          for(line in output){
            cli::cli_alert_danger(paste("DEBUG:", line, sep=" "))
          }
        }
        cli::cli_alert_danger("Failed: Unable to compile C file") 
        if(debug == TRUE){
          cli::cli_alert_danger("See above for more details")
        }
      }
      CFILE = FALSE
    }else{
      # Loading the shared library
      if(verbose == TRUE){
        cli::cli_alert("Loading the shared C library") }
      dyn.load(file.path(temp_directory, paste(c_libfile_base, .Platform$dynlib.ext, sep = "")))
    }
    if(verbose == TRUE){
      cli::cli_alert_success('System built')
      cli::cli_alert_info('To fetch a new analysis template use {.code system_fetch_template}')
      cli::cli_alert_info('For example:')
      cli::cli_alert_info('  fr = system_fetch_template(cfg, template = "Simulation")')
      cli::cli_alert_info('  fr = system_fetch_template(cfg, template = "Estimation")')
    }
  }else{
    if(verbose == TRUE){
      cli::cli_alert_danger(paste("Failed: file", file.path(temp_directory, c_libfile_base_c), " not found "))
    }
    CFILE = FALSE
  }
  

  if(CFILE == FALSE){
    if(verbose == TRUE){
      cli::cli_alert_warning("C model not available. Compile manually using the") 
      cli::cli_alert_warning("following command to debug:          ") 
     #cli::cli_alert_warning(paste("system('", compile_cmd,"')"))
      cli::cli_alert_warning(sprintf("system('R CMD SHLIB \"%s%sr_ode_model.c\"')", temp_directory, .Platform$file.sep))
    }
  }
  
  # Returning the ubiquity model object:
  if(file.exists(file.path(temp_directory, "auto_rcomponents.R"))){
    source(file.path(temp_directory, "auto_rcomponents.R"))
    eval(parse(text=paste0("cfg = system_fetch_cfg_", c_libfile_base, "()")))

    # storing the output directory
    cfg$options$misc$output_directory =  output_directory 
  } 
  
} else {
  cli::cli_alert_danger(paste("Still unable to find system file >", system_file,"<", sep=""))
}
return(cfg)}

# -------------------------------------------------------------------------
#'@export 
#'@title Fetch Ubiquity Workshop Sections
#'@description With the ubiquity package this function can be used to fetch
#' example files for different sections of the workshop.
#'
#'@param section Name of the section of workshop to retrieve  ("Simulation")
#'@param overwrite if \code{TRUE} the new workshop files will overwrite any existing files present (\code{FALSE})
#'@param copy_files if \code{TRUE} the files will be written to the output_directory, if \code{FALSE} only the names and locations of the files will be returned (\code{TRUE})
#'@param output_directory directory where workshop files will be placed (getwd())
#'@details Valid sections are "Simulation", "Estimation", "Titration" "Reporting", and "NCA"
#'
#'@return list
#'@examples
#' \donttest{
#' workshop_fetch("Estimation", output_directory=tempdir(), overwrite=TRUE)
#'}
workshop_fetch <- function(section          = "Simulation", 
                           overwrite        = FALSE,
                           copy_files       = TRUE,
                           output_directory = getwd()){
  res = list()
  allowed = c("Simulation", "Estimation", "Titration", "Reporting", "Testing", "NCA")

  isgood = TRUE
  # This function only works if we're using the package
  if(!(system.file(package="ubiquity") == "") |
       dir.exists(file.path("examples", "R"))){
    if(section %in% allowed){
    
      if(!(system.file(package="ubiquity") == "") ){
        src_dir = system.file("ubinc", "scripts", package="ubiquity")
        csv_dir = system.file("ubinc", "csv",     package="ubiquity")
        sys_dir = system.file("ubinc", "systems", package="ubiquity")
      } else {
        src_dir = file.path("examples", "R")
        csv_dir = file.path("examples", "R")
        sys_dir = file.path("examples")
      }

      sources      = c()
      destinations = c()
      write_file   = c()

      if(section=="Simulation"){
         sources      = c(file.path(src_dir, "analysis_single.r"            ),
                          file.path(src_dir, "analysis_multiple.r"          ),
                          file.path(src_dir, "analysis_multiple_file.r"     ),
                          file.path(sys_dir, "system-mab_pk.txt"            ),
                          file.path(csv_dir, "mab_pk_subjects.csv"))
         destinations = c("analysis_single.r",
                          "analysis_multiple.r",
                          "analysis_multiple_file.r",
                          "system.txt",
                          "mab_pk_subjects.csv")
         write_file   = c(TRUE, TRUE, TRUE, TRUE, TRUE)
      } else if(section=="Estimation") {
         sources      = c(file.path(src_dir, "analysis_parent.r"                    ),
                          file.path(src_dir, "analysis_parent_metabolite.r"         ),
                          file.path(src_dir, "analysis_parent_metabolite_global.r"  ),
                          file.path(src_dir, "analysis_parent_metabolite_nm_data.r" ),
                          file.path(sys_dir, "system-adapt.txt"                     ),
                          file.path(csv_dir, "pm_data.csv"                          ),
                          file.path(csv_dir, "nm_data.csv"                          ))
         destinations = c("analysis_parent.r",                   
                          "analysis_parent_metabolite.r",        
                          "analysis_parent_metabolite_global.r",  
                          "analysis_parent_metabolite_nm_data.r", 
                          "system.txt",
                          "pm_data.csv",                          
                          "nm_data.csv"                                             )
         write_file   = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
      } else if(section=="Reporting") {
         sources      = c(file.path(src_dir, "make_report_PowerPoint.R"), 
                          file.path(src_dir, "make_report_Word.R"), 
                          file.path(sys_dir, "system-mab_pk.txt"))
         destinations = c("make_report_PowerPoint.R",
                          "make_report_Word.R", 
                          "system.txt")

         write_file   = c(TRUE, TRUE, TRUE)
      } else if(section=="Titration") {
         sources      = c(file.path(src_dir, "analysis_repeat_dosing.r"                     ),
                          file.path(src_dir, "analysis_repeat_infusion.r"                   ),
                          file.path(src_dir, "analysis_state_reset.r"                       ),
                          file.path(src_dir, "analysis_visit_dosing_titration.r"            ),
                          file.path(src_dir, "analysis_visit_dosing_titration_stochastic.r" ),
                          file.path(src_dir, "analysis_visit_infusion_dosing.r"             ),
                          file.path(sys_dir, "system-mab_pk.txt"                            ))
         destinations = c("analysis_repeat_dosing.r",                    
                          "analysis_repeat_infusion.r",                  
                          "analysis_state_reset.r",                       
                          "analysis_visit_dosing_titration.r",            
                          "analysis_visit_dosing_titration_stochastic.r", 
                          "analysis_visit_infusion_dosing.r",                           
                          "system.txt")
         write_file   = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
      } else if(section=="Testing") {
         sources      = c(file.path(src_dir, "workshop_test.R"))
         destinations = c("workshop_test.R")
         write_file   = c(TRUE)
      } else if(section=="NCA") {
         sources      = c(file.path(src_dir, "analysis_nca_md.R"            ),
                          file.path(src_dir, "analysis_nca_sd.R"            ),
                          file.path(src_dir, "analysis_nca_sparse.R"        ),
                          file.path(src_dir, "nca_generate_data.R"          ),
                          file.path(sys_dir, "system-mab_pk.txt"            ),
                          file.path(csv_dir, "pk_all_md.csv"                ),
                          file.path(csv_dir, "pk_all_sd.csv"                ),
                          file.path(csv_dir, "pk_sparse_sd.csv"))
         destinations = c("analysis_nca_md.R"      ,                        
                          "analysis_nca_sd.R"      ,                        
                          "analysis_nca_sparse.R"  ,                        
                          "nca_generate_data.R"    ,                        
                          "system-mab_pk.txt"      ,
                          "pk_all_md.csv"          ,
                          "pk_all_sd.csv"          ,
                          "pk_sparse_sd.csv")
         write_file   = rep(TRUE, length(sources))

      }

      # if overwrite ifs FALSE we check each of the destination files to see if
      # they exist. Then we set write_file to FALSE if they do exist, and throw
      # up an error.
      if(!overwrite){
        for(fidx in 1:length(destinations)){
          if(copy_files){
            if(file.exists(file.path(output_directory, destinations[fidx]))){
              write_file[fidx] = FALSE 
            }
          } else {
              # If we're not copying the files then we set
              # the write_file flag to FALSE
              write_file[fidx] = FALSE
          }
        }
      }

      # storing the details in res
      res$sources      = sources
      res$destinations = destinations
      res$write_file   = write_file

      # next we write the files that are TRUE
      for(fidx in 1:length(destinations)){
        if(write_file[fidx]){
          file.copy(sources[fidx], file.path(output_directory, destinations[fidx]), overwrite=TRUE)
          cli::cli_alert(paste("Creating file:", file.path(output_directory, destinations[fidx] )))
        } else {
          isgood = FALSE
          cli::cli_alert_warning(paste("File:", file.path(output_directory, destinations[fidx]), "exists, and was not copied."))
          cli::cli_alert_warning(      "Set overwrite=TRUE to force this file to be copied.")
        }
      }
    } else {
      isgood = FALSE
      cli::cli_alert_danger(paste("section >", section, "< is not valid must be one of: ", paste(allowed, collapse=", "), sep=""))
    }

  } else {
    isgood = FALSE
    cli::cli_alert("workshop_fetch()")
    cli::cli_alert("Unable to find ubiquity package or stand alone distribution files")
  }


  res$isgood = isgood

return(res)}
# -------------------------------------------------------------------------
#'@export
#'@title Create New \code{system.txt} File 
#'
#'@description  Copy a blank template (\code{system_file="template"}) file to the working directory or an example by specifying the following:
#'
#' \itemize{
#'   \item \code{"template"} - Empty system file template
#'   \item \code{"adapt"} - Parent/metabolite model taken from the adapt manual used in estimation examples [ADAPT]
#'   \item \code{"two_cmt_cl"} - Two compartment model parameterized in terms of clearances 
#'   \item \code{"one_cmt_cl"} - One compartment model parameterized in terms of clearances 
#'   \item \code{"two_cmt_micro"} - Two compartment model parameterized in terms of rates (micro constants)
#'   \item \code{"one_cmt_micro"} - One compartment model parameterized in terms of rates (micro constants)
#'   \item \code{"mab_pk"} - General compartmental model of mAb PK from Davda 2014 [DG]
#'   \item \code{"pbpk"} - PBPK model of mAb disposition in mice from Shah 2012 [SB]
#'   \item \code{"pbpk_template"} - System parameters from Shah 2012 [SB] have been defined for all species along with the set notation to be used as a template for developing models with physiological parameters
#'   \item \code{"pwc"} - Example showing how to make if/then or piece-wise continuous variables  
#'   \item \code{"tmdd"} - Model of antibody with target-mediated drug disposition
#'   \item \code{"tumor"} - Transit tumor growth model taken from Lobo 2002 [LB] 
#' }
#'
#'@param file_name name of the new file to create   
#'@param system_file name of the system file to copy
#'@param overwrite if \code{TRUE} the new system file will overwrite any existing files present
#'@param output_directory \code{getwd()} directory where system file will be placed
#'
#'@return \code{TRUE} if the new file was created and \code{FALSE} otherwise
#'
#' @details 
#'
#' References
#'
#' \itemize{
#' \item{[ADAPT]} Adapt 5 Users Guide \url{https://bmsr.usc.edu/files/2013/02/ADAPT5-User-Guide.pdf}
#' \item{[DG]} Davda et. al. mAbs (2014) 6(4):1094-1102  \doi{10.4161/mabs.29095}
#' \item{[LB]} Lobo, E.D. & Balthasar, J.P. AAPS J (2002) 4, 212-222  \doi{10.1208/ps040442}
#' \item{[SB]} Shah, D.K. & Betts, A.M. JPKPD (2012) 39 (1), 67-86 \doi{10.1007/s10928-011-9232-2}
#'}
#'
#'
#'
#'@examples
#' \donttest{
#' # To create an example system file named example_system.txt:
#' system_new(system_file      = "mab_pk", 
#'            file_name        = "system_example.txt", 
#'            overwrite        = TRUE,  
#'            output_directory = tempdir())
#'}
system_new  <- function(file_name        = "system.txt", 
                        system_file      ="template", 
                        overwrite        = FALSE,  
                        output_directory = getwd()){

 # Getting a list of the system files
 sfs = system_new_list()

 isgood = FALSE

 output_file = file.path(output_directory, file_name)

 if(system_file %in% names(sfs)){
   write_file = TRUE
   # if ovewrite is false we check to see if the destination file exists. If it
   # does exist, we ste write_file to false
   if(!overwrite){
     if(file.exists(output_file)){
       cli::cli_alert_danger(paste("Error the file >", output_file, "< exists set overwrite=TRUE to overwrite", sep=""))
       write_file = FALSE}
   }

    # if the source file exists and write_file is true then
    # we try to copy the file
    file_path = sfs[[system_file]][["file_path"]]
    if(file.exists(file_path) & write_file){
      isgood = file.copy(file_path, output_file, overwrite=TRUE)
    }
 } else{
   cli::cli_alert_danger(paste("The system file tempalte >", system_file, "< is invalid", sep=""))
   cli::cli_alert_danger(paste("Please choose one of the following:", sep=""))
   for(sf in names(sfs)){
     cli::cli_alert_danger(paste(stringr::str_pad(sf, pad=" ", side="right", width=15), "| ", sfs[[sf]][["description"]], sep=""))
   }
 }
isgood}
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
#'@export
#'@title Fetch List of Available System Templates
#'
#'@description  Returns a list of internal templates with descriptions of their contents and file locations
#'
#' @return list with the template names as the keys
#' \itemize{
#' \item{file_path} Full path to the system file
#' \item{description} Description of what this system file provides
#'}
#'
#'@examples
#' # To get a list of systems
#' systems = system_new_list()
system_new_list  <- function(){

 sfs = list(template      = list(file_path = NULL, description="Empty template file."),      
            mab_pk        = list(file_path = NULL, description="Human antibody compartmental PK with IIV (Davda 2014)"),         
            pbpk          = list(file_path = NULL, description="Full antibody PBPK model (Shah 2012)"),          
            pwc           = list(file_path = NULL, description="Example of how to define picewise continuous functions (if/then statements)"), 
            pbpk_template = list(file_path = NULL, description="Template file with PBPK parameters for multiple species coded mathematical set examples. "), 
            tumor         = list(file_path = NULL, description="Tumor inhibition model (Lobo 2002) with mathematical set examples"),  
            tmdd          = list(file_path = NULL, description="Full TMDD model with examples of how to code the same system as both ODEs and processes"),          
            adapt         = list(file_path = NULL, description="Parent metabolite model taken from the Adapt user manual"),          
            one_cmt_micro = list(file_path = NULL, description="One compartment model with micro-constants"), 
            one_cmt_cl    = list(file_path = NULL, description="One compartment model with clearances"),  
            two_cmt_micro = list(file_path = NULL, description="Two compartment model with micro-constants"), 
            two_cmt_cl    = list(file_path = NULL, description="Two compartment model with clearances"))  

  for(system_file in names(sfs)){

    # If the package is installed we pull it from there:
    if((system.file(package="ubiquity") != "")){
      if(system_file == "template"){
        file_path       = system.file("ubinc",    "templates", "system_template.txt", package="ubiquity")
      } else {
        file_path       = system.file("ubinc",    "systems", sprintf('system-%s.txt',system_file), package="ubiquity")
      }
    } 
    else {
      if(system_file == "template"){
        file_path       = file.path('library', 'templates',  'system_template.txt')
      } else {
        file_path       = file.path('examples', sprintf('system-%s.txt',system_file))
      }
    }


    # storing the file path
    sfs[[system_file]][["file_path"]] = file_path
  }

sfs}

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

#'@export
#'@title Create New Analysis Template 
#'
#'@description Building a system file will produce templates for R and other languages.
#' This function provides a method to make local copies of these templates.
#'
#'@param cfg ubiquity system object    
#'@param template template type  
#'@param overwrite if \code{TRUE} the new system file will overwrite any existing files present
#'@param output_directory directory where workshop files will be placed (getwd())
#'
#'@return List with vectors of template \code{sources}, \code{destinations}
#' and corresponding write success (\code{write_file}), also a list element
#' indicating the overall success of the function call (\code{isgood})
#'
#'@details The template argument can have the following values for the R
#'workflow:
#'
#' \itemize{
#'  \item{"Simulation"}       produces \code{analysis_simulate.R}: R-Script named with placeholders used to run simulations
#'  \item{"Estimation"}       produces \code{analysis_estimate.R}: R-Script named with placeholders used to perform naive-pooled parameter estimation
#'  \item{"NCA"}              produces \code{analysis_nca.R}: R-Script to perform non-compartmental analysis (NCA) and report out the results
#'  \item{"ShinyApp"}         produces \code{ubiquity_app.R}, \code{server.R} and \code{ui.R}: files needed to run the model through a Shiny App either locally or on a Shiny Server
#'  \item{"Model Diagram"}    produces \code{system.svg}: SVG template for producing a model diagram (Goto \url{https://inkscape.org} for a free SVG editor)
#'  \item{"Shiny Rmd Report"} produces \code{system_report.Rmd} and \code{test_system_report.R}: R-Markdown file used to generate report tabs for the Shiny App and a script to test it
#'  \item{"myOrg"}            produces \code{myOrg.R}: R-Script for defining functions used within your organization
#'}
#'
#'And this will create files to use in other software:
#'
#' \itemize{
#'  \item{"Adapt"}            produces \code{system_adapt.for} and \code{system_adapt.prm}: Fortran and parameter files for the currently selected parameter set in Adapt format.
#'  \item{"Berkeley Madonna"} produces \code{system_berkeley_madonna.txt}: text file with the model and the currently selected parameter set in Berkeley Madonna format
#'  \item{"nlmixr"}           produces \code{system_nlmixr.R} For the currently selected parameter set to define the system in the `nlmixr` format.
#'  \item{"NONMEM"}           produces \code{system_nonmem.R} For the currently selected parameter set as a NONMEM conntrol stream.
#'  \item{"Monolix"}          produces \code{system_monolix.txt} For the currently selected parameter set as a NONMEM conntrol stream.
#'  \item{"mrgsolve"}         produces \code{system_mrgsolve.cpp}: text file with the model and the currently selected parameter set in mrgsolve format  
#'}
#'
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#'
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Creating a simulation template
#' fr =  system_fetch_template(cfg, 
#'       template         = "Simulation", 
#'       output_directory = tempdir())
#'}
system_fetch_template  <- function(cfg, template="Simulation", overwrite=FALSE, output_directory=getwd()){

 res = list()
 # These are the allowed templates:
 allowed = c("Simulation", "Estimation", 
             "ShinyApp",   "Shiny Rmd Report",
             "NCA", 
             "mrgsolve",   
             "myOrg", 
             "Model Diagram",
             "Berkeley Madonna", 
             "Adapt", "nlmixr",
             "NONMEM", "Monolix",
             "mrgsolve")


 # default value for the return variable
 isgood = TRUE 

 if(template %in% allowed){
   # first we look to see if the package is installed, if it's not
   # we look for the system_template.txt file 
   if((system.file(package="ubiquity") != "")){
     template_dir = system.file("ubinc", "templates", package="ubiquity")
   } 
   else {
     template_dir = file.path('library', 'templates')
   }
   temp_directory = cfg[["options"]][["misc"]][["temp_directory"]]

   # pulling the current parameter set
   current_set = cfg[["parameters"]][["current_set"]]
  
   # building up the lists of sources and destinations
   sources      = c()
   destinations = c()
   write_file   = c()

   if(template == "Simulation"){
     sources      = c(file.path(temp_directory, "auto_simulation_driver.R"))
     destinations = c("analysis_simulate.R")
     write_file   = c(TRUE)
   }
   if(template == "Estimation"){
     sources      = c(file.path(temp_directory, "auto_analysis_estimation.R"))
     destinations = c("analysis_estimate.R")
     write_file   = c(TRUE)
   }
   if(template == "NCA"){
     sources      = c(file.path(template_dir, "r_nca.R"))
     destinations = c("analysis_nca.R")
     write_file   = c(TRUE)
   }
   if(template == "ShinyApp"){
     sources      = c(file.path(template_dir, "ubiquity_app.R"), 
                      file.path(template_dir, "ubiquity_server.R"),
                      file.path(template_dir, "ubiquity_ui.R"))
     destinations = c("ubiquity_app.R", "server.R", "ui.R")
     write_file   = c(TRUE, TRUE, TRUE)
   }
   if(template == "Shiny Rmd Report"){
     sources      = c(file.path(template_dir, "r_system_report.Rmd"),
                      file.path(template_dir, "r_test_rmd.R"))
     destinations = c("system_report.Rmd", "test_system_report.R")
     write_file   = c(TRUE, TRUE)
   }
   if(template == "mrgsolve"){
     sources      = c(file.path(temp_directory, sprintf("target_mrgsolve-%s.cpp",current_set)))
     destinations = c("system_mrgsolve.cpp")
     write_file   = c(TRUE)
   }
   if(template == "Berkeley Madonna"){
     sources      = c(file.path(temp_directory, sprintf("target_berkeley_madonna-%s.txt",current_set)))
     destinations = c("system_berkeley_madonna.txt")
     write_file   = c(TRUE)
   }
   if(template == "Adapt"){
     sources      = c(file.path(temp_directory, sprintf("target_adapt_5.for")),
                      file.path(temp_directory, sprintf("target_adpat_5-%s.prm",current_set)))
     destinations = c("system_adapt.for", "system_adapt.prm")
     write_file   = c(TRUE, TRUE)
   }
   if(template == "myOrg"){
     sources      = c(file.path(template_dir, sprintf("report.yaml")))
     destinations = c("myOrg.yaml")
     write_file   = c(TRUE)
   }

   if(template == "Model Diagram"){
     sources      = c(file.path(template_dir, sprintf("system.svg")))
     destinations = c("system.svg")
     write_file   = c(TRUE)
   }
   if(template == "NONMEM"){
     sources      = c(file.path(temp_directory, sprintf("target_nonmem-%s.ctl",current_set)))
     destinations = c("system_nonmem.ctl")
     write_file   = c(TRUE, TRUE)
   }
   if(template == "Monolix"){
     sources      = c(file.path(temp_directory, sprintf("target_monolix-%s.txt",current_set)))
     destinations = c("system_monolix.txt")
     write_file   = c(TRUE, TRUE)
   }
   if(template == "nlmixr"){
     sources      = c(file.path(temp_directory, sprintf("target_nlmixr-%s.R",current_set)))
     destinations = c("system_nlmixr.R")
     write_file   = c(TRUE, TRUE)
   }

   # if overwrite ifs FALSE we check each of the destination files to see if
   # they exist. Then we set write_file to FALSE if they do exist, and throw
   # up an error.
   if(!overwrite){
     for(fidx in 1:length(destinations)){
       if(file.exists(file.path(output_directory, destinations[fidx]))){
         write_file[fidx] = FALSE 
       }
     }
   }

   # storing the details in res
   res$sources      = sources
   res$destinations = destinations
   res$write_file   = write_file
  

   # next we write the files that are TRUE
   for(fidx in 1:length(destinations)){
     if(write_file[fidx]){
       file.copy(sources[fidx], file.path(output_directory, destinations[fidx]), overwrite=TRUE)
       vp(cfg, sprintf("Creating file: %s", file.path(output_directory, destinations[fidx])))
     } else {
       isgood = FALSE
       vp(cfg, sprintf("File: %s, exists, and was not copied.", file.path(output_directory, destinations[fidx])))
       vp(cfg, sprintf("Set overwrite=TRUE to force this file to be copied."))
     }
   }
 } else {
   isgood = FALSE
   vp(cfg, sprintf("Template type: %s not recognized", template))
   vp(cfg, sprintf(" must be one of: %s", paste(allowed, collapse=', ')))
 }

  if(!isgood){
    vp(cfg, "ubiquity::system_fetch_template()")
    vp(cfg, "One or more templates failed to copy. See messages above for details")
  }
  res$isgood = isgood
return(res)}
# -------------------------------------------------------------------------
# cfg = system_load_data(cfg, dsname, data_file, data_sheet)
#
#'@export
#'@title Loading Datasets 
#'@description Loads datasets at the scripting level from  a variable if
#' \code{data_file} is a data.frame or from the following
#' formats (based on the file extension)
#'\itemize{
#' \item csv - comma delimited 
#' \item tab - tab delimited
#' \item xls or xlsx - excel spread sheet
#'}
#'
#' Multiple datasets can be loaded as long as they are given different
#' names. Datasets should be in a NONMEM-ish format with the
#' first row containing the column header names.
#'
#'@param cfg ubiquity system object    
#'@param dsname short name of the dataset to be used to link this dataset to different operations
#'@param data_file the file name of the dataset or a data frame containing the data 
#'@param data_sheet argument identifying the name of the sheet in an excel file
#' 
#'@return Ubiquity system object with the dataset loaded
system_load_data <- function(cfg, dsname, data_file, data_sheet){

  if(is.data.frame(data_file)){
    cfg$data[[dsname]]$values = data_file
    cfg$data[[dsname]]$data_file$name  = "From data frame"
  }
  else{
    # Reading the data based on the file extension
    if(file.exists(data_file)){
      if(regexpr(".xls$", as.character(data_file), ignore.case=TRUE) > 0){
        cfg$data[[dsname]]$values = as.data.frame(readxl::read_xls(path=data_file, sheet=data_sheet))
        cfg$data[[dsname]]$data_file$sheet  = data_sheet
      }

      if(regexpr(".xlsx$", as.character(data_file), ignore.case=TRUE) > 0){
        cfg$data[[dsname]]$values = as.data.frame(readxl::read_xlsx(path=data_file, sheet=data_sheet))
        cfg$data[[dsname]]$data_file$sheet  = data_sheet
      }


      if(regexpr(".csv$", as.character(data_file), ignore.case=TRUE) > 0){
        cfg$data[[dsname]]$values = read.csv(data_file, header=TRUE)
      }

      if(regexpr(".tab$", as.character(data_file), ignore.case=TRUE) > 0){
        cfg$data[[dsname]]$values = read.delim(data_file, header=TRUE)
      }
      cfg$data[[dsname]]$data_file$name  = data_file
    } else {
      vp(cfg, " ------------------------------------") 
      vp(cfg, "ubiquity::system_load_data()") 
      vp(cfg, sprintf("unable to find the specified file >%s<", data_file)) 
      vp(cfg, " ------------------------------------")
    
    }
  }

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
#'@return Ubiquity system object with the specified parameter set active
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#' 
#' # Selecting the default parameter set
#' cfg = system_select_set(cfg, "default")
#'}
system_select_set = function(cfg, set_name='default', parameter_names=NULL){
#
# takes the system information variable cfg and makes the values in the string
# 'set name'  the active values
#

# defining parameters for the current set
if(is.null(cfg$parameters$sets[[set_name]])){
  vp(cfg,sprintf('Warning: Could not find set: %s', set_name))
  vp(cfg,sprintf('   Returning the default set instead'))
  set_name = 'default'
  cfg$parameters$matrix$value = cfg$parameters$sets$default$values
  cfg$parameters$current_set  = 'default'
  }

  cfg$parameters$matrix$value  = cfg$parameters$sets[[set_name]]$values
  cfg$parameters$current_set   = set_name
  p_idx = 1
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
#'@title Fetch System Parameters
#'
#'@description
#' Fetch the parameters of the currently selected parameter set. To switch
#' between parameter sets use \code{\link{system_select_set}}
#'
#'@param cfg ubiquity system object    
#'
#'@return List of parameters for the selected parameter set
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Fetching the default parameter set
#' parameters = system_fetch_parameters(cfg)
#'}
system_fetch_parameters <- function(cfg){
  return(cfg$parameters$values)}

#'@export
#'@title Fetch Mathematical Set 
#'
#'@description
#' Fetch the elements of the specified mathematical set that was defined in the system file.
#'
#'@param cfg ubiquity system object    
#'@param set_name name of mathematical set
#'
#'@return A sequence containing the elements of the parameter set or NULL if if there was a problem.
#'
#'@examples
#' \donttest{
#' # Creating a system file from the pbpk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "pbpk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Fetching the contents of the ORG mathematical set
#' ORG_elements = system_fetch_set(cfg, "ORG")
#'}
system_fetch_set <- function(cfg, set_name=NULL){
  set_contents = NULL
  isgood = TRUE

  if(set_name %in% names(cfg$options$math_sets)){
    set_contents = cfg$options$math_sets[[set_name]]
  } else {
    isgood = FALSE
    vp(cfg, paste("Error: mathematical set: >", set_name ,"< was not defined", sep=""))
    if(length(names(cfg$options$math_sets)) > 0){
      vp(cfg, paste("The following sets are defined for this system")) 
      vp(cfg, paste(names(cfg$options$math_sets), collapse=", "))
    } else {
      vp(cfg, "There are no sets defined for this system") }
  }

  if(!isgood){
    vp(cfg, "ubiquity::system_fetch_set()")
  }
  
  return(set_contents)}

#'@export
#'@title Fetch Variability Terms
#'@description Extract elements of the current variance/covariance matrix
#' specified in the system file with \code{<IIV:?:?> ?}, \code{<IIVCOR:?:?>?}, \code{<IIVSET:?:?> ?}, \code{<IIVCORSET:?:?>?}
#'
#'@param cfg ubiquity system object    
#'@param IIV1 row name of the variance/covariance matrix
#'@param IIV2 column name of the variance/covariance matrix 
#'
#'@return Value from the variance/covariance matrix   
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Covariance term for ETACL and ETAVc
#' val = system_fetch_iiv(cfg, IIV1="ETACL", IIV2="ETAVc")
#'}
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
    vp(cfg, "ubiquity::system_fetch_iiv() ")
    vp(cfg, "No IIV information was found") 
    vp(cfg, "These can be specified using: ") 
    vp(cfg, "<IIV:?>, <IIV:?:?>, and <IIVCOR:?:?> ")
  }
return(VALUE)}


#'@export
#'@title Zero All Model Inputs
#'@description Multiple default inputs can be specified in the system file. At
#' the scripting level this function can be used to set all inputs to zero.
#' Then only the subsequently specified inputs will be applied.
#'
#'@param cfg ubiquity system object    
#'@param bolus Boolean value indicating weather bolus inputs should be set to zero
#'@param rates Boolean value indicating weather infusion rate inputs should be set to zero
#'
#'@return Ubiquity system object with the specified inputs set to zero
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Clear only infusion rates
#' cfg = system_zero_inputs(cfg, bolus=TRUE, rates=FALSE)
#'
#' # Clear all inputs:
#' cfg = system_zero_inputs(cfg)
#'}
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
#'@return Ubiquity system object with the covariate set
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Setting the covariate WT to 50
#' cfg = system_set_covariate(cfg, 
#'                            covariate = "WT",
#'                            times     = c(0), 
#'                            values    = c(50))
#'}
system_set_covariate <- function(cfg, covariate, times, values){
  isgood = TRUE
  if(!(length(times) == length(values)) ) {
    vp(cfg, "The times and values have differnt lengths") 
    isgood = FALSE
    }
  if(!(covariate %in% names(cfg$options$inputs$covariates))){
    vp(cfg, sprintf("The covariate name %s could not be found", covariate)) 
    isgood = FALSE
  }
  if(isgood){
    cfg$options$inputs$covariates[[covariate]]$times$values  = times 
    cfg$options$inputs$covariates[[covariate]]$values$values = values
  } else {
    vp(cfg, sprintf(" Something went wrong and the covariate, ")) 
    vp(cfg, sprintf(" was not set, see the messages above.")) }

return(cfg)}


#'@export
#'@title Set Infusion Rate Inputs
#'@description Defines infusion rates specified in the system file using  \code{<R:?>}
#'
#'@param cfg ubiquity system object    
#'@param rate name of infusion rate    
#'@param times list of time values   
#'@param levels corresponding list of infusion values   
#'
#'@return Ubiquity system object with the infusion rate set
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Clearing all inputs
#' cfg = system_zero_inputs(cfg)
#'
#' # 5 minute infusion at 10 mg/min
#' cfg = system_set_rate(cfg,
#'            rate   = "Dinf",
#'            times  = c(0,  5), 
#'            levels = c(10, 0))
#'}
#'@seealso \code{\link{system_zero_inputs}}
system_set_rate <- function(cfg, rate, times, levels){
  isgood = TRUE
  if(!(length(times) == length(levels)) ) {
    vp(cfg, "The times and levels have differnt lengths") 
    isgood = FALSE
    }
  if(!(rate %in% names(cfg$options$inputs$infusion_rates))){
    vp(cfg, sprintf("The rate name %s could not be found", rate)) 
    isgood = FALSE
  }
  if(isgood){
    cfg$options$inputs$infusion_rates[[rate]]$times$values  = times 
    cfg$options$inputs$infusion_rates[[rate]]$levels$values = levels
  } else {
    vp(cfg, "Something went wrong and the rate, ") 
    vp(cfg, "was not set, see the messages above.") }
return(cfg)}

#'@export
#'@title Setting Analysis Options
#'@description Different options associated performing analyses (e.g running
#' simulations, performing parameter estimation, logging, etc.) can be set
#' with this function
#'
#'@param cfg ubiquity system object    
#'@param group options are grouped together by the underlying activity being performed: "estimation",  "general", "logging", "simulation", "solver", "stochastic", or "titration"
#'@param option for each group there are a set of options 
#'@param value corresponding value for the option 
#'
#'@return Ubiquity system object with the option set
#'
#'@details 
#'
#' \bold{\code{group="estimation"}}
#'
#' The default estimation in R is performed using either the \code{optim} or \code{optimx} libraries.
#' This is selected by setting the \code{optimizer} option:
#'  
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "optimizer",
#'                        value  = "optim")
#' }
#'  
#' The optimization routine then specified using the \code{method}. By default this \code{option} is
#' set to \code{Nelder-Mead}.
#'  
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "method",
#'                        value  = "Nelder-Mead")
#' }
#'  
#' And different attributes are then selected using the control.
#'  
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "control",
#'                        value  = list(trace  = TRUE,
#'                                      maxit  = 500,
#'                                      REPORT = 10))
#' }
#' 
#' For the different methods and control options, see the documentation for the \code{optim}
#' and \code{optimx} libraries.
#'
#' To perform a global optimization you can install either the particle swarm (\code{pso})
#' genetic algorithm (\code{GA}) libraries.
#' To use the particle swarm set the \code{optimizer} and \code{method}:
#'  
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "optimizer",
#'                        value  = "pso")
#'
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "method",
#'                        value  = "psoptim")
#' }
#' 
#' The control option is a list described \code{pso} documentation.
#'
#' To use the genetic algorithm set the optimizer and method:
#' 
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "optimizer",
#'                        value  = "ga")
#'
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "method",
#'                        value  = "ga")
#' }
#' 
#' The control option is a list and the list elements are the named options in the GA
#' documentation. Use the following as an example:
#' 
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "control",
#'                        value  = list(maxiter  = 10000,
#'                                     optimArgs = list(
#'                                       method  = "Nelder-Mead",
#'                                       maxiter = 1000)))
#' }
#' 
#' To alter initial guesses see: \code{\link{system_set_guess}}
#'
#' When performing parameter estimation, the internal function
#' \code{system_od_general} is used. This is the function that simulates your
#' system at the conditions defined for the different cohorts. This is pretty
#' flexible but if you want to go beyond this you can set the
#' \code{observation_function} option:
#'
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "observation_function",
#'                        value  = "my_od")
#' }
#'
#' That will instruct the optimziation routines to use the user defined
#' function \code{my_od}. You will need to construct that function to have the
#' same input/output format as \code{\link{system_od_general}}.
#'
#' \bold{\code{group=general}}
#'
#' \itemize{
#' \item \code{"output_directory"}   = String where analysis outputs will be
#'     placed. Generally you wont want to change this, but it can be useful in Shiny
#'     apps where you need to have each shiny user generate output in that
#'     users directory : \code{file.path(".", "output")}
#' }
#'
#' \bold{\code{group=logging}}
#'
#' By default ubiquity prints different information to the console and logs this
#' information to a log file. The following options can be used to control
#' this behavior:
#'
#' \itemize{
#' \item \code{"enabled"}   = Boolean variable to control logging: \code{TRUE}
#' \item \code{"file"}      = String containing the name of the log file: \code{file.path("transient", "ubiquity_log.txt")}
#' \item \code{"timestamp"} = Boolean switch to control appending a time stamp to log entries: \code{TRUE}
#' \item \code{"ts_str"}    = String format of timestamp: "%Y-%m-%d %H:%M:%S"
#' \item \code{"debug"}     = Boolean switch to control debugging (see below): \code{FALSE}
#' \item \code{"verbose"}   = Boolean switch to control printing to the console \code{FALSE}
#' }
#'
#'
#'
#' To enable debugging of different functions (like when performing esitmation), 
#' set the \code{debug} option to \code{TRUE}. Important function calls will be 
#' trapped and information will be logged and reported to the console.
#'
#' \preformatted{
#'cfg = system_set_option(cfg, 
#'                        group  = "estimation",
#'                        option = "debug",
#'                        value  = FALSE)
#'}
#'
#' \bold{\code{group="simulation"}}
#'\itemize{
#' \item \code{"include_important_output_times"} - Automatically add bolus, infusion rate switching times, etc: \code{"yes"}(default), \code{"no"}.
#' \item \code{"integrate_with"} - Specify if the ODE solver should use the Rscript (\code{"r-file"}) or compiled C (\code{"c-file"}), if the build process can compile and load the C version it will be the default otherwise it will switch over to the R script.
#' \item \code{"output_times"} - Vector of times to evaulate the simulation (default \code{seq(0,100,1)}).
#' \item \code{"solver"} - Selects the ODE solver: \code{"lsoda"} (default), \code{"lsode"}, \code{"vode"}, etc.; see the documentation for \code{\link[deSolve]{deSolve}} for an exhaustive list.
#' \item \code{"sample_bolus_delta"} - Spacing used when sampling around bolus events (default \code{1e-6}). 
#' \item \code{"sample_forcing_delta"} - Spacing used when sampling around forcing functions (infusion rates, covariates, etc) (default \code{1e-3}). 
#' }
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
#'cfg=system_set_option(cfg,
#'                      group  = "simulation",
#'                      option = "solver", 
#'                      value  = "vode")
#'
#'cfg=system_set_option(cfg,
#'                      group  = "solver",
#'                      option = "hmax", 
#'                      value  = 0.01)
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
#'  \item\code{"ssp"} - A list of the calculated static secondary parameters to include (default all parameters defined by \code{<As>})
#'  \item\code{"outputs"} - A list of the predicted outputs to include (default all outputs defined by \code{<O>})
#'  \item\code{"states"} - A list of the predicted states to include(default all states)
#'  \item\code{"sub_file"} - Name of data set loaded with (\code{\link{system_load_data}}) containing subject level parameters and coviariates
#'  \item\code{"sub_file_sample"} - Controls how subjects are sampled from the dataset
#'  }
#'
#' If you wanted to generate \code{1000} subjects but only wanted the parameters, you would
#' use the following:
#' \preformatted{
#'cfg = system_set_option(cfg,
#'                        group  = "stochastic", 
#'                        option = "nsub ",
#'                        value  = 1000)
#'
#'cfg = system_set_option(cfg,
#'                        group  = "stochastic", 
#'                        option = "ponly",
#'                        value  = TRUE )
#' }
#'
#'
#' If you wanted to exclude both states and secondary parameters, while only including 
#' the output \code{Cp_nM}, you would do the following:
#' \preformatted{
#'
#'cfg = system_set_option (cfg, 
#'                         group  = "stochastic",
#'                         option = "ssp",
#'                         value  = list())
#'
#'cfg = system_set_option (cfg, 
#'                         group  = "stochastic",
#'                         option = "states",
#'                         value  = list())
#'
#'cfg = system_set_option (cfg, 
#'                         group  = "stochastic",
#'                         option = "outputs",
#'                         value  = c("Cp_nM")) 
#' }
#'
#' To pull subject information from a data file instead of generating the subject
#' parameters from IIV information the \code{sub_file} option can be used. The value here
#' \code{SUBFILE_NAME} is the name given to a dataset loaded with
#' (\code{\link{system_load_data}}):
#'
#' \preformatted{
#'cfg=system_set_option(cfg, 
#'                      group  = "stochastic",
#'                      option = "sub_file",
#'                      value  = "SUBFILE_NAME")
#' }
#'  
#' Sampling from the dataset can be controlled using the \code{sub_file_sample} option:
#'  
#' \preformatted{
#'cfg=system_set_option(cfg, 
#'                      group  = "stochastic",
#'                      option = "sub_file_sample",
#'                      value  = "with replacement")
#' }
#'  
#' Sampling can be done sequentially (\code{"sequential"}), with replacement
#' (\code{"with replacement"}), or without replacement (\code{"without replacement"})
#'
#' \bold{\code{group="titration"}}
#'
#' \code{"titrate"} - By default titration is disable (set to \code{FALSE}). If you are
#' going to use titration, enable it here by setting this option to \code{TRUE}.
#' This will force #' \code{\link{simulate_subjects}} to use 
#' \code{\link{run_simulation_titrate}} internally when running simulations.
#'
system_set_option <- function(cfg, group, option, value){
 
  groups = c('general', 'solver', 'stochastic', 'simulation', 'estimation', 'logging', 'titration')
  
  errormsgs = c()
  # checking the user input
  isgood = TRUE
  if(group %in% groups){
    #
    # Loading required packages based on options selected 
    #
    if(group == "general" & option == "output_directory"){
      # we're going to make sure the directory exists
      if(!dir.exists(value)){
         if(!dir.create(value)){
           isgood = FALSE
           errormsgs = c(errormsgs, paste("unable to create output_directory >", value,"<", sep=""))
           errormsgs = c(errormsgs, paste("output_directory not set"))
         }
      }

      if(isgood){
        cfg[["options"]][["misc"]][["output_directory"]] = value
      }
    }

    if(group == "simulation" & option == "parallel"){
      if(value == "multicore"){
        if(!system_req("doParallel")){
          isgood = FALSE
          errormsgs = c(errormsgs, "Unable to load the doParallel package")
          errormsgs = c(errormsgs, 'install.packages("doParallel")')
        }
        if(!system_req("foreach")){
          isgood = FALSE
          errormsgs = c(errormsgs, "Unable to load the foreach package")
          errormsgs = c(errormsgs, 'install.packages("foreach")')
        }
        if(!isgood){
          errormsgs = c(errormsgs, "Unable to load one or more packages needed for the  multicore option") }
      }
    }

    if(group == "estimation" & option == "optimizer"){
      if(value == "pso"){
        if(!system_req("pso")){
          isgood = FALSE
          errormsgs =  c(errormsgs, errormsgs, "Unable to load the particle swarm optimizer (pso) package")
          errormsgs =  c(errormsgs, errormsgs, 'install.packages("pso")')
        }
      }
    }
    if(group == "estimation" & option == "optimizer"){
      if(value == "ga"){
        if(!system_req("GA")){
          isgood = FALSE
          errormsgs = c(errormsgs, "Unable to load the Genetic Algoriths (GA) package")
          errormsgs = c(errormsgs, 'install.packages("GA")')
        }
      }
    }

    if(group == "estimation" & option == "observation_function"){
      if(!exists(value, mode="function")){
        isgood = FALSE
        errormsgs = c(errormsgs, "Unable to set the observation_function")
        errormsgs = c(errormsgs, paste0('The user defined function >', value, '< ', "was not found."))
        errormsgs = c(errormsgs, paste0("You must create this object before setting this option."))
      }
    }

    if(isgood){
      # setting stochastic options
      if(group == 'stochastic'){
        if((option == "states") | (option == "outputs")){
          for(val in value){
            if(!(val %in% names(cfg$options$mi[[option]]))){
              errormsgs = c(errormsgs, paste(option, " >", val, "< not found", sep=""))
              isgood = FALSE
            }
          } 
        }
        if((option == "ssp")){
          for(val in value){
            if(!(val %in% names(cfg[["options"]][["ssp"]]))){
              errormsgs = c(errormsgs, paste("static secondary parameter (ssp) >", val, "< not found", sep=""))
              isgood = FALSE
            }
          } 
        }

        # Making sure the specified dataset is loaded
        if(option == "sub_file"){
          # If value is NULL then we're disabling the sub_file
          if(!is.null(value)){
            # if it's not null we want to check if the dataset has been
            # defined
            if(!(value %in% names(cfg$data))){
              errormsgs = c(errormsgs, paste("Error: dataset >", value, "< not found, please load first", sep=""))
              errormsgs = c(errormsgs, "using system_load_data()")
              isgood = FALSE
            }
          }
        }
        if(option == "sub_file_sample"){
          if(!any(value == c("with replacement", "sequential", "without replacement"))){
            errormsgs = c(errormsgs,  paste("The value", toString(value), "is invalid and must be one of the following"))
            errormsgs = c(errormsgs,        "  sequential          - sample from data file sequentially")
            errormsgs = c(errormsgs,        "  with replacement    - sample from data file with replacement")
            errormsgs = c(errormsgs,        "  without replacement - sample from data file with out replacement")
            isgood = FALSE
          }
        }


        if(isgood){
          cfg$options$stochastic[[option]] = value
        }else{
           errormsgs = c( errormsgs,  paste("The following option >", option, "< is not valid", sep=""))
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
             errormsgs = c(errormsgs, "The titrate option should be TRUE or FALSE")
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
    }
    
  } else {
    # flagging a bad group
    isgood = FALSE
    errormsgs = c(errormsgs, paste("The specified group >", group,"< is invalid", sep=""))
    errormsgs = c(errormsgs, "Valid groups are:")
    for(valid in groups){
      errormsgs = c(errormsgs, paste("   ->", valid))}
  }
  
  
  # If the error flag has been switched above, then we print some inforamtion for the user
  if(!isgood){
    vp(cfg, "ubiquity::system_set_option()                 ", "h1") 
    vp(cfg, "Something went wrong and the option ") 
    vp(cfg, "was not set:")
    vp(cfg, errormsgs)
    }
  
return(cfg)}

#'@export
#'@title Titration Rules
#'@description Defines a new titration rule and the times when that rule is evaluated
#'
#'@param cfg ubiquity system object    
#'@param name name for the titration rule
#'@param times list of times when the rule will be evaluated 
#'@param timescale time scale associated with the titration times (as defined by \code{<TS:?>})
#'
#'@return Ubiquity system object with the titration rule created
#'
#'@details
#' \preformatted{
#'cfg = system_new_tt_rule(cfg,
#'                         name      = "rname",
#'                         times     = c(0, 2, 4),
#'                         timescale = "weeks")'
#' }
#' A titration rule identifies a set of times (\code{times}) and an associated time
#' scale (\code{timescale}) in which titration events can potentially occur. Any
#' times scale, as defined in the system file with \code{<TS:?>}, can be used in
#' place of "weeks" above. The \code{name}, \code{"rname"} above, is used to link the
#' titration rule to different conditions discussed below. The name should be
#' a string beginning with a letter, and it can contain any combination of
#' numbers, letters, and underscores. With the rule created we can then add conditions to that rule.'
#'
#'@seealso \code{\link{system_set_tt_cond}}, \code{\link{run_simulation_titrate}}
system_new_tt_rule <- function(cfg, name, times, timescale){

  isgood = TRUE
  # empty list holding the new titration inforamtion

  errormsgs = c()

  if(!timescale %in% names(cfg$options$time_scales)){
    isgood = FALSE
    errormsgs = c(errormsgs, paste("The timescale: >", timescale, "< was not defined", sep=""))
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
    vp(cfg, "ubiquity::system_new_tt_rule()", fmt="h1") 
    vp(cfg, "Something went wrong and the        ") 
    vp(cfg, "titration rule was not set          ") 
    vp(cfg, errormsgs) 
    }
return(cfg)
}


#'@export
#'@title Define Titration Triggers and Actions
#'@description Once a rule has been defined using
#' \code{\link{system_new_tt_rule}}, it can then be used by specifying checks at
#' each of the titration time points that, when true, will perform some actions. 
#'
#'@param cfg ubiquity system object    
#'@param name string containing the name for the titration rule to which this condition applies
#'@param cond string that evaluates a boolean value that is \code{TRUE} when the action should be triggered
#'@param action stringing that evaluates to what should be done when the condition is met (e.g. changing the dose, state change, etc) 
#'@param value code to be stored in the titration history to track when this condition has been triggered
#'
#'@return Ubiquity system object with the titration condition defined
#'
#'
#'@details
#'
#' The general syntax for setting a new condition is:
#'
#' \preformatted{
#'cfg = system_new_tt_cond(cfg,
#'                         name   = "rname",
#'                         cond   = "BOOLEAN EXPRESSION",
#'                         action = "EXPRESSION",
#'                         value  = "VALUE")
#'}
#'
#' The \code{name}
#' input will associate this condition with a previously defined rule. For each
#' time defined when the rule was created, the condition (\code{cond}) will be
#' evaluated. If that condition evaluates as \code{TRUE} then the \code{action} will be
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
#' mathematical expressions or even complicated user defined functions.
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
#' \bold{Note:} Protype functions are strings but sometimes it is necessary to
#' specify strings within this string. For the main string use double quotes (")
#' and for the internal strings use single quotes (')
#'
#' \bold{\code{SI_TT_BOLUS}}
#'
#' The simplest way to apply a bolus when the condition is true is to use the following:
#'
#' \preformatted{
#'action = "SI_TT_BOLUS[state=’At’, 
#'                      values=c(10, 10, 10), 
#'                      times=c(0, 1, 2)]"
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
#'action = "SI_TT_BOLUS[state    = ’At’, 
#'                      values   = c(5, 5, 10), 
#'                      times    = c(0, 2, 4), 
#'                      repdose  = ’last’, 
#'                      number   = 7, 
#'                      interval = 4]"
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
#'action = "SI_TT_RATE[rate=’Dinf’, times=c(0, 60), levels=c(20.0, 0)]"
#' }
#'
#' If we wanted to do this every day for 9 more days (a total of 10 days) we can repeat
#' the sequence:
#'
#' \preformatted{
#'action = "SI_TT_RATE[rate     = ’Dinf’, 
#'                     times    = c(0, 60), 
#'                     levels   = c(20, 0), 
#'                     repdose  = ’sequence’, 
#'                     number   = 9, 
#'                     interval = 24*60]"
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
#'action = "SI_TT_STATE[Cc][0.0]"
#' }
#'
#' The value here is a number but you can use any mathematical
#' combination of variables available in the titration environment. Also you
#' can create your own user function and place the function call within the
#' brackets above.
#'
#' \bold{Titration Environment}
#'
#' The \code{cond}, \code{action}, and \code{value} statements can use any variables available in
#' the titration environment. If you want to perform complicated actions, you can
#' simply create a user defined functions and pass it the variables from the
#' titration environment that you need. These include named variables from the
#' model as well as internal variables used to control the titration.
#'
#' \bold{States and Parameters}
#'
#' System parameters (\code{<P>}), static secondary parameters (\code{<As>}) and 
#' the initial value of covariates are available. Also the state values 
#' (at the current titration time) can be used. These are all available as 
#' the names specified in the \code{system.txt} file. Since system resets
#' (\code{SI_TT_STATE}) are processed first, any changes made to states are 
#' the values that are active for other actions.
#'
#' \bold{Internal Simulation Variables}
#'
#' Internal variables are used to control titration activities. These variables can also be used in the conditions and actions.
#'
#' \itemize{
#'   \item \code{SIMINT_p} - list of system parameters
#'   \item \code{SIMINT_cfg} - system configuration sent into the titration routine
#'   \item \code{SIMINT_cfgtt}- system configuration at the current titration event time
#'   \item \code{SIMINT_ttimes} - vector of titration times (in simulation units)
#'   \item \code{SIMINT_ttime} - current titration time  (in simulation units)
#'   \item \code{SIMINT_tt_ts} - list of time scales for the current titration
#'   \item \code{SIMINT_history} - data frame tracking the history of conditions that evaluated true with the following structure:
#'   \item \itemize{
#'         \item \code{tname} - name of titration rule
#'         \item \code{value} - value indicating condition that was satisfied
#'         \item \code{simtime} - simulation time when that rule/value were triggered
#'         \item \code{timescale} -  time at the rule timescale when that rule/value were triggered
#' }
#' }
#'
#' \bold{Individual Simulations}
#'
#' To run an individual titration simulation use the following:
#'
#' \preformatted{
#'som = run_simulation_titrate(parameters, cfg)
#' }
#'
#'  This provides the same output as \code{\link{run_simulation_ubiquity}} with
#'  two extra fields. The first, \code{som$titration}, contains three columns for each
#'  titration rule. The columns will have a length equal and corresponding to the
#'  simulation times. If the rule name is rname, then the column headers will have
#'  the following names and meanings:
#' \itemize{
#'   \item \code{tt.rname.value} - Value of the rule for the active condition or -1 if not triggered
#'   \item \code{tt.rname.simtime} - Simulation time where the last condition became active
#'   \item \code{tt.rname.timescale} - Simulation time in the time scale the rule was specified in
#' }
#'
#'  The second field is \code{som$titration_history} which contains a summary list of all of the titration events that were triggered.
#' \itemize{
#'    \item \code{tname} - Titration rule name
#'    \item \code{value} - Value of the rule for the active condition or -1 if not triggered   
#'    \item \code{simtime} - Simulation time where the last condition became active
#'    \item \code{timescale} - Simulation time in the time scale the rule was specified in
#' }
#' 
#' To convert this structured list into a data frame the \code{\link{som_to_df}} command can be used:
#' 
#' \preformatted{
#'sdf = som_to_df(cfg, som)
#' }
#'
#' To run stochastic titration simulations, the same function is used:
#'
#' \preformatted{
#'som = simulate_subjects(parameters, cfg)
#' }
#'
#' This will add a data a list element called \code{som$titration} with three
#' fields for each titration rule:
#'
#' \itemize{
#'   \item \code{tt.rname.value} - Value of the rule for the active condition or -1 if not triggered
#'   \item \code{tt.rname.simtime} - Simulation time where the last condition became active
#'   \item \code{tt.rname.timescale} - Simulation time in the time scale the rule was specified in
#' } 
#'
#' Each of these fields is a matrix with an entry for each simulation time
#' (column) and each subject (row). This data structure can also be converted to
#' a data frame using \code{som_to_df}.
#' 
#'
#'@seealso \code{\link{system_new_tt_rule}}, \code{\link{run_simulation_titrate}},  \code{\link{som_to_df}}, \code{\link{simulate_subjects}} 
system_set_tt_cond <- function(cfg, name, cond, action, value='-1'){

  isgood = TRUE

  errormsgs = c()

  if(!(name %in% names(cfg$titration$rules))){
    errormsgs = c(errormsgs, paste( "The rule >", name, "< was not found, first create the rule using system_new_tt_rule then add conditions", sep=""))
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
    vp(cfg, "ubiquity::system_set_tt_cond()",  fmt="h1") 
    vp(cfg, "Something went wrong and the        ") 
    vp(cfg, "titration condition was not set     ") 
    vp(cfg, errormsgs) 
    }


return(cfg)
}

#'@export
#'@title Parse String for Prototype Functions
#'@keywords internal
#'@description A string can contain any number of prototype functions, and this function will find them and replace them with the actual R code.
#'
#'@param cfg ubiquity system object    
#'@param str string
#'
#'@return String with the prototype functions replaced
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
      vp(cfg, paste('String:        ', str,                       sep=""))
      vp(cfg, paste('Pattern name:  ', pname,                     sep=""))
      vp(cfg, paste('Pattern:       ', patterns[[pname]]$pattern, sep=""))
      vp(cfg, paste('Error Message: ', errormsg,                  sep=""))
    
    }
  
  }

 return(newstr)
}


#'@export
#'@title Parse Prototype Functions for Arguments
#'@keywords internal
#'@description 
#' Parses strings to find abstract functions (of the format
#' SIFUNC[ARG1][ARG2][ARG3] and extract the arguments from that function and
#' replace it with actual functions and any additional arguments needed
#'
#'@param str string containing the prototype function call
#'@param pattern string indicating the start of the function eg. \code{"SI_TT_BOLUS["}
#'@param replace string to replace \code{pattern} with
#'@param narg number of arguments to prototype function
#'@param op string used to indicating open parenthesis 
#'@param cp string used to indicating close parenthesis 
#'
#'@return string containing the actual function call/code built from the prototype function
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
#'@title Actual Function Called by \code{SI_TT_BOLUS}
#'@keywords internal
#'@description The prototype function \code{SI_TT_BOLUS} provides an interface to this function. Based on the input from \code{SI_TT_BOLUS}
#' bolus inputs will be updated for the current titration time. 
#' 
#'@param cfg       ubiquity system object    
#'@param state     dosing state/compartment (Defined in \code{<B:events>})
#'@param values    vector of dosing amounts (in dosing units defined by \code{<B:events>})
#'@param times     vector of dosing times relative to the current titration time (in # time units defiend by \code{<B:times>})
#'@param tt_ts     list of timescale values for the current titration time
#'@param tsinfo    list with timescale information for inputs (bolus, rates, etc)
#'@param repdose   \code{"none"}, \code{"last"}, \code{"all"}
#'@param interval  interval to repeat in the units defined in \code{<B:times>}
#'@param number    number of times to repeat 
#'
#'@return ubiquity system object with the bolus dosing updated.
system_set_tt_bolus <- function(cfg, state, values, times, tt_ts,  tsinfo, repdose="none", interval=1, number=0){


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
#'@title Actual Function Called by \code{SI_TT_RATE}
#'@description The prototype function \code{SI_TT_RATE} provides an abstract interface to this function. Based on the input from \code{SI_TT_RATE}
#' infusion rate inputs will be updated for the current titration time. 
#' 
#'@param cfg       ubiquity system object    
#'@param rate      name of the infusion rate to update(Defined in \code{<R:?>})
#'@param times     vector of switching times relative to the current titration time (in time units defined by \code{<R:?>})
#'@param levels    vector of infusion rates (in dosing units defined by \code{<R:?>})
#'@param tt_ts     list of timescale values for the current titration time
#'@param tsinfo    list with timescale information for inputs (bolus, rates, etc)
#'@param repdose   \code{"none"} or \code{"sequence"}
#'@param interval  interval to repeat in the units defined in \code{<R:?>}
#'@param number    number of times to repeat 
#'
#'@return ubiquity system object with the infusion rates updated.
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

#'@export
#'@title Set Bolus Inputs
#'@description Defines infusion rates specified in the system file using  \code{<B:times>} and   \code{<B:events>} 
#'
#'@param cfg ubiquity system object    
#'@param state name of the state to apply the bolus
#'@param times list of injection times 
#'@param values corresponding list injection values     
#'
#'@return Ubiquity system object with the bolus information set
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Clearing all inputs
#' cfg = system_zero_inputs(cfg)
#'
#' # SC dose of 200 mg
#' cfg = system_set_bolus(cfg, state   ="At", 
#'                             times   = c(  0.0),  #  day
#'                             values  = c(200.0))  #  mg
#'}
#'@seealso \code{\link{system_zero_inputs}}
system_set_bolus <- function(cfg, state, times, values){
  
  errormsgs = c()

  # checking the user input
  isgood = TRUE
  if(!(length(times) == length(values))){
    errormsgs = c(errormsgs, "The times and values have differnt lengths")
    errormsgs = c(errormsgs, " ")
    isgood = FALSE
    }
  if(!(state %in% names(cfg$options$inputs$bolus$species))){
    errormsgs = c(errormsgs, paste("The state >", state, "< could not be found", sep=""))
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
    vp(cfg, sprintf("system_set_bolus()", fmt="h1")) 
    vp(cfg, sprintf("Something went wrong and the bolus  ")) 
    vp(cfg, sprintf("was not set:")) 
    vp(cfg, errormsgs) 
    
    }

return(cfg)}

#'@export
#'@title Set Variability Terms
#'@description Set elements of the current variance covariance matrix
#' specified in the system file with \code{<IIV:?:?> ?}, \code{<IIVCOR:?:?>?}, \code{<IIVSET:?:?> ?}, \code{<IIVCORSET:?:?>?}
#'
#'@param cfg ubiquity system object    
#'@param IIV1 row name of the variance/covariance matrix
#'@param IIV2 column name of the variance/covariance matrix element
#'@param value value to assign to the variance/covariance matrix element
#'
#'@return Ubiquity system object with IIV information set
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Clearing all inputs
#' cfg = system_zero_inputs(cfg)
#'
#' # Setting the covariance element for CL and Vc to 0.03
#' cfg = system_set_iiv(cfg,
#'                      IIV1 = "ETACL",
#'                      IIV2 = "ETAVc",
#'                      value=0.03)
#'}
#'@seealso \code{\link{system_fetch_iiv}}
system_set_iiv <- function(cfg, IIV1, IIV2, value){
  if("iiv" %in% names(cfg)){
    IIV1_idx = match(c(IIV1), names(cfg$iiv$iivs))
    IIV2_idx = match(c(IIV2), names(cfg$iiv$iivs))
    
    if(is.na(IIV1_idx)){
      vp(cfg, paste("IIV >", IIV1, "<not found", sep="")) 
    }else if(is.na(IIV2_idx)){
      vp(cfg, paste("IIV >", IIV2, "<not found", sep="")) 
    }else{
      cfg$iiv$values[IIV1_idx, IIV2_idx] = value
      cfg$iiv$values[IIV2_idx, IIV1_idx] = value
    }
  } else {
    vp(cfg, "ubiquity::system_set_iiv()", fmt="h1")
    vp(cfg, "No IIV information was found") 
    vp(cfg, "These can be specified using: ") 
    vp(cfg, "<IIV:?>, <IIV:?:?>, and <IIVCOR:?:?> ")
  }
return(cfg)}

#-----------------------------------------------------------
#'@export
#'@title View Information About the System
#'@description Displays information (dosing, simulation options, covariates,
#' etc) about the system.
#'
#'@param cfg ubiquity system object    
#'@param field string indicating the aspect of the system to display
#'@param verbose Boolean variable that when set to true will echo the information to the screen 
#'
#'@return sequence of strings with system in formation (one line per element)
#'
#' The \code{field} 
#' \itemize{
#'    \item \code{"all"} will show all information about the system
#'    \item \code{"parameters"} summary of parameter information
#'    \item \code{"bolus"} currently set bolus dosing
#'    \item \code{"rate"} infusion rate dosing 
#'    \item \code{"covariate"} covariates
#'    \item \code{"iiv"} variance/covariance information
#'    \item \code{"datasets"} loaded datasets
#'    \item \code{"simulation"} simulation options
#'    \item \code{"estimation"} estimation options
#'    \item \code{"nca"} non-compartmental analyses that have been performed
#' }
#'@examples
#' # To log and display the current system information:
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#'   msgs = system_view(cfg, verbose=TRUE)
#' }
system_view <- function(cfg,field="all", verbose=FALSE) {
  
  msgs = c()
  
  # Processing infusion rate information
  if(field == "all" | field== "parameters"){
      msgs = c(msgs, sprintf(" Parameter Information"))
      msgs = c(msgs, sprintf(" Parameter set selected:"))
      msgs = c(msgs, sprintf("   Short Name:  %s", cfg$parameters$current_set))
      msgs = c(msgs, sprintf("   Description: %s", cfg$parameters$sets[[cfg$parameters$current_set]]$name))
      msgs = c(msgs, sprintf(" Default parameters for current set:"))
      msgs = c(msgs, sprintf("%s |  %s | %s",
                     pad_string('name', 18), 
                     pad_string('value', 12), 
                     pad_string('units', 15)))
      msgs = c(msgs, paste(replicate(52, "-"), collapse = ""))
      for(pidx in 1:length(cfg$parameters$matrix$name)){
      msgs = c(msgs, sprintf("%s |  %s | %s",
                  pad_string(as.character(cfg$parameters$matrix$name[pidx]), 18), 
                  var2string(cfg$parameters$matrix$value[pidx], 12), 
                  pad_string(as.character(cfg$parameters$matrix$units[pidx]), 15)))
      }
      msgs = c(msgs, paste(replicate(52, "-"), collapse = ""))
      msgs = c(msgs, " ")
  }
  
  
  # Processing bolus information
  if(field == "all" | field== "bolus"){
    if("bolus" %in% names(cfg$options$inputs))  {
      msgs = c(msgs, sprintf(" Bolus dosing details "))
      msgs = c(msgs, sprintf("%s |  %s | %s | %s",
                      pad_string("field", 10),
                      pad_string("values", 10),
                      pad_string("scaling", 10),
                      pad_string("units", 10)))
      msgs = c(msgs, paste(replicate(50, "-"), collapse = ""))
      msgs = c(msgs, sprintf("%s |  %s | %s | %s",
                     pad_string("times", 10),
                     pad_string(paste(cfg$options$inputs$bolus$times$values, collapse=" "), 10),
                     pad_string(cfg$options$inputs$bolus$times$scale, 10),
                     pad_string(cfg$options$inputs$bolus$times$units, 10)))
      
      for(species in names(cfg$options$inputs$bolus$species)){
        msgs = c(msgs,  sprintf("%s |  %s | %s | %s",
                   pad_string(species, 10),
                   pad_string(paste(cfg$options$inputs$bolus$species[[species]]$values, collapse=" "), 10),
                   pad_string(cfg$options$inputs$bolus$species[[species]]$scale, 10),
                   pad_string(cfg$options$inputs$bolus$species[[species]]$units, 10)))
      }
      msgs = c(msgs, paste(replicate(50, "-"), collapse = ""))
      
    } else {
      msgs = c(msgs, "No bolus information found") }
  }
  
  # Processing infusion rate information
  if(field == "all" | field== "rate"){
    if("infusion_rates" %in% names(cfg$options$inputs))  {
      msgs = c(msgs, sprintf(" Infusion rate details "))
      msgs = c(msgs, sprintf("%s | %s | %s | %s | %s",
                 pad_string("Rate ", 10),
                 pad_string("field", 10),
                 pad_string("values", 10),
                 pad_string("scaling", 10),
                 pad_string("units", 10)))
      msgs = c(msgs, paste(replicate(65, "-"), collapse = ""))
      for(rate    in cfg$options$inputs$infusion_rate_names){
        msgs =c(msgs, sprintf("%s | %s | %s | %s | %s",
                   pad_string(rate, 10),
                   pad_string('time', 10),
                   pad_string(paste(cfg$options$inputs$infusion_rates[[rate]]$times$values, collapse=" "), 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$times$scale, 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$times$units, 10)))
        msgs =c(msgs, sprintf("%s | %s | %s | %s | %s",
                   pad_string('', 10),
                   pad_string('levels', 10),
                   pad_string(paste(cfg$options$inputs$infusion_rates[[rate]]$levels$values, collapse=" "), 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$levels$scale, 10),
                   pad_string(      cfg$options$inputs$infusion_rates[[rate]]$levels$units, 10)))
      }
      msgs = c(msgs, paste(replicate(65, "-"), collapse = ""))
      msgs = c(msgs, " ")
    } else {
      msgs =c(msgs, "No infusion rate information found") }
  }
  
  # Processing covariate information
  if(field == "all" | field== "covariate"){
    if("covariates" %in% names(cfg$options$inputs))  {
      msgs = c(msgs, sprintf(" Covariate details"))
      msgs = c(msgs, sprintf("%s | %s | %s | %s",
                     pad_string(" Covariate", 10),
                     pad_string("field", 10),
                     pad_string("values", 10),
                     pad_string("units", 10)))
      msgs = c(msgs, paste(replicate(50, "-"), collapse = ""))
        for(covariate in names(cfg$options$inputs$covariates)){
          msgs = c(msgs, sprintf("%s | %s | %s | %s",
                     pad_string(covariate, 10),
                     pad_string('time', 10),
                     pad_string(paste(cfg$options$inputs$covariates[[covariate]]$times$values, collapse=" "), 10),
                     pad_string(      cfg$options$inputs$covariates[[covariate]]$times$units, 10)))
          msgs = c(msgs, sprintf("%s | %s | %s | %s",
                     pad_string(sprintf('(%s)', cfg$options$inputs$covariates[[covariate]]$cv_interp), 10),
                     pad_string('levels', 10),
                     pad_string(paste(cfg$options$inputs$covariates[[covariate]]$values$values, collapse=" "), 10),
                     pad_string(      cfg$options$inputs$covariates[[covariate]]$values$units,  10)))
        }
        msgs = c(msgs, paste(replicate(50, "-"), collapse = ""), " ")
    } else {
      msgs = c(msgs, "No covariate information found", " ")}
  }
  
  # Processing iiv information    
  if(field == "all" | field== "iiv"){
    if("iiv" %in% names(cfg))  {
      msgs = c(msgs, sprintf(" IIV details"))
      msgs = c(msgs, sprintf(" IIV/Parameter set:"))
      msgs = c(msgs, sprintf("   Short Name:  %s ", cfg$iiv$current_set))
      msgs = c(msgs, sprintf(" Variance/covariance matrix"))
      iivs = names(cfg$iiv$iivs)
      # creating the headers
      msgs = c(msgs, " ")
      row_str =  pad_string(" ", 18)
      for(colidx in 1:length(iivs)){
        row_str = sprintf("%s%s", row_str, pad_string(iivs[colidx], 18))}
        msgs = c(msgs, row_str)
      for(rowidx in 1:length(iivs)){
        row_str = sprintf("%s",  pad_string(iivs[rowidx], 18))
        for(colidx in 1:length(iivs)){
          row_str = sprintf("%s%s", row_str, var2string( cfg$iiv$values[rowidx,colidx], 18))
        }
        msgs = c(msgs, row_str)
      }
      msgs = c(msgs, " " )
        
      msgs = c(msgs, sprintf(" On parameters"))
      for(pname in names(cfg$iiv$parameters)){
         msgs = c(msgs, sprintf(" %s, %s(%s)",
                       pad_string(pname,10),
                       pad_string(cfg$iiv$parameters[[pname]]$iiv_name,10),
                       cfg$iiv$parameters[[pname]]$distribution, 10)  )
      }
      
    } else {
      msgs = c(msgs, "No IIV information found") }
  }

  #
  # Simulation Options
  #
  if(field == "all" | field== "simulation"){
     msgs = c(msgs, sprintf(" ", "Simulation details"))
     if('integrate_with' %in% names(cfg$options$simulation_options)){
       msgs = c(msgs, sprintf(" integrate_with          %s", cfg$options$simulation_options$integrate_with))
     }
     if('output_times' %in% names(cfg$options$simulation_options)){
       msgs = c(msgs, sprintf(" output_times            %s ", var2string_gen(cfg$options$simulation_options$output_times)))
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
        msgs = c(msgs, " ", " Dataset details")
        for(ds_name   in names(cfg$data)){
          msgs = c(msgs, paste(replicate(20, "-"), collapse = ""))
          msgs = c(msgs, sprintf(" Name:      %s", ds_name))
          msgs = c(msgs, sprintf(" Data File: %s", cfg$data[[ds_name]]$data_file$name))
          if("sheet" %in% names(cfg$data[[ds_name]]$data_file)){
            msgs = c(msgs, sprintf(" Sheet:     %s", cfg$data[[ds_name]]$data_file$sheet))
          }
          msgs = c(msgs, sprintf(" Columns:   %s", paste(colnames(cfg$data[[ds_name]]$values), collapse=", ")))
          msgs = c(msgs, sprintf(" Rows:      %d", nrow(cfg$data[[ds_name]]$values)))
        }
      } else {
       msgs = c(msgs, " No datasets loaded") }
  }


  #
  # Estimation Options
  #
  if(field == "all" | field== "estimation"){
     msgs = c(msgs, " ")
     msgs = c(msgs,         "Estimation details ")
     msgs = c(msgs, sprintf(" Parameter set:          %s",  cfg[["parameters"]][["current_set"]]))
     msgs = c(msgs, sprintf(" Parameters estimated:   %s",  toString(names(cfg[["estimation"]][["mi"]]))))
     msgs = c(msgs, sprintf(" objective_type          %s",  cfg[["estimation"]][["objective_type"]]))
     msgs = c(msgs, sprintf(" observation_function    %s",  cfg[["estimation"]][["options"]][["observation_function"]]))
  }


  #
  # Dataset information
  #
  if(field == "all" | field== "cohorts"){
     if("cohorts" %in% names(cfg))  {
       msgs = c(msgs, " ")
       msgs = c(msgs," Cohort details")
       for(ch_name   in names(cfg$cohorts)){
         msgs = c(msgs,sprintf(" Cohort: %s", ch_name))
         msgs = c(msgs, paste(replicate(20, "-"), collapse = ""))
         msgs = c(msgs,sprintf(" dataset: %s; (%s)", cfg$cohorts[[ch_name]]$dataset, cfg$data[[cfg$cohorts[[ch_name]]$dataset]]$data_file$name))

         # output times
         if("output_times" %in% names(cfg[["cohorts"]][[ch_name]])){
           msgs = c(msgs,sprintf(" Cohort-specific output times (output_times) "))
           msgs = c(msgs, sprintf("     output_times = %s", var2string_gen(cfg[["cohorts"]][[ch_name]][["output_times"]])))
           msgs = c(msgs, "")
         }

         msgs = c(msgs,sprintf(" Cohort options (options) "))

         #options
         if('options' %in% names(cfg$cohorts[[ch_name]])){
           for(opname in names(cfg$cohorts[[ch_name]]$options)){
             msgs = c(msgs, sprintf("     %s = c(%s)", opname, toString(cfg$cohorts[[ch_name]]$cf[[opname]])))
           }
         } else{
           msgs = c(msgs, "     none")
         }
         msgs = c(msgs, " ")

         #filter 
         msgs = c(msgs, " Cohort filter (cf)")
         if('cf' %in% names(cfg$cohorts[[ch_name]])){
           for(col_name in names(cfg$cohorts[[ch_name]]$cf)){
             msgs = c(msgs, sprintf("     %s = c(%s)", col_name, toString(cfg$cohorts[[ch_name]]$cf[[col_name]])))
           }
         } else{
           msgs = c(msgs, "     none")
         }
         msgs = c(msgs, " ")

         msgs = c(msgs, " Cohort-specific parameters (cp)")
         if('cp' %in% names(cfg$cohorts[[ch_name]])){
           for(pname in names(cfg$cohorts[[ch_name]]$cp)){
             msgs = c(msgs, sprintf("     %s = %s", pname, toString(cfg$cohorts[[ch_name]]$cp[[pname]])))
           }
         } else{
           msgs = c(msgs, "     none")
         }

         msgs = c(msgs, " ")
         msgs = c(msgs, " Outputs")
         for(oname in names(cfg$cohorts[[ch_name]]$outputs)){

           msgs = c(msgs, sprintf("   >%s<              ", oname))
           msgs = c(msgs, sprintf("    Dataset:         "))
           msgs = c(msgs, sprintf("     Sample Time  %s ", cfg$cohorts[[ch_name]]$outputs[[oname]]$obs$time))
           msgs = c(msgs, sprintf("     Observation  %s ", cfg$cohorts[[ch_name]]$outputs[[oname]]$obs$value))
           if('missing' %in% names(cfg$cohorts[[ch_name]]$outputs[[oname]]$obs)){
               msgs = c(msgs, sprintf("     Missing      %s ", toString(cfg$cohorts[[ch_name]]$outputs[[oname]]$obs$missing)))
           }

           msgs = c(msgs, " ")

           msgs = c(msgs, sprintf("    Model:           "))
           msgs = c(msgs, sprintf("     Timescale    %s ", cfg$cohorts[[ch_name]]$outputs[[oname]]$model$time))
           msgs = c(msgs, sprintf("     Output       %s ", cfg$cohorts[[ch_name]]$outputs[[oname]]$model$value))
           msgs = c(msgs, sprintf("     Variance     %s ", cfg$cohorts[[ch_name]]$outputs[[oname]]$model$variance))
           msgs = c(msgs, sprintf("    ---              "))

         }
         msgs = c(msgs, " ")

       }
     } else {
       msgs = c(msgs, " No cohort information found") }
  }

  #
  # NCA 
  #
  # Processing infusion rate information
  if(field == "all" | field== "nca"){
    if("nca" %in% names(cfg)){
      for(analysis_name in  names(cfg[["nca"]])){
        nca_tmp = cfg[["nca"]][[analysis_name]]
        NCA_cols = system_fetch_nca_columns(cfg, analysis_name = analysis_name)
        msgs = c(msgs, " ")
        msgs = c(msgs, "NCA Details")
        msgs = c(msgs, paste("  Analysis:                       ", analysis_name))
        msgs = c(msgs, paste("   Options:                       "))
        msgs = c(msgs, paste("      Dose to conc scale          ", nca_tmp[["ana_opts"]][["dscale"]]))
        msgs = c(msgs, paste("      Min NCA points              ", nca_tmp[["ana_opts"]][["NCA_min"]]))
        msgs = c(msgs, paste("      Extrapolate C0              ", nca_tmp[["ana_opts"]][["extrap_C0"]]))
        msgs = c(msgs, paste("      Number of extrap points     ", nca_tmp[["ana_opts"]][["extrap_N"]]))
        msgs = c(msgs, paste("      Sparse                      ", nca_tmp[["ana_opts"]][["sparse"]]))
        msgs = c(msgs, paste("   Dataset (", nca_tmp[["ana_opts"]][["dsname"]], ")"))
        msgs = c(msgs, paste("      NCA Field-->Column in dataset"))
        msgs = c(msgs, paste("      -----------------------------"))
        for(dsfield in names(nca_tmp[["ana_opts"]][["dsmap"]])){
          msgs = c(msgs, paste("      ", dsfield, "-->", nca_tmp[["ana_opts"]][["dsmap"]][[dsfield]], sep=""))
        }
        msgs = c(msgs, paste("   The analysis contains the following columns"))
        msgs = c(msgs, "")
        len_NCA_col     = NCA_cols$len_NCA_col      
        len_label       = NCA_cols$len_label       
        len_from        = NCA_cols$len_from        
        len_description = 40
        nca_res_header  = paste(pad_string("column name", location="end", maxlength=(len_NCA_col        + 2)), "|",
                                pad_string("from",        location="end", maxlength=(len_from           + 2)), "|",
                                pad_string("label",       location="end", maxlength=(len_label          + 2)), "|",
                                pad_string("description", location="end", maxlength=(len_description    + 2))     ,sep="")
     
        row_sep = paste(rep("-", nchar(nca_res_header)), collapse="")

        msgs = c(msgs, paste("     ", row_sep, sep=""))
        msgs = c(msgs, paste("     ", nca_res_header, sep=""))
        msgs = c(msgs, paste("     ", row_sep, sep=""))

        for(ridx in 1:nrow(NCA_cols[["NCA_col_summary"]])){
           col_name       = as.character(NCA_cols[["NCA_col_summary"]][ridx,][["col_name"]])
           from           = as.character(NCA_cols[["NCA_col_summary"]][ridx,][["from"]])
           label          = as.character(NCA_cols[["NCA_col_summary"]][ridx,][["label"]])
           description    = as.character(NCA_cols[["NCA_col_summary"]][ridx,][["description"]])

           nca_res_row    = paste(pad_string(col_name,    location="end", maxlength=(len_NCA_col        + 2)), "|",
                                  pad_string(from,        location="end", maxlength=(len_from           + 2)), "|",
                                  pad_string(label,       location="end", maxlength=(len_label          + 2)), "|",
                                  pad_string(description, location="end", maxlength=(len_description    + 2))     ,sep="")
           msgs = c(msgs, paste("     ", nca_res_row, sep=""))
        }
        msgs = c(msgs, paste("     ", row_sep, sep=""))
      }
    } else {
      msgs = c(msgs, "No NCA has been performed") 
    }

  }


  
  # Processing infusion rate information
  if(field == "all" | field== "XXX"){
   #  if("infusion_rates" %in% names(cfg$options$inputs))  {
   #  } else {
   #  }
  }

  # This will print the current results to the screen if 
  # verbose has been selected
  if(verbose){
     vp(cfg, msgs) }
  
return(msgs)}
# /system_view
#-----------------------------------------------------------

#'@export
#'@title Convert R Objects to Strings
#'@description Mechanism for converting R objects strings for reporting. 
#'@keywords internal
#'
#'@param var R variable
#'
#'@return Variable in string form
#'
#'@examples
#'var2string_gen(c(1,2,3))
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
#'@title Converts Numeric Variables into Padded Strings
#'@description Mechanism for converting numeric variables into strings for reporting. 
#'
#'@param vars numeric variable or a vector of numeric variables
#'@param maxlength if this value is greater than zero spaces will be added to the beginning of the string until the total length is equal to maxlength
#'@param nsig_e number of significant figures for scientific notation
#'@param nsig_f number of significant figures for numbers (2.123)
#'
#'@return Number as a string padded
#'
#'@examples
#'var2string(pi, nsig_f=20)
#'var2string(.0001121, nsig_e=2, maxlength=10)
var2string <- function(vars,maxlength=0, nsig_e = 3, nsig_f = 4) {
#  str = var2string(var, 12) 
#  converts the numerical value 'var' to a padded string 12 characters wide

strs = c()

for(var in vars){
  if(is.character(var)){
    str = var
  } else if(is.na(var)){
    str = "NA"
  } else if(is.nan(var)){
    str = "NaN"
  } else if(var == 0){
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

  strs = c(strs, str)
}



return(strs)}


#'@export
#'@title Pad String with Spaces
#'@description Adds spaces to the beginning or end of strings until it reaches the maxlength. Used for aligning text.
#'
#'@param str string
#'@param maxlength length to pad to
#'@param location either \code{"beginning"} to pad the left or \code{"end"} to pad the right
#'
#'@return Padded string
#'@examples
#'pad_string("bob", maxlength=10)
#'pad_string("bob", maxlength=10, location="end")
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
#'@title Run Population Simulations 
#'@description  Used to run Population/Monte Carlo simulations with subjects
#' generated from either provided variance/covariance information or a dataset. 
#' 
#'@param parameters list containing the typical value of parameters
#'@param cfg ubiquity system object    
#'@param progress_message text string to prepend when called from the ShinyApp
#'@param show_progress Boolean value controlling the display of a progress indicator (\code{TRUE})
#'
#'@return Mapped simulation output with individual predictions, individual
#' parameters, and summary statistics of the parameters. The Vignettes below
#' details on the format of the output. 
#'
#'@details 
#'
#' Failures due to numerical instability or other integration errors will be
#' captured within the function. Data for those subjects will be removed from the
#' output. Their IDs will be displayed as messages and stored in the output. 
#'
#'
#' For more information on setting options for population simulation see the
#' stochastic section of the \code{\link{system_set_option}} help file.
#'
#'
#'@seealso Vignette on simulation (\code{vignette("Simulation", package = "ubiquity")}) titration (\code{vignette("Titration", package = "ubiquity")}) as well as \code{\link{som_to_df}}
simulate_subjects = function (parameters, cfg, show_progress = TRUE, progress_message = "Simulating Subjects:"){
#function [predictions] = simulate_subjects(parameters, cfg)
#
# Inputs:
#
# cfg - System configuration variable generated in the following manner:
#
# cfg = build_system()
# cfg = system_select_set(cfg, 'default')
#
# parameters - list of typical parameter values. This can be obtained from
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
#      name of the data structure loaded with system_load_data
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


# List to hold the outputs
p = list(subjects = list(parameters           = NULL,
                         secondary_parameters = NULL),
         tcsummary = NULL,
         states    = NULL,
         outputs   = NULL,
         times     = NULL)

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
ssp_names    = names(cfg$options$ssp)

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

  if("ssp" %in% names(cfg$options$stochastic)){
    ssp_names = cfg$options$stochastic$ssp
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

  # Defining the columns to keep from the simulation
  ts_names     = names(cfg$options$time_scales) 
  ts_names     = ts_names[ts_names != "time"] 
  state_names  = unlist(state_names)
  output_names = unlist(output_names)
  ssp_names    = unlist(ssp_names)


  col_keep = c("time",
               state_names,
               output_names,
               ssp_names, 
               paste("ts.", ts_names, sep=""))

}



isgood = TRUE;

if("iiv" %in% names(cfg) | !is.null(sub_file)){

  # If the subjects file is null we check the IIV matrix
  if(is.null(sub_file)){
    # otherwise we check the IIV
    if(min((eigen((cfg$iiv$values + (cfg$iiv$values))/2))$values) <= 0){
      vp(cfg, "simulate_subjects()")
      vp(cfg, "Warning: The variance/covariance matrix is not   ")
      vp(cfg, "positive semi-definite. Testing only the diagonal")
      vp(cfg, "elements. I.e. no covariance/interaction terms   ")
    
      cfg$iiv$values = diag(diag(cfg$iiv$values))
      if(min((eigen((cfg$iiv$values + (cfg$iiv$values))/2))$values) <= 0){
        vp(cfg, "Failed using only diagonal/variance elements.")
        vp(cfg, "Check the specified IIV elements in")
        vp(cfg, "cfg$iiv$values")
        isgood = FALSE 
      } else {
        vp(cfg, "Using only the diagional elements seems to   ")
        vp(cfg, "have worked. Understand that the results do  ")
        vp(cfg, "not include any interaction.                 ")
      }
      vp(cfg, " ")
    }
  }
  else{
  
      # Summarizing information about the data file
      sub_file_dataset        = cfg[["data"]][[sub_file]][["values"]]
      sub_file_nrow           = nrow(sub_file_dataset)
      sub_file_nsub           = length(unique(sub_file_dataset[[sub_file_ID_col]]))
      sub_file_file_name      = cfg[["data"]][[sub_file]][["data_file"]][["name"]]

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

     # Checking to make sure that the required rows exist:
     if(!(sub_file_ID_col %in% names(sub_file_dataset))){
       vp(cfg, paste("Error: The required column >", sub_file_ID_col, "< specified dataset >", sub_file, "< is missing", sep="")) 
       vp(cfg, "This column assigns the subject ID to the row.")
       isgood = FALSE
     }
     if(!(sub_file_TIME_col %in% names(sub_file_dataset))){
       vp(cfg, paste("Error: The required column >", sub_file_TIME_col, "< specified dataset >", sub_file, "< is missing", sep="")) 
       vp(cfg, "This column associates the system time with the record and should have the same units as the system time.")
       isgood = FALSE
     }


     # Checking to make sure there is at least one subject:
     if(!(sub_file_nrow >0)){
       vp(cfg, paste("Error: The specified dataset:", sub_file, "contains no data", sep="")) 
       isgood = FALSE
     } else {
       if(isgood){
         if((nsub > sub_file_nsub & sub_file_sample == "without replacement")){
            vp(cfg, " ")
            vp(cfg, "simulate_subjects()")
            vp(cfg, sprintf("Warning: The number of subjects requested (%d) is greater than", nsub))
            vp(cfg, sprintf("the number in the subjects dataset (%d) so it is not", sub_file_nsub))
            vp(cfg, sprintf("possible to sample without replacement. Changing sampling"))
            vp(cfg, sprintf("method to 'with replacement'"))
            vp(cfg, " ")
            sub_file_sample = "with replacement"
         }
       }
     }
  }
  
  # Set the random seed
  set.seed(seed)

  if(isgood){
      vp(cfg, sprintf("Simulating multiple subjects (%d)", nsub), fmt="h1")
      vp(cfg, sprintf("Integrating with:            %s ",  cfg$options$simulation_options$integrate_with))
      vp(cfg, sprintf("Parallel set to:             %s ",  cfg$options$simulation_options$parallel))
      vp(cfg, sprintf("Number of cores:             %d ",  cfg$options$simulation_options$compute_cores))
      if(!is.null(sub_file)){                            
      vp(cfg, sprintf("Subjects source:             %s ", sub_file_file_name))
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
      if(show_progress){
        if(cfg$options$misc$operating_environment == 'script'){
          #pb = txtProgressBar(min=0, max=1, width=12, style=3, char='.') 
          # JMH for parallel
          cli::cli_progress_bar(total=100) 
          }
      }
    
      if(cfg$options$misc$operating_environment == 'gui'){
        pb <- shiny::Progress$new()
        # JMH how to parallelize 
        pb$set(message = progress_message, value = 0)
      }
    
      foreach_packages = c("deSolve", "dplyr")
    
      if(cfg$options$misc$distribution == "package"){
        foreach_packages = c(foreach_packages, "ubiquity")
      }
    
      if("multicore" == cfg$options$simulation_options$parallel){
        #
        # Running simulations in parallel
        #
    
        # Setting up and starting the cluter
        # for doSnow
        # cl <- makeSOCKcluster(cfg$options$simulation_options$compute_cores)
        # registerDoSNOW(cl)
        # snow_opts = list(progress = myprogress)
        # for doParallel
        cl <- makeCluster(cfg$options$simulation_options$compute_cores)
        doParallel::registerDoParallel(cl)
        snow_opts = list()
        
        somall <- foreach(sub_idx=1:nsub,
                          .verbose = FALSE,
                          .errorhandling='pass',
                       #  .options.snow=list(progress = myprogress),
                          .packages=foreach_packages) %dopar% {


          # Setting the seed based on the subject ID and the 
          # user specified seed: this applies to subject level 
          set.seed(cfg$options$stochastic$seed + sub_idx)
    
          # If we're using the c-file we tell the spawned instances to load
          # the library 
          if(cfg$options$simulation_options$integrate_with == "c-file"){
            dyn.load(file.path(cfg$options$misc$temp_directory, paste( cfg$options$misc$c_libfile_base, .Platform$dynlib.ext, sep = "")))
          }
    
          # If we're running a stand alone distribution we load the functions
          if(cfg$options$misc$distribution == "stand alone"){
            source(file.path("library","r_general","ubiquity.R"))}
    
          # now we load the system specific functions
          source(file.path(cfg$options$misc$temp_directory, "auto_rcomponents.R"))
        
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
            tcres = tryCatch(
              { 
               exec.time = system.time((som = run_simulation_titrate(parameters_subject, cfg_sub)))
               list(exec.time = exec.time, som=som, msg="success")},
             error = function(e) {
               list(exec.time = NULL, som=NULL, error=NULL, msg="error")})
            }
          else{
            tcres = tryCatch(
              { 
               exec.time = system.time((som = run_simulation_ubiquity(parameters_subject, cfg_sub)))
               list(exec.time = exec.time, som=som, msg="success")},
             error = function(e) {
               list(exec.time = NULL, som=NULL, error=e, msg="error")})
            }


          som       = tcres$som
          msg       = tcres$msg

          #tmp = run_simulation_ubiquity(parameters_subject, cfg_sub)

          if(msg== "error"){
          # Checking for integration failure
            som$simout      = NULL
            som$skip_reason = "Integration failure"
          } else if(any(is.nan(as.matrix((som$simout))))){
          # checking to see if any of the results returned NAN
            som$simout      = NULL
            som$skip_reason = "NAN values in simulation output"
          }
        
          # Storing the subject id
          som$sub_idx   = sub_idx
        
          # saving the execution time
          som$exec.time = tcres$exec.time

          # storing the result of trycatch error
          som$error     = tcres$error
        
          # if(cfg$options$misc$operating_environment == 'gui'){
          #   pb$inc(1/nsub, detail = sprintf('%d/%d (%d %%)', sub_idx, nsub, floor(100*sub_idx/nsub))) }
          # Only keep the simout columns the user wants 
          if(!is.null(som$simout)){
            som$simout  = dplyr::select(som$simout , all_of(col_keep))
          }
        
          som }
    
        #
        # Stopping the cluster
        #
        stopCluster(cl)
    
      }
      else{
        system_req("foreach")
        #
        # Running simulations sequentially 
        #
        somall <- foreach(sub_idx=1:nsub) %do% {
        
          # Setting the seed based on the subject ID and the 
          # user specified seed: this applies to subject level 
          set.seed(cfg$options$stochastic$seed + sub_idx)
        
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
            tcres = tryCatch(
              { 
               exec.time = system.time((som = run_simulation_titrate(parameters_subject, cfg_sub)))
               list(exec.time = exec.time, som=som, msg="success")},
             error = function(e) {
               list(exec.time = NULL, error=NULL, som=NULL, msg="error")})
            }
          else{
            tcres = tryCatch(
              { 
               exec.time = system.time((som = run_simulation_ubiquity(parameters_subject, cfg_sub)))
               list(exec.time = exec.time, som=som, msg="success")},
             error = function(e) {
               list(exec.time = NULL, error=e, som=NULL, msg="error")})
            }
          som       = tcres$som
          msg       = tcres$msg
        
          if(msg== "error"){
          # Checking for integration failure
            som$simout      = NULL
            som$skip_reason = "Integration failure"
          } else if(any(is.nan(as.matrix((som$simout))))){
          # checking to see if any of the results returned NAN
            som$simout      = NULL
            som$skip_reason = "NAN values in simulation output"
          }

          # Storing the subject id
          som$sub_idx = sub_idx
        
          # saving the execution time
          som$exec.time = tcres$exec.time

          # storing the result of trycatch error
          som$error  = tcres$error
        
          # Updating progress indicators
          if(show_progress){
            if(cfg$options$misc$operating_environment == 'script'){
              cli::cli_progress_update(set=sub_idx/nsub*100)
            }
           }
        
          if(show_progress){
            if(cfg$options$misc$operating_environment == 'gui'){
              pb$inc(1/nsub, detail = sprintf('%d/%d (%d %%)', sub_idx, nsub, floor(100*sub_idx/nsub))) }
          }

          # Only keep the simout columns the user wants 
          if(!is.null(som$simout)){
            som$simout  = dplyr::select(som$simout , all_of(col_keep))
          }
        
          som }
      }


      # Pulling out the lengths of different things
      ntimes = length(somall[[1]]$simout$time)
      npsec  = length(ssp_names)
    
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
      p[["subjects"]][["secondary_parameters"]]  = NULL
      if(npsec > 0){
        p[["subjects"]][["secondary_parameters"]]  = as.data.frame(matrix(0, ncol = npsec, nrow=nsub))
        
        # putting the column names
        colnames( p[["subjects"]][["secondary_parameters"]]) = ssp_names
      }
    
      # And storing the output times/timescales
      p$times    = som$simout["time"]
      # creating the time patch vectors for the different timescales
      for(timescale_name   in names(cfg$options$time_scales)){
       timescale_name = sprintf('ts.%s', timescale_name)
       p$times[[timescale_name]] = c(som$simout[[timescale_name]])
      }

      subs_skipped = NULL
    
      for(som in somall){
        sub_idx = som$sub_idx
      
        # If som$simout is null it needs to be skipped 
        # so we capture that information here:
        if(is.null(som$simout)){
         # Storing the id of the subject being skipped
         subs_skipped = rbind(subs_skipped, 
                   data.frame(id     = sub_idx,
                              reason = som$skip_reason))
        } else {
          # storing the secondary parameters
          if(npsec > 0){
            p$subjects$secondary_parameters[sub_idx,] = som$simout[1,ssp_names]
          }
         
          # Storing the states, outputs and titration information
          for(state_name   in state_names){
            p$states[[state_name]][sub_idx,] = som$simout[[state_name]] }
          
          for(output_name   in output_names){
            p$outputs[[output_name]][sub_idx,] = som$simout[[output_name]] }
         
          for(titration_name   in names(som$titration)){
            p$titration[[titration_name]][sub_idx,] = som$titration[[titration_name]]}
        }
        sub_idx = sub_idx + 1;
      }


      #------------------------------------
      # Processing skipped subjects
      if(!is.null(subs_skipped)){
        # Saving the parameter combinations that caused the problems
        subs_skipped$parmaeters = p$subjects$parameters[as.numeric(subs_skipped$id),]

        # Removing the rows associated with skipped subjects from the
        # Parameters
        p$subjects$parameters = p$subjects$parameters[-as.numeric(subs_skipped$id), ]
        # Secondary parameters
        p$subjects$secondary_parameters = p$subjects$secondary_parameters[-as.numeric(subs_skipped$id), ]
        # States
        for(state_name   in state_names){
          p$states[[state_name]][-as.numeric(subs_skipped$id),]
        }
        # Outputs
        for(output_name   in output_names){
          p$outputs[[output_name]][-as.numeric(subs_skipped$id),]
        }
        # Titration names
        for(titration_name   in  names(som$titration)){
          p$titrations[[titration_name]][-as.numeric(subs_skipped$id),]
        }

        vp(cfg, "The following subjects were skipped")
        for(sub_idx in subs_skipped$id){
          vp(cfg, paste(" ", sub_idx, subs_skipped[subs_skipped$id == sub_idx, ]$reason))
        }
        vp(cfg, paste("The results will only include", nrow(p$subjects$parameters), "subjects"))
      }
      p$subs_skipped = subs_skipped
      #------------------------------------
    
      # Cleaning up the progress bar objects
      if(show_progress){
        if(cfg$options$misc$operating_environment == 'script'){
          cli::cli_progress_done()
        }
      }
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
        mymat = p$states[[state_name]]
        tc = timecourse_stats(mymat,ci)
        eval(parse(text=sprintf('p$tcsummary[["s.%s.lb_ci"]]   = tc$stats$lb_ci',   state_name))) 
        eval(parse(text=sprintf('p$tcsummary[["s.%s.ub_ci"]]   = tc$stats$ub_ci',   state_name))) 
        eval(parse(text=sprintf('p$tcsummary[["s.%s.mean"]]    = tc$stats$mean',    state_name))) 
        eval(parse(text=sprintf('p$tcsummary[["s.%s.median"]]  = tc$stats$median',  state_name))) 
        }
      for(output_name   in names(p$outputs)){
        mymat = p$outputs[[output_name]]
        tc = timecourse_stats(mymat,ci)
        eval(parse(text=sprintf('p$tcsummary[["o.%s.lb_ci"]]   = tc$stats$lb_ci',   output_name))) 
        eval(parse(text=sprintf('p$tcsummary[["o.%s.ub_ci"]]   = tc$stats$ub_ci',   output_name))) 
        eval(parse(text=sprintf('p$tcsummary[["o.%s.mean"]]    = tc$stats$mean',    output_name))) 
        eval(parse(text=sprintf('p$tcsummary[["o.%s.median"]]  = tc$stats$median',  output_name))) 
        }
    }
  }

} else {
  vp(cfg, "Error:Trying to simulate subjects with       ")
  vp(cfg, "   variability, but no variance/covariance   ")
  vp(cfg, "   information or dataset containing         ")
  vp(cfg, "   population information was specified.     ")
  vp(cfg, "                                             ")
  vp(cfg, "                                             ")
  vp(cfg, "   Modify the system.txt file to add the     ")
  vp(cfg, "   IIV information using the following:      ")
  vp(cfg, "    <IIV:?>      ?                           ")
  vp(cfg, "    <IIV:?:?>    ?                           ")
  vp(cfg, "    <IIVCOR:?:?> ?                           ")
  vp(cfg, "                                             ")
  vp(cfg, "   Or load a dataset with subject parameters ")
  vp(cfg, "   and covariates and specify this in the    ")
  vp(cfg, "   stochastic options.                       ")
  isgood = FALSE
}

if(!isgood){
  vp(cfg, "simulate_subjects()")
}
cli::cli_rule()

return(p)
}


#'@export
#'@title Calculate Timecourse Statistics for a Matrix of Responses
#'@keywords internal
#'@description 
#'  Given a matrix (d) of time courses (each row is an individual and each column is
#'  a time point) and a confidence interval (ci) this will calculate the mean,
#'  median, confidence intervals and a vector of values for creating patches.
#'
#'@param d matrix of responses (each row an individual and each column a time point)
#'@param ci confidence interval in percent (eg, 95)
#'
#'@return List with the following elements:
#'
#' \itemize{
#'   \item \code{stats$ub_ci}  vector of confidence interval upper bound 
#'   \item \code{stats$lb_ci}  vector of confidence interval lower bound 
#'   \item \code{stats$mean}   vector of mean values
#'   \item \code{stats$median} vector of median values
#'   }
timecourse_stats = function (d, ci){

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
#'@title Extracts Covariates for a Subject from a Subject Data File
#'@keywords internal
#'@description 
#' This function is used when stochastic simulations are being performed using
#' a data file for the subject level information. If the data file contains
#' covariate information, this function will update the system for each subjects
#' covariates. 
#'
#'@param tmpcfg ubiquity system object    
#'@param cov_found list of covariates found in dataset
#'@param sub_dataset name of dataset with subject parameters
#'@param sub_ID_col name of column in dataset with subject IDs 
#'@param sub_TIME_col name of column in dataset with simulation time
#'@param file_ID subject ID to extract covariates for
#'
#'@return ubiquity system object with the covariates set to those for the current subject
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
#'@title Generate Subject
#'@keywords internal
#'@description 
#' Generates subject with variability specified using the \code{<IIV:?>} descriptor
#' in the system file
#'
#'@param parameters vector of nominal parameter values
#'@param cfg ubiquity system object    
#'
#'@return List with a field named \code{parameters} containing a sample representing a subject
generate_subject = function (parameters, cfg){
# function [subject] = generate_subject(parameters, cfg)

invisible(system_req("MASS"))

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
iiv_sample = MASS::mvrnorm(n = 1, muzero, covmatrix, tol = 1e-6, empirical = FALSE, EISPACK = FALSE);

# now looping through each parameter with inter-individual variability
#names(cfg$iiv$iivs)
#names(cfg$iiv$parameters)
TMP_equation  = NULL
TMP_iiv_value = NULL
TMP_iiv_name  = NULL
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
#'@title Generates a Parameter Based on \code{<IIV:?>} in the System File
#'@description  Internal function used to generate parameters based on IIV information 
#'@keywords internal
#'
#'@param SIMINT_parameters parameters vector containing the typical values
#'@param SIMINT_cfg ubiquity system object    
#'@param SIMINT_PARAMETER_TV  Typical value of the parameter in question
#'@param SIMINT_IIV_VALUE sample from mvr distribution
#'@param SIMINT_equation equation relating IIV and typical value to the parameter value with variability
#'
#'@return parameter value with the variability applied
generate_parameter = function (SIMINT_parameters, SIMINT_cfg, SIMINT_PARAMETER_TV, SIMINT_IIV_VALUE, SIMINT_equation){
  # Defining the system parameters locally
  for(SIMINT_pname in names(SIMINT_cfg$options$mi$parameters)){
    eval(parse(text=paste(sprintf("%s = SIMINT_parameters[SIMINT_cfg$options$mi$parameters$%s]", SIMINT_pname, SIMINT_pname))))
  }

  # Evaluating the parameter with IIV
  return( eval(parse(text=paste(SIMINT_equation))))
}


#'@export
#'@title Initialize System Log File
#'@description Initializes the currently specified system log file.
#'@param cfg ubiquity system object    
#'
#'@return ubiquity system object with logging enabled
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Initialzing the log file
#' cfg = system_log_init(cfg)
#'}
system_log_init = function (cfg){
# initializes the log file then enables logging

  file.create(cfg$options$logging$file)
  cfg$options$logging$enabled = TRUE
  system_log_entry(cfg, 'Ubiquity log init - R')

return(cfg)
}

#-------------------------------------------------------------------------
#'@export
#'@title Save variables to files     
#'@description Triggered when debugging is enabled, this function will save
#' the contents of values to the specified file name in the ubiquity temporary
#' directory.
#'@param cfg ubiquity system object    
#'@param file_name name of the save file without the ".RData" extension
#'@param values named list of variables to save
#'
#'@return Boolean variable indicating success 
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#'
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # enable debugging:
#' cfg=system_set_option(cfg,group = "logging", 
#'                          option = "debug", 
#'                          value  = TRUE)
#'
#' # Saving the cfg variable 
#' system_log_debug_save(cfg, 
#'    file_name = 'my_file',
#'    values = list(cfg=cfg))
#'
#'}
system_log_debug_save = function (cfg, file_name = "my_file", values = NULL){

   isgood = TRUE

   if(cfg$options$logging$debug){
     if(is.null(values)){
       isgood = FALSE
       vp(cfg, "ubiquity::system_log_debug_save()")
       vp(cfg, "values set to NULL")
     } else if(!is.null(values)){
       # file name to hold the debugging information
       fn = file=file.path(cfg$options$misc$temp_directory, paste(file_name, ".RData", sep=""))
       system_log_entry(cfg, paste("Debugging file:", fn))
       save(values, file=fn)
     }
   }

isgood}
#-------------------------------------------------------------------------
#'@export
#'@title Add Log Entry
#'@description Appends a specified line to the analysis log
#'@keywords internal
#'
#'@param cfg ubiquity system object    
#'@param entry string containing the log entry
#'
#'@return Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE})
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Initialzing the log file
#' system_log_entry(cfg, "Text of log entry")
#'}
system_log_entry = function(cfg, entry){

isgood = FALSE

# if logging is disabled we don't do anything 
if(cfg$options$logging$enabled ==  TRUE){
  # If the log file doesn't exist we initialize it
  if(!file.exists(cfg$options$logging$file)){
   system_log_init(cfg);
  }
  # If the timestamp is enabled we prepend it to the
  # log message
  if(cfg$options$logging$timestamp == TRUE){
    entry = sprintf('%s %s',  format(Sys.time(), format=cfg$options$logging$ts_str), entry)
  }

  # Now we dump it to the log file:
  isgood = write(entry, file=cfg$options$logging$file, append=TRUE)
  }
isgood}

#'@export
#'@title Print and Log Messages
#'@description  Used to print messages to the screen and the log file.
#'
#'@param cfg ubiquity system object    
#'@param str sequence of strings to print
#'@param fmt string format should be one of the following: \code{"h1"},
#'\code{"h2"}, \code{"h3"}, \code{"verbatim"}, \code{"alert"} (default), \code{"warning"},
#'\code{"danger"}. 
#'
#'@return Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE})
#'
#'@examples
#' \donttest{
#' # Creating a system file from the mab_pk example
#' fr = system_new(file_name        = "system.txt", 
#'                 system_file      = "mab_pk", 
#'                 overwrite        = TRUE, 
#'                 output_directory = tempdir())
#' 
#' # Building the system 
#' cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
#'       output_directory          = file.path(tempdir(), "output"),
#'       temporary_directory       = tempdir())
#'
#' # Initialzing the log file
#' vp(cfg, "Message that will be logged")
#'}
vp <- function(cfg, str, fmt="alert"){
# logging string 
system_log_entry(cfg, str)

isgood = FALSE

# printing if verbose is enabled
if('options' %in% names(cfg)){
if('verbose' %in% names(cfg$options$logging)){
if(TRUE == cfg$options$logging$verbose){
  for(line in str){
    if(fmt == "alert"){
      cli::cli_alert(line) }
    if(fmt == "h1"){
      cli::cli_h1(line) }
    if(fmt == "h2"){
      cli::cli_h2(line) }
    if(fmt == "h3"){
      cli::cli_h3(line) }
    if(fmt == "danger"){
      cli::cli_alert_danger(line) }
    if(fmt == "warning"){
      cli::cli_alert_warning(line) }
    if(fmt == "verbatim"){
      cli::cli_verbatim(line) }
  }
  isgood = TRUE
  }}}
isgood}

#'@export
#'@keywords internal
#'@title Wrapper for system_log_entry Used in ShinyApp
#'@description Called from the ShinyApp to add a log entry with "App"
#' prepended to the log entry 
#'
#'@param cfg ubiquity system object    
#'@param text string to print/log
#'
#'@return Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE})
GUI_log_entry <-function(cfg, text){
 isgood =   system_log_entry(cfg, sprintf("App %s", text))
isgood}

#'@export
#'@keywords internal
#'@title Select Records from NONMEM-ish Data Set
#'@description Retrieves a subset of a NONMEM-ish data set based on a list containing filtering information.
#'@keywords internal
#'
#'@param cfg ubiquity system object    
#'@param values dataframe containing the dataset with column headers
#'@param filter list with element names as headers for \code{values} with values from the same header OR'd and values across headers AND'd
#'
#'@return subset of dataset 
#'
#'@details
#' If the dataset has the headings \code{ID}, \code{DOSE} and \code{SEX}  and
#' \code{filter} has the following format:
#'
#' \preformatted{
#'filter = list()
#'filter$ID   = c(1:4)
#'filter$DOSE = c(5,10)
#'filter$SEX  = c(1)
#'}
#'
#'It would be translated into the boolean filter:
#'
#'\preformatted{
#'((ID==1) | (ID==2) | (ID==3) | (ID==4)) & ((DOSE == 5) | (DOSE==10)) & (SEX == 1)
#'}
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

#'@export
#'@keywords internal
#'@title Convert Time in Timescale to Simulation Time
#'@description 
#' converts a time specified in a defined timescale (say weeks) to the
#' timescale of the simulation (say hours if the rates are in 1/hr units)
#'
#'@param cfg ubiquity system object    
#'@param tstime numeric time of the timescale
#'@param ts string containing the timescale 
#'
#'@return \code{tstime} in the system timescale units 
system_ts_to_simtime <-function(cfg, tstime, ts){
   simtime = c()
   if(ts %in% names(cfg$options$time_scales)){
     simtime = tstime/cfg$options$time_scales[[ts]]
   }
   else{
    vp(cfg, sprintf('Unable to find timescale %s', ts)) }
    return(simtime)
}

#'@export
#'@title Clear all Cohorts
#'@description Clear previously defined cohorts
#'
#'@param cfg ubiquity system object    
#'
#'@return ubiquity system object with no cohorts defined
system_clear_cohorts  <- function(cfg){
  cfg[["cohorts"]] = c()
return(cfg)}

#'@export
#'@title Define Estimation Cohort
#'@description Define a cohort to include in a parameter estimation
#'
#'@param cfg ubiquity system object    
#'@param cohort list with cohort information 
#'
#'@return ubiquity system object with cohort defined 
#'
#'@details 
#' Each cohort has a name (eg \code{d5mpk}), and the dataset containing the
#' information for this cohort is identified (the name defined in \code{\link{system_load_data}})
#'
#' \preformatted{cohort = list(
#'   name         = "d5mpk",
#'   dataset      = "pm_data",
#'   inputs       = NULL,
#'   outputs      = NULL)}
#'
#' Next if only a portion of the dataset applies to the current cohort, you
#' can define a filter (\code{cf} field). This will be 
#' applied to the dataset to only return values relevant to this cohort. For
#' example, if we only want records where the column \code{DOSE} is 5 (for the 5
#' mpk cohort). We can use the following: 
#'
#' \preformatted{cohort[["cf"]]   = list(DOSE   = c(5))}
#' 
#' If the dataset has the headings \code{ID}, \code{DOSE} and \code{SEX}  and
#' cohort filter had the following format:
#' 
#' \preformatted{cohort[["cf"]]   = list(ID    = c(1:4),
#'                         DOSE  = c(5,10),
#'                         SEX   = c(1))}
#'
#'It would be translated into the boolean filter:
#'
#'\preformatted{(ID==1) | (ID==2) | (ID==3) | (ID==4)) & ((DOSE == 5) | (DOSE==10)) & (SEX == 1)}
#'
#' Optionally you may want to fix a system parameter to a different value for a
#' given cohort. This can be done using the cohort parameter (\code{cp}) field.
#' For example if you had the body weight defined as a system parameter 
#' (\code{BW}), and you wanted to fix the body weight to 70 for the current
#' cohort you would do the following:
#'
#' \preformatted{cohort[["cp"]]   = list(BW        = c(70))}
#'
#' Note that you can only fix parameters that are not being estimated.
#'
#' By default the underlying simulation output times will be taken from the
#' general output_times option (see \code{\link{system_set_option}}). However It may also be 
#' necessary to specify simulation output times for a specific cohort. The
#' \code{output_times} field can be used for this. Simply provide a vector of
#'  output times:
#'
#' \preformatted{cohort[["output_times"]]   = seq(0,100,2)}
#'
#' Next we define the dosing for this cohort. It is only necessary to define
#' those inputs that are non-zero. So if the data here were generated from
#' animals given a single 5 mpk IV at time 0. Bolus dosing is defined 
#' using \code{<B:times>} and \code{<B:events>}. If \code{Cp} is the central
#' compartment, you would pass this information to the cohort in the
#' following manner:
#'
#' \preformatted{cohort[["inputs"]][["bolus"]] = list()
#' cohort[["inputs"]][["bolus"]][["Cp"]] = list(TIME=NULL, AMT=NULL)
#' cohort[["inputs"]][["bolus"]][["Cp"]][["TIME"]] = c( 0) 
#' cohort[["inputs"]][["bolus"]][["Cp"]][["AMT"]]  = c( 5)}
#'  
#' Inputs can also include any infusion rates (\code{infusion_rates}) or
#' covariates (\code{covariates}). Covariates will have the default value
#' specified in the system file unless overwritten here. The units here are
#' the same as those in the system file
#'  
#' Next we need to map the outputs in the model to the observation data in the
#' dataset. Under the \code{outputs} field there is a field for each output. Here 
#' the field \code{ONAME} can be replaced with something more useful (like 
#' \code{PK}). 
#'
#' \preformatted{cohort[["outputs"]][["ONAME"]] = list()}
#'
#' If you want to further filter the dataset. Say for example you
#' have two outputs and the \code{cf} applied above reduces your dataset
#' down to both outputs. Here you can use the  "of" field to apply an "output filter"
#' to further filter the records down to those that apply to the current output ONAME. 
#' \preformatted{cohort[["outputs"]][["ONAME"]][["of"]] = list(
#'        COLNAME          = c(),
#'        COLNAME          = c())}
#' If you do not need further filtering of data, you can you can just omit the field.
#'
#' Next you need to identify the columns in the dataset that contain your
#' times and observations. This is found in the \code{obs} field for the 
#' current observation:
#' \preformatted{cohort[["outputs"]][["ONAME"]][["obs"]] = list(
#'          time           = "TIMECOL",
#'          value          = "OBSCOL",
#'          missing        = -1)}
#'
#' The times and observations in the dataset are found in the \code{’TIMECOL’} column 
#' and the \code{’OBSCOL’} column (optional missing data option specified by -1). 
#'
#' These observations in the dataset need to be mapped to the appropriate
#' elements of your model defined in the system file. This is done with the
#' \code{model} field:
#'
#' \preformatted{cohort[["outputs"]][["ONAME"]][["model"]] = list(
#'          time           = "TS",       
#'          value          = "MODOUTPUT",
#'          variance       = "PRED^2")}
#'
#' First the system time scale indicated by the \code{TS} placeholder above
#' must be specfied. The time scale must correspond to the data found in
#' \code{TIMECOL} above.  Next the model output indicated by the \code{MODOUTPUT}
#' placeholder needs to be specified. This is defined in the system file using
#' \code{<O>} and should correspond to \code{OBSCOL} from the dataset. Lastly the
#' \code{variance} field specifies the variance model. You can use the keyword
#' \code{PRED} (the model predicted output) and any variance parameters. Some
#' examples include:
#'
#' \itemize{
#'   \item \code{variance = "1"} - Least squares
#'   \item \code{variance = "PRED^2"} -  Weighted least squares proportional to the prediction squared
#'   \item \code{variance = "(SLOPE*PRED)^2"}  Maximum likelihood estimation where \code{SLOPE} is defined as a variance parameter (\code{<VP>})
#' }
#'
#' The following controls the plotting aspects associated with this output. The
#' color, shape and line values are the values used by ggplot functions. 
#'
#' \preformatted{cohort[["outputs"]][["ONAME"]][["options"]] = list(
#'         marker_color   = "black",
#'         marker_shape   = 16,
#'         marker_line    = 1 )}
#' 
#' If the cohort has multiple outputs, simply repeat the process above for the. 
#' additional cohorts. The estimation vignettes contains examples of this. 
#' 
#' \bold{Note: Output names should be consistent between cohorts so they will be grouped together when plotting results.}
#' 
#'@seealso Estimation vignette (\code{vignette("Estimation", package = "ubiquity")})
system_define_cohort <- function(cfg, cohort){
  
 if('options' %in% names(cohort)){
   cohort$options = c() }

 defopts = c()
 defopts[["marker_color"]]   = 'black'           
 defopts[["marker_shape"]]   = 0           
 defopts[["marker_line"]]    = 1
 
 validopts = c('marker_color', 'marker_shape', 'marker_line')

 

 # Default values for control structures
 isgood      = TRUE
 datasetgood = TRUE 
 

 #
 # checking the cohort name
 #
 if('name' %in% names(cohort)){
  if(cohort[["name"]] %in% names(cfg[["cohorts"]])){
    isgood = FALSE
    vp(cfg, sprintf('Error: cohort with name >%s< has already been defined', cohort[["name"]]))
  }
  else{
    name_check = ubiquity_name_check(cohort[["name"]])

    cohort_name = cohort[["name"]]
    # Checking the cohort name
    if(!name_check[["isgood"]]){
      isgood = FALSE
      vp(cfg, sprintf('Error: cohort with name >%s< is invalid', cohort[["name"]]))
      vp(cfg, sprintf('Problems: %s', name_check[["msg"]]))
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
      vp(cfg, sprintf('Error: A bolus input was specified for this cohort but'))
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
      # Check to see if covariates were defined
      # processing each covariates
      for(iname in names(cohort$inputs$covariates)){
        if(iname %in% names(cfg$options$inputs$covariates)){
          if('AMT'  %in% names(cohort$inputs$covariates[[iname]]) & 
             'TIME' %in% names(cohort$inputs$covariates[[iname]])){
            if(length(cohort$inputs$covariates[[iname]]$AMT) != length(cohort$inputs$covariates[[iname]]$TIME)){
              isgood = FALSE
              vp(cfg, sprintf('Error: For the covariates >%s< the length of ', iname))
              vp(cfg, sprintf('       the AMT and TIME fields need to be the same'))
            } else {
              # Checking for multiple entries in the time column for the same
              # time:
              if(length(cohort$inputs$covariates[[iname]]$TIME) != length(unique(cohort$inputs$covariates[[iname]]$TIME))){
                vp(cfg, sprintf('Warning: Covariate %s has duplicate time values. Only ', iname)) 
                vp(cfg, sprintf('         the first value for each time will be used')) 
                cohort$inputs$covariates[[iname]]$AMT  = cohort$inputs$covariates[[iname]]$AMT[!duplicated(cohort$inputs$covariates[[iname]]$TIME)]
                cohort$inputs$covariates[[iname]]$TIME = cohort$inputs$covariates[[iname]]$TIME[!duplicated(cohort$inputs$covariates[[iname]]$TIME)]
              }
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
     vp(cfg, sprintf('Error: A covariate was specified for this cohort but')) 
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
         vp(cfg, sprintf('       cohort$outputs$%s$obs$value  = "name"; ', oname))
       }

     
     }
     else{
      isgood = FALSE 
      vp(cfg, sprintf('Error: For the output >%s< no observation information was specified', oname))
      vp(cfg, sprintf('       cohort$outputs$%s$obs$time  = "name"; ', oname))
      vp(cfg, sprintf('       cohort$outputs$%s$obs$value = "name"; ', oname))
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

  # If the cohort has output times specified we check those to make sure that
  # the observation times lie within the range
  if("output_times" %in% names(cohort)){
    if(min(choutput_times) < min(cohort[["output_times"]]) |
       max(cohort[["output_times"]]) <  max(choutput_times)){
       vp(cfg, "Warning: cohort specified observation times lie outside of the range of")
       vp(cfg, "         specified output_times, the output_times will be automatically")
       vp(cfg, "         expanded to include these observation times.")

    }
  } 
}


if(isgood){
  cohort$name = NULL
  cfg$cohorts[[cohort_name]] = cohort
}
else{
  vp(cfg, "ubiquity::system_define_cohort()")
  vp(cfg, sprintf('Cohort name: >%s<', cohort_name))
  vp(cfg, 'There was an error and the cohort information was not set.')
}

  return(cfg)
  
}


#'@export
#'@title General Observation Details Function
#'@description Used to calculate observation details based on 
#' cohorts created with \code{system_define_cohort}
#'
#'@param pest vector of parameters to be estimated
#'@param cfg ubiquity system object    
#'@param estimation \code{TRUE} when called during an estimation and \code{FALSE} when called to test objective function or generate observation information for plotting
#'@param details \code{TRUE} to display information about cohorts as they are simulated (useful for debugging when passed through \code{\link{system_simulate_estimation_results}})
#'
#'@return  If estimation is TRUE then the output is a matrix  of observation details of the format:
#' \preformatted{od$pred  = [TIME, OBS, PRED, VAR, OUTPUT, COHORT] }
#' 
#'   The values are the observed (\code{OBS}) data, predicted
#'   values (\code{PRED}) and variance (\code{VAR}) at the given \code{TIME}. The columns \code{OUTPUT} and
#'   \code{COHORT} can be used for sorting. These should be unique numbers.
#' 
#'  When estimation is \code{FALSE} we output \code{od$pred} is a data frame with the
#'  following headings:
#' \preformatted{od$pred  = [TIME, OBS, PRED, VAR, SMOOTH, OUTPUT, COHORT] }
#' 
#'   The \code{TIME}, \code{OBS}, \code{PRED} and \code{VAR} are the same as those listed above. The \code{SMOOTH}
#'   variable is \code{FALSE} for rows that correspond to records in the dataset and
#'   \code{TRUE} when the \code{PRED} represents the smooth predictions. The \code{OUTPUT} and \code{COHORT}
#'   columns here are text values used when defining the cohorts.
#'  
#'  
#'  Also the \code{od$all} list item is created with all of the simulation information
#'  stored for each cohort:
#' \preformatted{od$all = [ts.time, ts.ts1, ... ts.tsn, pred, name, cohort]}
#'\itemize{
#'   \item \code{tstime}             - timescale of the system
#'   \item \code{ts.ts1, ... ts.tsn} - timescales defined in the system
#'   \item \code{pred}               - smooth prediction
#'   \item \code{name}               - state or output name corresponding to the prediction
#'   \item \code{cohort}             - name of the cohort for these predictions
#' }
#'
#' Lastly the field \code{isgood} will be set to \code{FALSE} if any problems are encountered, and \code{TRUE} if everything worked.
#' \preformatted{od$isgood = TRUE}
#'
#'@seealso \code{\link{system_define_cohort}} and \code{\link{system_simulate_estimation_results}}
system_od_general <- function(pest, cfg, estimation=TRUE, details=FALSE){

od     = c()
odall  = c()
odpred = c() 


isgood = TRUE

chidx = 1
for(cohort_name in names(cfg$cohorts)){

  if(details){vp(cfg, sprintf("Cohort %s", cohort_name))}
  # Making a local cohort-specific copy of cfg
  chcfg = cfg 

  # pulling out the current cohort
  cohort =  cfg$cohorts[[cohort_name]]

  # Smooth output times
  # By default the output times will be those for the simulation
  choutput_times = cfg$options$simulation_options$output_times

  # If this cohort has a different set of output times then 
  # we overwrite the defaults
  if("output_times" %in% names(cohort)){
    choutput_times = cohort[["output_times"]]
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
        chcfg=system_set_covariate( cfg       = chcfg,
                                    covariate = cname, 
                                    times     = cohort$inputs$covariates[[cname]]$TIME,
                                    values    = cohort$inputs$covariates[[cname]]$AMT)
    }
  }

  # Simulating the cohort  
  som = run_simulation_ubiquity(chparameters, chcfg, SIMINT_dropfirst=FALSE) 



  # Flag to indicate that an error has occurred and the parameters should be
  # dumped if debugging is enabled (bottom of the for loop)
  DUMP_PARAMS = FALSE

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
    odchunk$PRED = stats::approx( x      = som$simout[[sprintf("ts.%s", output_ts)]], 
                                  y      = som$simout[[output_name]], 
                                  xout   = odchunk$TIME, 
                                  method = "linear")$y

    # calculating the variance
    odchunk$VAR = calculate_variance(SIMINT_parameters = chparameters, 
                                     SIMINT_varstr     = cohort$outputs[[output]]$model$variance, 
                                     SIMINT_odchunk    = odchunk, 
                                     SIMINT_cfg        = chcfg)

    # Checking for integration failures by looking at the predcitons
    # and calculations made based on those predictions:
    if(any(c(is.na(odchunk$VAR), is.na(odchunk$PRED)))){
      isgood = FALSE
      DUMP_PARAMS = TRUE
      # If debugging is set we dump the information to the screen
      if(chcfg$options$logging$debug){
          vp(chcfg, sprintf("Simulation failed for cohort: %s, output: %s", cohort_name, output))
      }
    }


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

  # If debugging is enabled and the solver failed we dump the parameters
  # (initial guess, current value and the difference).
  if(chcfg$options$logging$debug & DUMP_PARAMS){
      vp(chcfg, "         Parameter | Guess         | Value         | Difference ")
      for(tmppname in names(pest)){
        vp(chcfg, 
          sprintf("%s | %s | %s | %s", 
          pad_string(str=tmppname, maxlength=18),
          var2string(maxlength=13, vars=chcfg$estimation$parameters$guess[[tmppname]], nsig_e=5, nsig_f=5),
          var2string(maxlength=13, vars=pest[[tmppname]], nsig_e=5, nsig_f=5),
          var2string(maxlength=13, vars=(pest[[tmppname]] - chcfg$estimation$parameters$guess[[tmppname]]), nsig_e=5, nsig_f=5)))
     }
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

od$pred   = odpred
od$all    = odall
od$isgood = isgood

return(od)

}


#'@export
#'@title Create Full Parameter Vector from Estimation Subset
#'@keywords internal
#'@description Can be used to take a subset of parameters (those being
#' estimated and returned from ' \code{\link{system_estimate_parameters}})
#' into a full list of system parameters.
#'@param pest list containing subset of parameters being estimated 
#'@param cfg ubiquity system object    
#'
#'@return Full list of parameters with default values for the currently selected parameter set and the values in pest  merged
#' 
#'@details    
#'  This function is used to build a full parameter set from a subset, and is
#'  normally used during parameter estimation in the observation details
#'  function when the entire parameter vector is needed to simulate the system.
#' 
#'  The function select_set pulls out a parameter set and can optionally select
#'  only a subset for estimation:
#' \preformatted{pnames = c('Vp', 'CL')
#'cfg = system_select_set(cfg, "default", pnames)}
#' 
#'  The default values of this subset can be accessed in the following way:
#' \preformatted{pest = system_fetch_guess(cfg)}
#' 
#'  The estimation routines will work with this reduced parameter set, but to
#'  run simulations the full set is needed. The full values can be retrieved 
#'  using the following: 
#' \preformatted{parameters = fetch_full_parameters(pest, cfg) }
#' 
#'@seealso \code{\link{system_fetch_guess}}, \code{\link{system_select_set}}
fetch_full_parameters <- function(pest, cfg){
#
#  function [parameters_full] = fetch_full_parameters(parameters_subset, cfg) 
#
#


parameters_full = cfg$parameters$values


for(pname in names(pest)){
   parameters_full[[pname]] = pest[[pname]] 
}

return(parameters_full)
}



#'@export
#'@title Set Value for Parameter
#'@description Assigns a value for a named parameter in a parameter list.
#'
#'@param cfg ubiquity system object    
#'@param parameters vector of parameters
#'@param pname parameter name
#'@param value value         
#'
#'@return parameters vector with \code{pname} set to \code{value} 
#'@details     
#'
#'  To set the parameter Vc to a value of 3, the following would be used:
#' \preformatted{parameters = system_fetch_parameters(cfg) 
#'parameters = system_set_parameter(cfg, parameters, pname = 'Vc', value = 3) 
#' }
#'
#'
system_set_parameter <- function(cfg, parameters, pname, value){

if( pname %in% names(cfg$parameters$values)){
  parameters[[pname]] = value
} else {
  vp(cfg, "ubiquity::system_set_parameter()") 
  vp(cfg, sprintf('parameter name (%s) not found', pname)) 
}

return(parameters)
}

#'@title Calculates the Variance in od_general      
#'@description Takes the variance specified as a string and evaluates it
#' locally, and returns that value
#'
#'@keywords internal
#'@param SIMINT_parameters system parameters
#'@param SIMINT_varstr string containing variance calculation 
#'@param SIMINT_odchunk chunk of observation details containing predictions, observations and the time
#'@param SIMINT_cfg ubiquity system object    
#'
#'@return Variance calculated for a given set of parameters in a model
#'
#'
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
#'@title Simulate Individual Response
#'@description Controls the execution of individual simulations with deSolve using either R scripts or loadable C libraries. 
#'@param SIMINT_parameters vector of parameters
#'@param SIMINT_cfg ubiquity system object    
#'@param SIMINT_dropfirst when \code{TRUE} it will drop the first sample point (prevents bolus doses from starting at 0)
#'
#'@return The simulation output is mapped (\code{som}) is a list.
#' time-course is stored in the \code{simout} element. 
#'\itemize{
#' \item The first column (\code{time}) contains the simulation time in the units of the simulation. 
#' \item Next there is a column for each: State, output and system parameter   
#' \item Models with covariate will contain the initial value  (prefix: \code{SIMINT_CVIC_}) as well as the values at each time point
#' \item Each static and dynamic system parameter is also passed through
#' \item A column for each timescale is returned with a "\code{ts.}" prefix.
#'}
#'@seealso Simulation vignette (\code{vignette("Simulation", package = "ubiquity")})
run_simulation_ubiquity = function(SIMINT_parameters,SIMINT_cfg, SIMINT_dropfirst=TRUE){

SIMINT_isgood = TRUE

if(length(setdiff(names(SIMINT_parameters), names(SIMINT_cfg$parameters$values))) > 0){
  vp(SIMINT_cfg, "Error: You have specified one or more system parameters but have not")
  vp(SIMINT_cfg, "   defined them system file. To use these parameters:")
  vp(SIMINT_cfg, paste("   ", paste(setdiff(names(SIMINT_parameters), names(SIMINT_cfg$parameters$values)), sep=", "), sep=""))
  vp(SIMINT_cfg, "   define them using the <P> delimiter")
  SIMINT_isgood = FALSE
}

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
SIMINT_simulation_options$sample_bolus_delta             = 1e-6
SIMINT_simulation_options$sample_forcing_delta           = 1e-3

SIMINT_solver_opts = ""
if(length(SIMINT_cfg$options$simulation_options$solver_opts)>0){
  for(SIMINT_option in names(SIMINT_cfg$options$simulation_options$solver_opts)){
    SIMINT_solver_opts = sprintf("%s, %s=SIMINT_cfg$options$simulation_options$solver_opts$%s",SIMINT_solver_opts, SIMINT_option,SIMINT_option)
  }
}

# overriding the default simulation options
for(SIMINT_option in names(SIMINT_cfg$options$simulation_options)){
  if(is.null(SIMINT_simulation_options[[SIMINT_option]])){
    vp(SIMINT_cfg, paste("Unknown simulation option", SIMINT_option))}
  else{
    SIMINT_simulation_options[[SIMINT_option]] = SIMINT_cfg$options$simulation_options[[SIMINT_option]] }
}


# It can be important to force the solver to evaluate 
# the system at specific times to make sure all events 
# are observed. The way bolus values are handled means 
# the system will be evaluated at each bolus event. However
# other events must be accounted for explicitly. This includes 
# the time varying inputs like infusion_rates and timevarying parameters.
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
  SIMINT_IC = eval(parse(text=paste0(
                               "system_IC_",
                               SIMINT_cfg[["options"]][["misc"]][["c_libfile_base"]],
                               "(SIMINT_cfg, SIMINT_parameters)"))) }
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
                                       "step", 
                                       SIMINT_simulation_options$output_times,
                                       SIMINT_simulation_options$sample_forcing_delta)
  
  eval(parse(text=sprintf("SIMINT_forces$%s = SIMINT_my_ff", SIMINT_rate_name)))

  # adding the time values to important times
  SIMINT_important_times =   c(SIMINT_my_ff[,1], SIMINT_important_times)
  
}


# processing covariates    
# JMH add force times for the covariates
for(SIMINT_cv_name in names(SIMINT_cfg$options$inputs$covariates)){
  # Looping through each infusion rate 
  # plucking out the rate name
  SIMINT_my_cv = SIMINT_cfg$options$inputs$covariates[[SIMINT_cv_name]]

  # the full covariate (time varying component)
  SIMINT_my_ff = make_forcing_function(SIMINT_my_cv$times$values,
                                       SIMINT_my_cv$values$values,
                                       SIMINT_my_cv$cv_interp, 
                                       SIMINT_simulation_options$output_times,
                                       SIMINT_simulation_options$sample_forcing_delta)
  eval(parse(text=sprintf("SIMINT_forces$%s = SIMINT_my_ff", SIMINT_cv_name)))
  # adding the time values to important times
  SIMINT_important_times =   c(SIMINT_my_ff[,1], SIMINT_important_times)

  # covariate evaluated at the initial condition and carried forward
  SIMINT_my_ff = make_forcing_function(SIMINT_my_cv$times$values[1],
                                       SIMINT_my_cv$values$values[1],
                                       SIMINT_my_cv$cv_interp, 
                                       SIMINT_simulation_options$output_times,
                                       SIMINT_simulation_options$sample_forcing_delta)
  eval(parse(text=sprintf("SIMINT_forces$SIMINT_CVIC_%s = SIMINT_my_ff", SIMINT_cv_name)))
  # adding the time values to important times
  SIMINT_important_times =   c(SIMINT_my_ff[,1], SIMINT_important_times)

  # Adding times to the force_times vector to ensure state resets at these values
  SIMINT_force_times = c(SIMINT_force_times, SIMINT_my_cv$times$values)
  
}


# creating the bolus inputs
SIMINT_eventdata = eval(parse(text=paste0(
       "system_prepare_inputs_",
       SIMINT_cfg[["options"]][["misc"]][["c_libfile_base"]],
       "(SIMINT_cfg, SIMINT_parameters, SIMINT_force_times)")))

# adding sample times around the bolus times to the important times
SIMINT_important_times =   c(sample_around(SIMINT_eventdata$time, 
                                           SIMINT_simulation_options$output_times,
                                           SIMINT_simulation_options$sample_bolus_delta), 
                             SIMINT_important_times)
 

# If important times were selected to be included then we set the output times
# equal to that vector (bounded on either end by the min and max of the
# selected simulation times).
if("yes" == SIMINT_simulation_options$include_important_output_times){
  SIMINT_important_times = SIMINT_important_times[(SIMINT_important_times >= min(SIMINT_simulation_options$output_times))  
                                                & (SIMINT_important_times <= max(SIMINT_simulation_options$output_times))]
  SIMINT_output_times_actual = sort(unique(SIMINT_important_times))
} else {
  SIMINT_output_times_actual = SIMINT_simulation_options$output_times}

# RN:: round to 10 decimals (failing to do this results in duplicate values)
# fixes the LSODA error
# Error in lsoda(y, times, func, parms, ...) : illegal input detected before taking any integration steps - see written message
SIMINT_output_times_actual = sort(unique(round(SIMINT_output_times_actual,10)))

if(!SIMINT_isgood){
  vp(SIMINT_cfg, "run_simulation_ubiquity()")
  stop("See above for more information")
}

# constructing the simulation command depending on the integrate_with option

if("r-file" == SIMINT_simulation_options$integrate_with){
# simulating the system using R
SIMINT_simcommand = paste0('SIMINT_simout = deSolve::ode(SIMINT_IC, 
                                                         SIMINT_output_times_actual,
                                                         system_DYDT_',
                                                         SIMINT_cfg[["options"]][["misc"]][["c_libfile_base"]],', 
                                                         SIMINT_cfg, 
                                                         method=SIMINT_simulation_options$solver, 
                                                         events=list(data=SIMINT_eventdata)')
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

SIMINT_simcommand = ' SIMINT_simout <- deSolve::ode(SIMINT_IC, SIMINT_output_times_actual, 
                                           func     = "derivs", 
                                           parms    = unlist(SIMINT_parameters),
                                           jacfunc  = NULL, 
                                           dllname  = SIMINT_cfg$options$misc$c_libfile_base, 
                                           initfunc = "initparams", 
                                           initforc = "initforcs",
                                           forcings = SIMINT_forces, 
                                           method   = SIMINT_simulation_options$solver, 
                                           nout     = length(names(SIMINT_cfg$options$mi$odes)), 
                                           events   = list(data=SIMINT_eventdata), 
                                           outnames = names(SIMINT_cfg$options$mi$odes)'
SIMINT_simcommand = sprintf('%s %s)', SIMINT_simcommand, SIMINT_solver_opts)
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
  SIMINT_simout  = eval(parse(text=paste0(
                              "system_map_output_", 
                              SIMINT_cfg[["options"]][["misc"]][["c_libfile_base"]],
                              "(SIMINT_cfg, SIMINT_simout, SIMINT_parameters, SIMINT_eventdata)")))
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
SIMINT_simout  = eval(parse(text=paste0(
                            "add_observation_errors_", 
                            SIMINT_cfg[["options"]][["misc"]][["c_libfile_base"]],
                            "(SIMINT_simout, SIMINT_parameters, SIMINT_cfg)")))
SIMINT_ERR_toc = proc.time()

SIMINT_simout_mapped$timing$adding_error   = SIMINT_ERR_toc - SIMINT_ERR_tic


# Adding the simout to the mapped output. Up until now all calculations and
# modifications should have been done in a matrix to speed things up
# and lastly we convert it to a data frame
SIMINT_simout_mapped$simout = as.data.frame(SIMINT_simout)

return(SIMINT_simout_mapped) } 


#'@export
#'@title Converts the Wide/Verbose Output Simulation Functions into Data Frames
#'@description  
#' The functions \code{\link{run_simulation_ubiquity}}, \code{\link{simulate_subjects}}, or \code{\link{run_simulation_titrate}}
#' provide outputs in a more structured format, but it may be useful to
#' convert this "wide" format to a tall/skinny format. 
#'
#'@param cfg ubiquity system object    
#'@param som simulation output from \code{\link{run_simulation_ubiquity}}, \code{\link{simulate_subjects}}, or  \code{\link{run_simulation_titrate}}
#'
#'@return Data frame of the format:
#'
#'When applied to the output of \code{\link{run_simulation_ubiquity}} or  \code{\link{run_simulation_titrate}}
#'\itemize{
#'  \item \code{ts.time}                   - timescale of the system
#'  \item \code{ts.ts1}, ... \code{ts.tsn} - timescales defined in the system (<TS>)
#'  \item \code{pred}                      - predicted/simulated response
#'  \item \code{tt.ti1.x}                  - titration event information (*)
#'  \item \code{name}                      - state or output (<O>) name corresponding to the prediction
#'}
#'
#'When applied to the output of  \code{\link{simulate_subjects}}
#'\itemize{
#'  \item \code{ID}                      - subject ID
#'  \item \code{ts.time}                 - timescale of the system
#'  \item \code{ts.ts1, ... ts.tsn}      - timescales defined in the system (<TS>)
#'  \item \code{pred}                    - predicted/simulated response
#'  \item \code{tt.ti1.x}                - titration event information (*)
#'  \item \code{P1, P2, ... Pn}          - system parameters for the subject (<P>)
#'  \item \code{name}                    - state or output (<O>) name corresponding to the prediction
#'}
#' (* - field present when titration is enabled)
#'
#'
#'@seealso
#' \code{\link{run_simulation_titrate}} internally when running simulations.
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
    
    dfos = NULL
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

#'@title \code{pso} Wrapper for calculate_objective
#'@description The psoptim objective function assumes parameters will be a
#' vector and this function converts it to a named list to be consistent with the
#' ubiquity optmization routines. 
#' 
#'@keywords internal
#'@param pvect      system parameters
#'@param cfg ubiquity system object    
#'
#'@return objective function value
#'
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


#'@title \code{GA} Wrapper for calculate_objective
#'@description Converts the parameter vector to a named list and returns the
#' negative of the objective (turning the maximization into a minimization) 
#'
#'@keywords internal
#'@param pvect system parameters
#'@param cfg ubiquity system object    
#'
#'@return objective function value
#'
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
#'@title Calculates the Value of the Specified Objective Function 
#'@description For a given set of system parameters the objective function
#' will be calculated based on defined cohorts and variance models.
#'
#'@keywords internal
#'
#'@param parameters system parameters
#'@param cfg ubiquity system object    
#'@param estimation boolean variable to indicate if the objective function is being called during parameter estimation
#'
#'@return If estimation is \code{TRUE} it will return the objective function
#'value, if it is \code{FALSE} it will return a list with an element
#'\code{value} containing the objective function value and an element named
#'\code{isgood} that is \cite{TRUE} if the objective function was successful.
calculate_objective <- function(parameters, cfg, estimation=TRUE){


  errorflag = FALSE
  # We default value to NA and we catch it at the bottom in case something
  # fails between here and there
  value = NA

  bounds_violated = FALSE
  bvdiff = 0.0

  # Checking the bounds of the parameters
  if(any(parameters < cfg$estimation$parameters$matrix$lower_bound)){
    bounds_violated  = TRUE
    bvdiff = bvdiff +  sum(abs(cfg$estimation$parameters$matrix$lower_bound[parameters < cfg$estimation$parameters$matrix$lower_bound]- parameters[parameters < cfg$estimation$parameters$matrix$lower_bound]))
    # set the parameters below the bounds to the lower bound
    parameters[parameters < cfg$estimation$parameters$matrix$lower_bound] = cfg$estimation$parameters$matrix$lower_bound[parameters < cfg$estimation$parameters$matrix$lower_bound]
  }

  if(any(parameters > cfg$estimation$parameters$matrix$upper_bound)){
    bounds_violated  = TRUE
    # calculate the diff for the multiplier below
    bvdiff = bvdiff +  sum(abs(cfg$estimation$parameters$matrix$upper_bound[parameters > cfg$estimation$parameters$matrix$upper_bound]- parameters[parameters > cfg$estimation$parameters$matrix$upper_bound]))
    # set the parameters above thier bounds to their upper bound
    parameters[parameters > cfg$estimation$parameters$matrix$upper_bound] = cfg$estimation$parameters$matrix$upper_bound[parameters > cfg$estimation$parameters$matrix$upper_bound]
  }

  # By default the objective function multiplier will be 1.0
  objmult = 1.0

  # however if here were bounds violations that will be increased
  if(bvdiff > 0){
    objmult = objmult + 10*exp(bvdiff)
  } 

  if(is.infinite(objmult)){
    objmult = .Machine$double.xmax/1e6
  }


  # Trying to pull out the observations
  # if we fail we throw an error and flip the error flag
  tcres = list(od=NULL)
  tcres = tryCatch(
   { 
      eval(parse(text=sprintf('od = %s(parameters, cfg)', cfg[["estimation"]][["options"]][["observation_function"]])))

    list(od=od, msg="success")},
    error = function(e) {
    vp(cfg, sprintf(' -> unable to retrieve observations'))
    vp(cfg, sprintf(' -> possible causes:')) 
    vp(cfg, sprintf('      o cfg$estimation$options$options$observation_function is not defined'))
    vp(cfg, sprintf('      o odd parameter combinations sent to the'))
    vp(cfg, sprintf('        objective function during estimation '))
    vp(cfg, sprintf('        is causing problems '))
    vp(cfg, sprintf(' Error: %s ', e$message))
    list(value=e, od=NULL, msg="error")})

  if(tcres$msg == "error"){
    errorflag = TRUE
  }

  
  # # Sometimes the eval above fails and it doesn't trigger the error block
  # # but when run outside of try catch it does work. 
  # if(is.null(tcres$od) & tcres$msg == "success"){
  #   eval(parse(text=sprintf('od = %s(parameters, cfg)', cfg$estimation$options$observation_function)))
  # }


  # detecting failures in the od function:
  if("isgood" %in% names(tcres$od)){
    if(!od$isgood){
      errorflag = TRUE
    }
  }

  od = tcres$od



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

    if(objmult > 1){
      value = abs(value)*objmult
    }
    '

 
  # this code attempts to calculate the objective function value:
  tcres = tryCatch(
   { 
   eval(parse(text=tCcode))
   list(value=value,
        msg="success") },
    error = function(e) {
    vp(cfg, sprintf(' Error: %s ', e$message))
    list(value=e, msg="error")})

   
    # If the objective function value is successfully calculated then we
    # pull out the value, otherwise we set value to a large value to push the
    # optimizer away from that parameter set and then we flip the error flag
    if(tcres$msg =="success"){
      value = tcres$value
    } else if(tcres$msg=="error"){
      value = .Machine$double.xmax/100
      errorflag = TRUE
    }
  }

  # if the objective function is Inf or NA we throw the error flag and
  # set the value to a large number
  if(is.na(value) | is.infinite(value)){
    value = .Machine$double.xmax/100
    errorflag = TRUE } 

  
  if(cfg$options$logging$debug){
    vp(cfg, paste("Obj:", toString(value) ," Bound Difference:", toString(bvdiff), "Objective Multiplier:", toString(objmult)))}


  # If we're in the estimation we return the objective function value
  # otherwise we return a structured output and any relevant errors
  if(estimation){
    return(value)
  }else{
    of = list()
    of$value = value 
    of$isgood = !errorflag
    of$od = od
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



#-----------------------------------------------------------
# system_estimate_parameters - controls the estimation process
#'@export
#'@title Control Estimation Process  
#'@description Manages the flow of parameter estimation using data specified with \code{system_define_cohort}.
#'
#'@param cfg ubiquity system object    
#'@param flowctl string to control what the flow of the function 
#'@param analysis_name string containing the name of the analysis 
#'@param archive_results boolean variable to control whether results will be archived
#'
#'@return parameter estimates
#'
#'@details
#'
#'  The \code{flowctl} argument can have the following values
#'  \itemize{
#'   \item \code{"plot guess"} return the initial guess
#'   \item \code{"estimate"} perform estimation
#'   \item \code{"previous estimate as guess"} load previous estimate for \code{analysis_name} and use that as the initial guess
#'   \item \code{"plot previous estimate"} return the previous estimate for \code{analysis_name}
#'  }
system_estimate_parameters <- function(cfg, 
                                       flowctl         = "plot guess",
                                       analysis_name   = "my_analysis", 
                                       archive_results = TRUE){

  # Pulling the output directory from the ubiquity object
  output_directory = cfg$options$misc$output_directory 

  # File to store estimation results
  fname_estimate = file.path(output_directory, paste(analysis_name, ".RData", sep=""))

  if((flowctl == "estimate") | (flowctl == "previous estimate as guess")){
    # Checking the analysis_name
    name_check = ubiquity_name_check(analysis_name)
    if(!name_check$isgood){
      vp(cfg, sprintf('ubiquity::system_plot_cohorts()', fmt="warning"))
      vp(cfg, sprintf('Error: the analyssis name >%s< is invalid', analysis_name), fmt="warning")
      vp(cfg, sprintf('Problems: %s', name_check$msg),   fmt="warning")
      analysis_name = 'analysis'
      vp(cfg, sprintf('Instead Using: %s', analysis_name), fmt="warning")
      }
  
    #loading the previous estimate and setting that as a guess
    if(flowctl == "previous estimate as guess"){
      vp(cfg, paste("Loading the previous solution from:", fname_estimate))
      load(file=fname_estimate)
      vp(cfg, "Setting initial guess to previous solution")
      isgood_previous = TRUE
      for(pname in names(cfg$estimation$parameters$guess)){
        if(pname  %in% names(pest)){
          cfg = system_set_guess(cfg, pname=pname, value=pest[[pname]]) 
        } else {
          isgood_previous = FALSE
          vp(cfg, paste("   Parameter", pname, "was not found in the previous estimate"), fmt="warning")
        }
      }
      if(!isgood_previous){
        vp(cfg, "   Some parameters were not specified in the previous estimate",           fmt="warning")
        vp(cfg, "   (see above). This can happen if you add parameters to be    ",          fmt="warning")
        vp(cfg, "   estimated. For those that were found, the previous estimate",           fmt="warning")
        vp(cfg, "   will be used. For the others the default values will be used instead.", fmt="warning")
        vp(cfg, "   system_estimate_parameters()", fmt="warning")
      }
    }

    # performing the estimation
    pe   = estimate_parameters(cfg)
    pest = pe$estimate
    save(pe, pest, file=fname_estimate)
    if(archive_results){
      archive_estimation(analysis_name, cfg)
      vp(cfg, paste("Estimate archived to:", fname_estimate))
    }
  } else if(flowctl == "plot guess"){
    pest = system_fetch_guess(cfg)
  } else if(flowctl == "plot previous estimate"){
    vp(cfg, paste("Loading the previous solution from:", fname_estimate))
    load(file=fname_estimate)
  }

return(pest)}
# /system_estimate_parameters
#-----------------------------------------------------------

#-----------------------------------------------------------
# estimate_parameters
#'@title Performs parameter estimation 
#'@description Performs the actual parameter estimation
#'@keywords internal
#'@param cfg ubiquity system object    
#'
#'@return list with elements: 
#' \itemize{
#' \item \code{estimate} - vector of parameter estimates
#' \item \code{raw} - raw output from the underlying optimization routine
#' \item \code{conv} - list of convergence criteria with keys \code{num} (numeric)  \code{text} (text description)
#' \item \code{obj} - objective function value
#' \item \code{statistics_est} - solution statistics
#' \item \code{sysup} - Text to update the system file with the parameter estimates
#' \item \code{estimate} - Names list of parameter estiamtes
#' \item \code{report} - Named list with elements for reporting
#' \item \code{cohorts} - Cohort elements from the \code{cfg} file
#' \item \code{cohort_view} - Cohort view from \code{system_view()} 
#' \item \code{system_file} - Contents of the system file when estimation was
#' run
#' }
estimate_parameters <- function(cfg){

# Pulling the output directory from the ubiquity object
output_directory = cfg$options$misc$output_directory 

pest = c()
pest$sysup = ''

# calling calculate_ojective outside of the estimation scope to make sure 
# it is working properly
odtest = calculate_objective(cfg$estimation$parameters$guess, cfg, estimation=FALSE)

  if(odtest$isgood){
      vp(cfg,'Starting Estimation', fmt="h2")
      vp(cfg, sprintf('Parameters:          %s', paste(names(cfg[["estimation"]][["mi"]]), collapse=", ")))
      vp(cfg, sprintf('Objective Function:  %s', cfg[["estimation"]][["objective_type"]]))
      vp(cfg, sprintf('Optimizer:           %s', cfg[["estimation"]][["options"]][["optimizer"]]))
      vp(cfg, sprintf('Method:              %s', cfg[["estimation"]][["options"]][["method"]]))
      vp(cfg, sprintf('Observation Detials: %s', cfg[["estimation"]][["options"]][["observation_function"]]))
      vp(cfg, sprintf('Integrating with:    %s', cfg[["options"]][["simulation_options"]][["integrate_with"]]))


      #
      # Clearing out any previous outputs summarizing the solution
      #  - estimate csv files
      #  - report
      if(file.exists(file.path(output_directory,"report.txt"        ))){file.remove(file.path(output_directory,"report.txt"        ))}
      if(file.exists(file.path(output_directory,"parameters_all.csv"))){file.remove(file.path(output_directory,"parameters_all.csv"))}
      if(file.exists(file.path(output_directory,"parameters_est.csv"))){file.remove(file.path(output_directory,"parameters_est.csv"))}
 

      # For global optimizers we want to check to see if the bounds make
      # sense. 
      if(cfg[["estimation"]][["options"]][["optimizer"]] %in% c("pso", "ga")){
        warn_bounds = FALSE
        for(pidx in 1:length(cfg[["estimation"]][["parameters"]][["guess"]])){

           pname = names(cfg[["estimation"]][["parameters"]][["guess"]])[pidx]
           plb   = cfg[["estimation"]][["parameters"]][["matrix"]][["lower_bound"]][pidx]
           pub   = cfg[["estimation"]][["parameters"]][["matrix"]][["upper_bound"]][pidx]

          if(plb ==   .Machine$double.eps){
            warn_bounds = TRUE
            vp(cfg, paste("Warning: The lower bound of", pname, "is eps"))
          }
          if(plb ==  -.Machine$double.xmax){
            warn_bounds = TRUE
            vp(cfg, paste("Warning: The lower bound of", pname, "is -inf"))
          }
          if(pub ==   -.Machine$double.eps){
            warn_bounds = TRUE
            vp(cfg, paste("Warning: The upper bound of", pname, "is -eps"))
          }
          if(pub ==  .Machine$double.xmax){
            warn_bounds = TRUE
            vp(cfg, paste("Warning: The upper bound of", pname, "is inf"))
          }
        }

        if(warn_bounds){
          vp(cfg, paste("The global optimizer",cfg[["estimation"]][["options"]][["optimizer"]], "needs reasonable parameter bounds."))  
          vp(cfg, "The bounds listed above may cause problems")
        }
      }

      # Default convergence critera
      conv_text    = "No termination criteria found"
      conv_num     = "-1"
      conv_lookup  = NULL

      estimation_tic = proc.time()
      #
      # We perform the estimation depending on the optimizer selected 
      #
      if(cfg[["estimation"]][["options"]][["optimizer"]] %in% c('optim', 'optimx', 'optimr')){
        if(cfg[["estimation"]][["options"]][["method"]]  %in% c("Brent", "L-BGFS-B")){
          eval(parse(text=sprintf('p = %s(cfg[["estimation"]][["parameters"]][["guess"]], 
                                          calculate_objective, 
                                          cfg     = cfg, 
                                          lower   = cfg[["estimation"]][["parameters"]][["matrix"]][["lower_bound"]],
                                          upper   = cfg[["estimation"]][["parameters"]][["matrix"]][["upper_bound"]],
                                          method  = cfg[["estimation"]][["options"]][["method"]] , 
                                          control = cfg[["estimation"]][["options"]][["control"]])', 
                                          cfg[["estimation"]][["options"]][["optimizer"]])))
        } else {
          eval(parse(text=sprintf('p = %s(cfg[["estimation"]][["parameters"]][["guess"]],
                                          calculate_objective, 
                                          cfg     = cfg, 
                                          method  = cfg[["estimation"]][["options"]][["method"]] , 
                                          control = cfg[["estimation"]][["options"]][["control"]])', 
                                          cfg[["estimation"]][["options"]][["optimizer"]])))
        
        }

        # algorithm specific convergence criteria:
        if("convergence" %in% names(p)){
          conv_num = toString(p$convergence) }

        conv_lookup = list("0"  = "Absolute toleraace reached (normal termination)",  
                           "1"  = "Maximum iterations reached",
                           "20" = "Bad initial guess objective fuction returns: INF, NULL or NA",
                           "21" = "Intermediate parameter set failed",
                           "10" = "Degeneracy of Nelder-Mead simplex",
                           "51" = paste("L-BFGS-B Warning: ", p[["message"]]),
                           "52" = paste("L-BFGS-B Error: ", p[["message"]]))

      }
      else if(cfg[["estimation"]][["options"]][["optimizer"]] %in% c('pso')){
        # Setting the random seed
        vp(cfg, paste('Random seed:         ', cfg[["options"]][["stochastic"]][["seed"]], sep=""))
        set.seed(cfg[["options"]][["stochastic"]][["seed"]])
        p = pso::psoptim(par     = as.vector(cfg[["estimation"]][["parameters"]][["guess"]]),
                         fn      = calculate_objective_pso, 
                         cfg     = cfg, 
                         lower   = cfg[["estimation"]][["parameters"]][["matrix"]][["lower_bound"]],
                         upper   = cfg[["estimation"]][["parameters"]][["matrix"]][["upper_bound"]],
                         control = cfg[["estimation"]][["options"]][["control"]])

        # algorithm specific convergence criteria:
        if("convergence" %in% names(p)){
          conv_num = toString(p$convergence) }
        conv_lookup = list("0" = "Absolute toleraace reached (normal termination)",
                           "1" = "Maximal number of function evaluations reached",
                           "2" = "Maximal number of iterations reached",
                           "3" = "Maximal number of restarts reached",
                           "4" = "Maximal number of iterations without improvement reached")
      }
      else if(cfg[["estimation"]][["options"]][["optimizer"]] %in% c('ga')){
        # Setting the random seed
        vp(cfg, paste('Random seed:         ', cfg[["options"]][["stochastic"]][["seed"]], sep=""))
        set.seed(cfg[["options"]][["stochastic"]][["seed"]])
        # par     = as.vector(cfg$estimation$parameters$guess),

        # This is a string of the control variables that the user passed on.
        # By default we have none (empty string):
        ctl_list = c(" ")

        # now we loop through each option and construct cs 
        if(!is.null(cfg[["estimation"]][["options"]][["control"]])){
          for(cname in names(cfg[["estimation"]][["options"]][["control"]])){
             ctl_list = c(ctl_list, sprintf('%s=cfg[["estimation"]][["options"]][["control"]]$%s', cname, cname))
            }
            ctl_str = paste(ctl_list, collapse=",\n ")
        }
        else{
          ctl_str  = "" }
        
          eval(parse(text=sprintf('p = ga(type    = "real-valued",
                                          fitness = calculate_objective_ga , 
                                          cfg     = cfg, 
                                          min     = cfg[["estimation"]][["parameters"]][["matrix"]][["lower_bound"]],
                                          max     = cfg[["estimation"]][["parameters"]][["matrix"]][["upper_bound"]]%s)', ctl_str)))

        conv_num    = "-1"
        conv_lookup = list("-1" = "GA has no termination criteria")
      }

      estimation_toc = proc.time()
      elapsed =  (estimation_toc - estimation_tic)[["elapsed"]]

      if(elapsed < 120){
        elapsed_time = var2string(elapsed, nsig_f=2, nsig_e=2)   
        elapsed_units= 'seconds'
      } else if(elapsed < 3600){
        elapsed_time = var2string(elapsed/60, nsig_f=2, nsig_e=2) 
        elapsed_units= 'minutes'
      } else {
        elapsed_time = var2string(elapsed/60/60, nsig_f=2, nsig_e=2)
        elapsed_units= 'hours'
      }

    # Displaying the convergence critieria
    if(!is.null(conv_lookup)){
      if(conv_num %in% names(conv_lookup)){
        conv_text = conv_lookup[[conv_num]]
      }
    }
    conv_desc = paste0("Exit status: (", conv_num, ") ", conv_text)

    vp(cfg, paste("Estimation Complete", sep=""), "h2")
    vp(cfg, paste("Duration: ", elapsed_time, " ", elapsed_units, sep=""))
    vp(cfg, conv_desc)

    # Keeping the convergence informaation 
    pest[["conv"]] = list(num  = conv_num,
                          text = conv_text,
                          desc = conv_desc)

    # because each optimizer returns solutions in a different format
    # we collect them here in a common structure
    # First we keep the 'raw' data
    pest$raw = p

    if(cfg[["estimation"]][["options"]][["optimizer"]] == "optim"){
      pest$estimate = p$par 
      pest$obj      = p$value
    } 
    else if(cfg[["estimation"]][["options"]][["optimizer"]] == "optimx"){
      pest$obj               = p$value
      for(pname in names(cfg[["estimation"]][["parameters"]][["guess"]])){
        pest[["estimate"]][[pname]] = p[[pname]]
      }

    } 
    # Particle swarm (pso) 
    else if(cfg[["estimation"]][["options"]][["optimizer"]] %in% c("pso")){
      # Pso returns the parameters as a vector so we 
      # have to put it back into a list for the other functions
      pest[["obj"]]      = p[["value"]]
      pest[["estimate"]] = list()
      pidx = 1
      for(pname in names(cfg[["estimation"]][["parameters"]][["guess"]])){
        pest$estimate[[pname]] = p$par[pidx]
        pidx = pidx+1
      }
    } 
    # Genetic algorithm (ga) output
    else if(cfg[["estimation"]][["options"]][["optimizer"]] %in% c("ga")){
       pest$obj = p@fitnessValue
       pest$estimation = structure(rep(-1, length(cfg[["estimation"]][["parameters"]][["guess"]])), 
                                      names=names(cfg[["estimation"]][["parameters"]][["guess"]]))
       pidx = 1
       for(pname in names(cfg[["estimation"]][["parameters"]][["guess"]])){
         pest$estimate[[pname]] = p@solution[pidx]
         pidx = pidx+1
       }
    }

   # Making sure the parameters are within the bounds
   if(any(pest$estimate < cfg$estimation$parameters$matrix$lower_bound)){
     pest$estimate[pest$estimate < cfg$estimation$parameters$matrix$lower_bound] = cfg$estimation$parameters$matrix$lower_bound[pest$estimate < cfg$estimation$parameters$matrix$lower_bound]
   }

   if(any(pest$estimate > cfg$estimation$parameters$matrix$upper_bound)){
     pest$estimate[pest$estimate > cfg$estimation$parameters$matrix$upper_bound] = cfg$estimation$parameters$matrix$upper_bound[pest$estimate > cfg$estimation$parameters$matrix$upper_bound]
   }

   files = NULL
   pest$statistics_est = NULL
   vp(cfg, "Calculating solution statistics", fmt="h2")
   vp(cfg, "Be patient this can take a while when there are many parameters.")

   tCcode = '
      # Generating the solution statistics and writing the results to a file
      pest$statistics_est = solution_statistics(pest$estimate, cfg)
      files = generate_report(parameters = pest$estimate, 
                              ss         = pest$statistics_est, 
                              cfg        = cfg,
                              conv_desc  = conv_desc)

      vp(cfg, "Contents of report.txt", fmt="h2")
      vp(cfg, files$report_file_contents, fmt="verbatim")
      
      vp(cfg, "system file update code", fmt="h2")
      vp(cfg, "If you are happy with the results, the following")
      vp(cfg, "can be used to update system.txt file. Just copy, ")
      vp(cfg, "paste, and delete the previous entries")'

   tcres = tryCatch(
    { 
      eval(parse(text=tCcode))
    "success"},
      error = function(e) {
        vp(cfg, "")
        vp(cfg, "Solution statistics calculation failed. This can happen ", fmt="warning" )
        vp(cfg, "when you have a parameter set that makes the system stiff,", fmt="warning")
        vp(cfg, "or when the parameters are not uniquely identifiable.", fmt="warning")
        vp(cfg, "")
        vp(cfg, "This is the output from the failed attempt:", fmt="warning")
        for(ename in names(e)){
          vp(cfg, paste("   DEBUG:", ename, "->",  toString(e[[ename]]), sep=" "), fmt="warning")
        }
        vp(cfg, "")
        vp(cfg, "You can run this manually using the following command:", fmt="warning")
        vp(cfg, "ss =  solution_statistics(pest, cfg)")
        vp(cfg, "The final parameter estimates are:")
    "error"})

    # Saving the report information 
    pest$report = files

    # appending a snapshot of the cohort information to the pest variable for archiving later 
    pest$cohorts      = cfg$cohorts
    pest$cohort_view  = system_view(cfg, "cohorts")

    # appending the system file
    pest$system_file = readLines(cfg$options$misc$system_file)

    for(pname in names(pest$estimate)){
      pindex = cfg$parameters$matrix$name == pname
      ptmp = c()
      ptmp$set_name  = cfg$parameters$current_set
      ptmp$value     = var2string(maxlength=12, nsig_f=5, nsig_e=5, vars=pest$estimate[[pname]])
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
          pstr =  '<P>  '
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
    cli::cli_verbatim(pstr)
    pest$sysup = paste(pest$sysup, pstr, "\n")
    }

    # Notifying the user if any parameters were found at their upper bound
    warn_bounds = FALSE
    for(pname in names(pest[["estimate"]])){
      if(compare_estimate(cfg = cfg, parameters = pest[["estimate"]], pname=pname) %in% c("U", "L")){
        if(!warn_bounds){
          vp(cfg, "The following parameters were found at ", fmt="warning")
          vp(cfg, "or near their bounds:",                   fmt="warning")
        }
        if(compare_estimate(cfg = cfg, parameters = pest[["estimate"]], pname=pname) == "U"){
          vp(cfg, paste(pname, ": upper bound", sep=""), fmt="warning")
        } 
        if(compare_estimate(cfg = cfg, parameters = pest[["estimate"]], pname=pname) == "L"){
          vp(cfg, paste(pname, ": lower bound", sep=""), fmt="warning")
        } 
        warn_bounds = TRUE
      }
    }

    # Writing system update text to a file
    sysup_file =file.path(cfg[["options"]][["misc"]][["output_directory"]], "system_update.txt")
    fileConn<-file(sysup_file)
    writeLines(pest$sysup, fileConn)
    close(fileConn)

    # Writing session information to a file
    SI_file = file.path(cfg[["options"]][["misc"]][["output_directory"]], "sessionInfo.RData")
    SI_text = file.path(cfg[["options"]][["misc"]][["output_directory"]], "sessionInfo.txt")
    SI = sessionInfo()
    # SI object
    save(SI, file=SI_file)
    # SI text
    utils::capture.output(sessionInfo(), file=SI_text)

  } else {
    vp(cfg, sprintf('The estimation was terminated. We were unable to   '))
    vp(cfg, sprintf('calculate the objective at the initial guess.'))
  }

  return(pest)
}
# /estimate_parameters
#-----------------------------------------------------------

#-----------------------------------------------------------
#system_simulate_estimation_results
#'@export
#'@title Simulate Results at Estimates
#'@description Simulates the system at the parameter estimates \code{pest} for creating diagnostic plots
#'
#'@param cfg ubiquity system object    
#'@param pest vector of parameters
#'@param details set \code{TRUE} to display information about cohorts as they are simulated (useful for debugging)
#'
#'@return observations in a list, see \code{\link{system_od_general}} when \code{estimation=FALSE}
#'
#'@seealso \code{\link{system_define_cohort}}, \code{\link{system_plot_cohorts}}
#' and the vignette on parameter estimation (\code{vignette("Estimation", package = "ubiquity")}) 
system_simulate_estimation_results <- function(pest, cfg, details=FALSE){
 observations = NULL
 eval(parse(text=sprintf('observations = %s(pest, cfg, estimation=FALSE, details=details)', cfg$estimation$options$observation_function)))
 return(observations)
}
#/system_simulate_estimation_results
#-----------------------------------------------------------
#system_fetch_guess
#'@export
#'@title Fetch Current Parameter Guesses
#'@description Fetch a list of the guesses for the current parameter set and
#' parameters selected for estimation
#'
#'@param cfg ubiquity system object    
#'
#'@return list of current parameter gauesses
system_fetch_guess <- function(cfg){
  return(cfg$estimation$parameters$guess)
}
# /system_fetch_guess
#-----------------------------------------------------------

#-----------------------------------------------------------
# system_plot_cohorts
#'@export
#'@title Plot Estimation Results
#'@description Generates figures for each cohort/output for a given set of
#' parameter estimates. 
#'
#'@param erp output from \code{system_simulate_estimation_results}
#'@param cfg ubiquity system object    
#'@param plot_opts list controling how predictions and data are overlaid 
#'@param analysis_name string containing the name of the analysis 
#'@param archive_results boolean variable to control whether results will be archived
#'@param prefix depreciated input mapped to analysis_name
#'
#'@details
#'
#' The general format for a plot option for a given output (\code{OUTPUT}) is:
#'
#' \code{plot_opts$outputs$OUTPUTt$option = value}
#'
#' The following options are:
#' \itemize{
#'  \item \code{yscale} and  \code{xscale} \code{= "linear" or "log"}
#'  \item \code{ylabel} and  \code{xlabel} \code{= "text"}
#'  \item \code{xlim}   and  \code{ylim}   \code{= c(min, max)}
#'  }
#'
#' It is also possible to control the \code{height} and \code{width} of the time course \code{tc} and observed vs predicted \code{op} file by specifying the following in the default units of \code{ggsave}.
#' \itemize{
#'  \item \code{plot_opts$tc$width  = 10}  
#'  \item \code{plot_opts$tc$height = 5.5} 
#'  \item \code{plot_opts$op$width  = 10}  
#'  \item \code{plot_opts$op$height = 8.0} 
#'  }
#'
#' To control the figures that are generated you can set the purpose to either "print", "present" (default) or "shiny".
#'
#'  \code{plot_opts$purpose = "present"} 
#'
#'@return List of plot outputs containing two elements \code{timecourse} and
#' \code{obs_pred}, for the time course of and observed vs predicted,
#' respectively. Both of these fields contain three elements for a given
#' output. For example, say there is an output named \code{PK} the both the
#' \code{timecourse} and \code{obs_pred} elements will have a field named
#' \code{PK} containing a ggplot object
#' and two fields \code{PK_png} and \code{PK_pdf} containing the paths to the
#' files containing that figure in the respective formats. 
#'@seealso The estimation vignette (\code{vignette("Estimation", package = "ubiquity")}) 
system_plot_cohorts <- function(erp, plot_opts=c(), cfg, analysis_name='analysis', archive_results = TRUE, prefix=NULL){


if(!is.null(prefix)){
  vp(cfg, "The input 'prefix' has been depreciated and you should use analysis_name now")
}
if(!is.null(prefix) & analysis_name == "analysis"){
  vp(cfg, " The input analysis_name is being overwritten by the value in 'prefix' to maintain compatibility with older scripts.")
  vp(cfg, " in the future this will be removed and an error will result.")
  analysis_name = prefix
}
# list of graphics objects to return
grobs = list()
grobs$outputs = c()

# This gets rid of NOTES for the R package. 
OBS = NULL
PRED = NULL

def = c() 
def$yscale = "linear"
def$xlim  = NULL
def$ylim  = NULL


# Pulling the output directory from the ubiquity object
output_directory = cfg$options$misc$output_directory 

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

if(!is.null(plot_opts$purpose)){
  def$purpose  = plot_opts$purpose
} else {
  def$purpose = "present" }
#def$dim$tc$height= 5.5
#def$dim$op$width = 10
#def$dim$op$height= 8

for(output in unique(erp$pred$OUTPUT)){

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
for(output in unique(erp$pred$OUTPUT)){
  p = ggplot()
  color_string = c()
  output_scale = plot_opts$outputs[[output]]$yscale

  for(cohort in unique(erp$pred$COHORT)){
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
      
      co_options  = cfg$cohorts[[cohort]]$outputs[[output]][["options"]]
      #eval(parse(text = sprintf('p = p + geom_point(data=SAMPLE, aes(x=TIME, y=OBS), color="%s", shape=co_options$marker_shape,   size=2.0)', co_options[["marker_color"]])))
      #eval(parse(text = sprintf('p = p + geom_line( data=SMOOTH, aes(x=TIME, y=PRED, color="%s"), linetype=co_options$marker_line, size=0.9)',cohort)))
      
      marker_shape = co_options[["marker_shape"]]
      if(is.character(marker_shape)){
        marker_shape = as.numeric(marker_shape)
      }
      marker_line = co_options[["marker_line"]]
      if(is.character(marker_line)){
        marker_line  = as.numeric(marker_line )
      }
      eval(parse(text = paste('p = p + geom_point(data=SAMPLE, aes(x=TIME, y=OBS), color=co_options[["marker_color"]], shape=marker_shape, size=2.0)', sep="" )))
      eval(parse(text = paste('p = p + geom_line( data=SMOOTH, aes(x=TIME, y=PRED, color="', cohort, '"), linetype=marker_line, size=0.9)',sep="")))

      if(is.null(color_string)){
        color_string = sprintf('"%s"="%s"', cohort, co_options[["marker_color"]])
      } else{
        color_string = sprintf('%s, "%s"="%s"', color_string, cohort, co_options[["marker_color"]])
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
  p = prepare_figure(p, purpose=def$purpose)

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
        p =  gg_log10_yaxis(fo       = p,
                            ylim_min = min(plot_opts$outputs[[output]]$ylim),
                            ylim_max = max(plot_opts$outputs[[output]]$ylim))
    } else {
       p = gg_log10_yaxis(p)

    }
  } else {
    # if the yscale isn't log and there are ylim specified
    if(!is.null(plot_opts$outputs[[output]]$ylim)){
      p = p + ylim(plot_opts$outputs[[output]]$ylim) } 
  }


  fname_pdf = file.path(output_directory, paste(analysis_name, "_timecourse_", output, ".pdf", sep=""))
  ggsave(fname_pdf, plot=p, device="pdf", height=def$dim$tc$height, width=def$dim$tc$width)
  vp(cfg, sprintf('Figure written: %s', fname_pdf))

  fname_png = file.path(output_directory, paste(analysis_name, "_timecourse_", output, ".png", sep=""))
  ggsave(fname_png, plot=p, device="png", height=def$dim$tc$height, width=def$dim$tc$width)
  vp(cfg, sprintf('Figure written: %s', fname_png))

  # storing the plot object to be returned to the user
  eval(parse(text=sprintf('grobs$timecourse$%s     = p',         output)))
  eval(parse(text=sprintf('grobs$timecourse$%s_png = fname_png', output)))
  eval(parse(text=sprintf('grobs$timecourse$%s_pdf = fname_pdf', output)))

  # storing the list of outputs as well
  grobs$outputs = c(grobs$outputs, output)
}



#
# creating the observed vs predicted plot
#

for(output in unique(erp$pred$OUTPUT)){
  p = ggplot()
  color_string = c()
  output_scale = plot_opts$outputs[[output]]$yscale


  for(cohort in unique(erp$pred$COHORT)){
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

      co_options  = cfg$cohorts[[cohort]]$outputs[[output]][["options"]]
      marker_shape = co_options[["marker_shape"]]
      if(is.character(marker_shape)){
        marker_shape = as.numeric(marker_shape)
      }
      #eval(parse(text = sprintf('p = p + geom_point( data=SAMPLE, aes(x=PRED, y=OBS, color="%s"), shape=co_options$marker_shape, size=2.0)',cohort)))
      eval(parse(text = paste('p = p + geom_point( data=SAMPLE, aes(x=PRED, y=OBS, color="',cohort, '"), shape=marker_shape, size=2.0)',sep = "")))

      if(is.null(color_string)){
        color_string = sprintf('"%s"="%s"', cohort, co_options[["marker_color"]])
      } else{
        color_string = sprintf('%s, "%s"="%s"', color_string, cohort, co_options[["marker_color"]])
      }
    }

  }

  # getting all of the observation data (not smooth) for the output
  opds = erp$pred[(erp$pred$OUTPUT == output) & !erp$pred$SMOOTH, ]
  # if we're working on a log scale we strip out all of the nonzero values
  if(output_scale == "log"){
     opds = opds[opds$PRED>0 & opds$OBS>0,] }
  


  
  # setting the title to the output label
  p = p+ ggtitle(plot_opts$outputs[[output]]$ylabel)

  # moving the legend to the bottom
  p = p + theme(legend.title = element_blank()) 
  p = p + theme(legend.position = 'bottom')     

  if(output_scale == "log"){
    if(!is.null(plot_opts$outputs[[output]]$ylim)){
      p =  gg_axis(fo       = p,
                   ylim_min = min(plot_opts$outputs[[output]]$ylim),
                   ylim_max = max(plot_opts$outputs[[output]]$ylim),
                   xlim_min = min(plot_opts$outputs[[output]]$ylim),
                   xlim_max = max(plot_opts$outputs[[output]]$ylim))
    } else {
      p =  gg_axis(fo       = p,
                   ylim_min = min(c(tds[!tds$SMOOTH, ]$OBS, tds[!tds$SMOOTH, ]$PRED)),
                   ylim_max = max(c(tds[!tds$SMOOTH, ]$OBS, tds[!tds$SMOOTH, ]$PRED)),
                   xlim_min = min(c(tds[!tds$SMOOTH, ]$OBS, tds[!tds$SMOOTH, ]$PRED)),
                   xlim_max = max(c(tds[!tds$SMOOTH, ]$OBS, tds[!tds$SMOOTH, ]$PRED)))
    }
  }

  # overlaying the line of identity
  p = p + geom_abline(slope=1, intercept=0)
  p = p + ylab("Observed") + xlab("Predicted")

  p = prepare_figure(p, purpose=def$purpose)
  eval(parse(text=sprintf('p = p + scale_colour_manual(values=c(%s))', color_string)))



  fname_pdf = file.path(output_directory, paste(analysis_name, "_obs_pred_", output, ".pdf", sep=""))
  ggsave(fname_pdf, plot=p, device="pdf", height=def$dim$op$height, width=def$dim$op$width)
  vp(cfg, sprintf('Figure written: %s', fname_pdf))

  fname_png = file.path(output_directory, paste(analysis_name, "_obs_pred_", output, ".png", sep=""))
  ggsave(fname_png, plot=p, device="png", height=def$dim$op$height, width=def$dim$op$width)
  vp(cfg, sprintf('Figure written: %s', fname_png))

  # storing the plot object to be returned to the user
  eval(parse(text=sprintf('grobs$obs_pred$%s     = p',         output)))
  eval(parse(text=sprintf('grobs$obs_pred$%s_png = fname_png', output)))
  eval(parse(text=sprintf('grobs$obs_pred$%s_pdf = fname_pdf', output)))

}


if(archive_results){
  fname_grobs = file.path(output_directory, paste(analysis_name, "_pr.Rdata", sep=""))
  vp(cfg, sprintf('Graphics objects written to: %s', fname_grobs))
  save(grobs, file=fname_grobs)
}
return(grobs)

}
#/system_plot_cohorts
#-----------------------------------------------------------

#-----------------------------------------------------------
#system_set_guess
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
#'@param lb optionally change the lower bound (\code{NULL})
#'@param ub optionally change the upper bound (\code{NULL}) 
#'
#'@return cfg ubiquity system object with guess and bounds assigned   
#'
#' @details 
#'
#' When performing a parameter estimation, the initial guess will be the value
#' specified in the \code{system.txt} file for the currently selected parameter set. The
#' following command can be used after the parameter set has been selected to
#' specify the value (\code{VALUE}) of the parameter \code{PNAME} and optionally the lower (\code{lb})
#' and upper (\code{ub}) bounds:
#' \preformatted{cfg = system_set_guess(cfg, pname="PNAME", value=VALUE, lb=NULL, ub=NULL)}
#'
#' To set the initial guess for the parameter Vc to a value of 3, the following
#' would be used:
#' \preformatted{cfg = system_set_guess(cfg, "Vc", value=3)}
#'
#' To specify the guess and overwrite the upper bound on Vc and set it to 5
#' \preformatted{cfg = system_set_guess(cfg, "Vc", value=3, ub=5) }
system_set_guess <- function(cfg, pname, value, lb=NULL, ub=NULL){

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
  vp(cfg, "ubiquity::system_set_guess()")
}

return(cfg)

}
#/system_set_guess
#-----------------------------------------------------------



#-----------------------------------------------------------
#generate_report  
#'@title Generate Text Report with Estimation Results
#'@description Internal function used to generate a report of estimation results
#'@keywords internal
#'
#'@param cfg ubiquity system object    
#'@param parameters list of parameter estimates
#'@param ss output from solution_statistics 
#'@param conv_desc description of convergence criteria
#'
#'@return List with the following elements: 
#'
#'\itemize{
#'   \item \code{report_file} name of report file          
#'   \item \code{report_file_contents} contents of report file
#'   \item \code{parameters_all_file} name of CSV file with all parameters 
#'   \item \code{parameters_est_file} name of CSV file with only the estimates 
#'}
generate_report  <- function( parameters, ss, cfg, conv_desc){


parameters_full = fetch_full_parameters(cfg=cfg, pest=parameters)

# Pulling the output directory from the ubiquity object
output_directory = cfg$options$misc$output_directory 

report_file         = file.path(output_directory,"report.txt")
parameters_all_file = file.path(output_directory,"parameters_all.csv")
parameters_est_file = file.path(output_directory,"parameters_est.csv")

notes_str = 'F=Fixed parameter, L=estimate at/near lower bound, U=estimate at/near upper bound'; 
notes_str = paste0(notes_str, "; ", conv_desc)

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
    pstr = sprintf('%s%s    ',   pstr, var2string(maxlength=10, vars=parameters_full[[pname]]))
    pstr = sprintf('%s%s    ',   pstr, var2string(maxlength=11, vars=ss$confidence_interval$lower_bound[[pname]]))
    pstr = sprintf('%s%s      ', pstr, var2string(maxlength=11, vars=ss$confidence_interval$upper_bound[[pname]]))
    pstr = sprintf('%s%s     ',  pstr, var2string(maxlength=8,  vars= ss$coefficient_of_variation[[pname]]))
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
      row_str = sprintf('%s%s', row_str, var2string(maxlength=20, vars=ss$covariance[ridx,cidx]))
    }
    cidx = cidx+1
  }
  rl = c(rl, row_str)
  ridx = ridx+1
}
# Creating variance /covariance matrix


rl = c(rl, '', '', '',
       'Misc Information')
rl = c(rl, sprintf('OBJ = %s', var2string(maxlength=1, vars=ss$objective)))
rl = c(rl, sprintf('AIC = %s', var2string(maxlength=1, vars=ss$aic)))
rl = c(rl, sprintf('BIC = %s', var2string(maxlength=1, vars=ss$bic)))
rl = c(rl, conv_desc)

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


files                       = list()
files$report_file           = report_file         
files$report_file_contents  = rl
files$parameters_all_file   = parameters_all_file 
files$parameters_est_file   = parameters_est_file 
files$parameters_est        = p_est
files$parameters_all        = p_all


return(files)

}
#/generate_report  
#-----------------------------------------------------------

#-----------------------------------------------------------
#archive_estimation
#'@title Archive Estimation Results 
#'@keywords internal
#'@description 
#'
#'  Archives the estimation results by moving the output files to the same file
#'  names with \code{'name'} prepended to them. This prevents them from being
#'  overwritten in a different analysis script the following files are
#'  archived:
#'  \itemize{
#'   \item \code{output/parameters_all.csv}
#'   \item \code{output/parameters_est.csv}
#'   \item \code{output/report.txt}
#'  }
#'  Example:
#'
#'   archive_estimation('mysoln', cfg)
#'
#' Would rename the files above 
#'  \itemize{
#'   \item \code{output/mysoln-parameters_all.csv}
#'   \item \code{output/mysoln-parameters_est.csv}
#'   \item \code{output/mysoln-report.txt}
#'  }
#'
#'@param name analysis name 
#'@param cfg ubiquity system object    
#'
#'@return Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE})
#'
archive_estimation <- function(name, cfg){


f.source      = c()
f.destination = c()


# Pulling the output directory from the ubiquity object
#output_directory = cfg$options$misc$output_directory 
output_directory = cfg[["options"]][["misc"]][["output_directory"]]

f.source      = c(f.source,      file.path(output_directory, "parameters_all.csv"))
f.source      = c(f.source,      file.path(output_directory, "parameters_est.csv"))
f.source      = c(f.source,      file.path(output_directory, "report.txt"        ))
f.source      = c(f.source,      file.path(output_directory, "sessionInfo.RData" ))
f.source      = c(f.source,      file.path(output_directory, "sessionInfo.txt" ))
f.source      = c(f.source,      file.path(output_directory, "system_update.txt" ))

f.destination = c(f.destination, file.path(output_directory, paste(name, "-parameters_all.csv", sep="")))
f.destination = c(f.destination, file.path(output_directory, paste(name, "-parameters_est.csv", sep="")))
f.destination = c(f.destination, file.path(output_directory, paste(name, "-report.txt"        , sep="")))
f.destination = c(f.destination, file.path(output_directory, paste(name, "-sessionInfo.RData" , sep="")))
f.destination = c(f.destination, file.path(output_directory, paste(name, "-sessionInfo.txt" ,   sep="")))
f.destination = c(f.destination, file.path(output_directory, paste(name, "-system_update.txt" , sep="")))

# clearing out the destination files to prevent old results from lingering
for(fidx in 1:length(f.destination)){ 
  if(file.exists(f.destination[fidx])){
    file.remove(f.destination[fidx]) 
  }
}

vp(cfg, "Archiving the estimation results", fmt="h2")
for(fidx in 1:length(f.source)){ 
  if(file.exists(f.source[fidx])){
    file.copy(f.source[fidx], f.destination[fidx], overwrite=TRUE)
    vp(cfg, sprintf('%s --> %s', f.source[fidx], f.destination[fidx]))
  }
}
TRUE}
#/archive_estimation
#-----------------------------------------------------------


#-----------------------------------------------------------
#compare_estimate
#'@title Compares Estimate to Bounds
#'@description Compares the parameter estimate to the bounds and indicates if
#' the estimate is near the bound.
#'
#'@keywords internal
#'
#'@param cfg ubiquity system object    
#'@param parameters list of parameter estimates
#'@param pname name of parameter to compare
#'
#'@return L - near the lower bound, U - near the upper bound
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
#/compare_estimate
#-----------------------------------------------------------



#'@title Calculate Solution Statistics
#'@keywords internal
#'@description Attempts to determine the variance/covariance matrix,
#' confidence intervals and CV percent for a list of parameter estimates 
#' \code{parameters}. This method was taken from the ADAPT 5 User's Guide
#' chapter 3.
#' 
#'@param cfg ubiquity system object    
#'@param parameters list of parameter estimates
#'
#'@return list containing information about the provided estimates
#'
#'@details 
#'
#' The returned list has the following format:
#' \itemize{
#'   \item \code{objective} - objective function value
#'   \item \code{num_observations} - number of observations
#'   \item \code{degrees_of_freedom} - degrees of freedom 
#'   \item \code{aic} - Akaike information criterion 
#'   \item \code{bic} - Bayesian (Schwarz) information criterion 
#'   \item \code{covariance} - variance covariance matrix
#'   \item \code{wls} - defined for weighted least squares objective with the following elements:
#'   \itemize{
#'     \item \code{jacobian}  - Jacobian matrix
#'     \item \code{weights}  - diagonal matrix of weights
#'     \item \code{error_variance}  - diagonal matrix of variances
#'   }
#'   \item \code{ml} - defined for maximum likelihood objective with the following elements:
#'   \itemize{
#'     \item \code{M} - Jacobian matrix with block for variance parameters
#'   }
#'   \item \code{coefficient_of_variation$pname} - CV percent for parameter \code{pname}
#'   \item \code{confidence_interval$lower_bound$pname} - Lower bound of the confidence interval for \code{pname}
#'   \item \code{confidence_interval$upper_bound$pname} - Upper bound of the confidence interval for \code{pname}
#' }
#'
#'@seealso Vignette on estimation (\code{vignette("Estimation", package = "ubiquity")}) 
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
  observations = NULL
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

#'@title Verify System Steady State
#'@keywords internal
#'
#'@description Takes the output  \code{\link{run_simulation_ubiquity}} and verifies that the system is running at steady state by analyzing the timecourse of all of the states in the system
#'
#'@param cfg ubiquity system object    
#'@param som output of \code{\link{run_simulation_ubiquity}} 
#'@return list with name \code{steady_state} (boolean indicating weather the system was at steady state) and \code{states} a vector of states that have steady state offset.  
check_steady_state  <- function(cfg, som){ 

  offset_found = FALSE

  res = list()
  res$states = c()

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
        res$states = c(res$states, sname)
       
       }
     }
  }

  res$steady_state = !offset_found

res}


#'@export
#'@title Verify System Steady State 
#'
#'@description Takes the ubiquity system object and other optional inputs to verify the system is running at steady state. This also provides information that can be helpful in debugging systems not running at steady state. 
#'
#'@param cfg ubiquity system object    
#'@param parameters        optional set of parameters (\code{NULL}) to check at steady state (if set to \code{NULL} then the parameters for the currently selected parameter set will be used)
#'@param zero_rates        Boolean value to control removing all rate inputs (\code{TRUE})
#'@param zero_bolus        Boolean value to control removing all bolus inputs (\code{TRUE})
#'@param output_times      sequence of output times to simulate for offset determination (\code{seq(0,100,1)})
#'@param offset_tol        maximum percent offset to be considered zero (\code{.Machine$double.eps*100})
#'@param derivative_tol    maximum derivative value to be considered zero (\code{.Machine$double.eps*100})
#'@param derivative_time   time to evaluate derivatives to identify deviations (\code{0}), set to \code{NULL} to skip derivative evaluation
#'@return List with the following names
#' \itemize{
#' \item \code{steady_state} Boolean indicating weather the system was at steady state
#' \item \code{states_derivative} Derivatives that had values greater than the \code{derivative_tol}
#' \item \code{states_simulation} States that had values greater than the \code{offset_tol}
#' \item \code{som} Simulated output 
#' \item \code{derivatives} Derivatives
#' \item \code{states_derivative_NA_NaN} States that had derivatives that evaluated as either NA or NaN
#' \item \code{states_simulation_NA_NaN} States with simulation values that had either NA or NaN
#' \item \code{derivative_tc} Data frame with the timecourse of states where the derivative was found to be greater than tolerance (states_derivative)
#' }
system_check_steady_state  <- function(cfg, 
                                       parameters        = NULL, 
                                       zero_rates        = TRUE,
                                       zero_bolus        = TRUE,
                                       output_times      = seq(0,100,1),
                                       offset_tol        = .Machine$double.eps*100,
                                       derivative_tol    = .Machine$double.eps*100, 
                                       derivative_time   = 0){ 

  vp(cfg, sprintf('Checking for steady state offset'), fmt="h2")
  res = list()
  res$states_simulation        = c()
  res$states_derivative        = c()
  res$states_simulation_NA_NaN = c()
  res$states_derivative_NA_NaN = c()
  res$som                      = c()
  res$derivatives              = list()
  res$derivative_tc            = NULL

  derivative_offset_found = FALSE
  simulation_offset_found = FALSE

  if(is.null(parameters)){
    parameters = system_fetch_parameters(cfg)
  }

  #
  # Clearing out inputs
  #
  if(zero_rates){
    cfg = system_zero_inputs(cfg, bolus=FALSE, rates=TRUE)
    vp(cfg, sprintf('   - Removing infusion inputs'))
  }
  if(zero_bolus){
    cfg = system_zero_inputs(cfg, bolus=TRUE, rates=FALSE)
    vp(cfg, sprintf('   - Removing bolus inputs'))
  }

  if(!is.null(output_times)){
    cfg=system_set_option(cfg, group  = "simulation", 
                               option = "output_times", 
                               output_times)
    vp(cfg, sprintf('   - Setting simulation times: %s', var2string_gen(output_times)))
  }
  
  vp(cfg, sprintf(' '))

  # Calculating the derivatives
  if(!is.null(derivative_time)){
    # First we calculate the initial conditions
    SIMINT_IC = eval(parse(text=paste0("system_IC_", 
                                        cfg[["options"]][["misc"]][["c_libfile_base"]],
                                       "(cfg, parameters)")))

    # Next we evaluate the derivative at that 
    # initial condition and the specified time
    SIMINT_DER = eval(parse(text=paste0("system_DYDT_", 
                                        cfg[["options"]][["misc"]][["c_libfile_base"]],
                                        "(derivative_time, SIMINT_IC, cfg)")))
    vp(cfg, sprintf(' First we analyze the derivatives, values of the ODEs, at time %s',var2string(derivative_time) ))
    vp(cfg, sprintf(' with a derivative_tol = %.3e', derivative_tol))
    vp(cfg, sprintf(' '))
    if(any(abs(SIMINT_DER$dy) > derivative_tol) | any(is.nan(SIMINT_DER$dy)) | any(is.na(SIMINT_DER$dy))){
      vp(cfg, sprintf(' Derivatives were found that were larger than the tolerance'))
      vp(cfg, sprintf(' ---------------------'))
      vp(cfg, sprintf('       dx/dt  | state  '))
      vp(cfg, sprintf(' ---------------------'))

      NA_NaN_FLAG             = FALSE
      derivative_offset_found = TRUE
      stctr = 1
      for(sname in names(cfg$options$mi$states)){

        # Storing the derivatives to be returned
        res$derivatives[sname] = SIMINT_DER$dy[stctr]
        
        # Checking to see if the state returned NaN or NA
        if(is.nan(SIMINT_DER$dy[stctr]) | is.na(SIMINT_DER$dy[stctr])){
           dxdtstr = pad_string(toString(SIMINT_DER$dy[stctr]), maxlength = 13)
           vp(cfg, sprintf('%s | %s', dxdtstr, sname))
           # flipping the flag
           NA_NaN_FLAG = TRUE 
          res$states_derivative_NA_NaN  = c(res$states_derivative_NA_NaN, sname)
        } else  {
          if(abs(SIMINT_DER$dy[stctr]) > derivative_tol){
           dxdtstr = sprintf("%s ", var2string(maxlength=13, nsig_e=3, nsig_f=2, vars=SIMINT_DER$dy[stctr]))
           vp(cfg, sprintf('%s| %s', dxdtstr, sname))
           res$states_derivative  = c(res$states_derivative, sname)
          }
        } 


        stctr = stctr +1
      }
      # If we hit some NA or NaNs we drop a message to the user
      if(NA_NaN_FLAG){
        vp(cfg, "One or more derivatives evaluated as NaN or NA, see above for details")
      }
      vp(cfg, sprintf(' '))
    } else {
      vp(cfg, sprintf(' The magnitudes of all derivatives were below the tolerance'))
      vp(cfg, sprintf(' '))
    }
  }

  # Simulating the system
  som = run_simulation_ubiquity(parameters, cfg, FALSE)
  vp(cfg, sprintf(' Next we simulate (from time = %s to %s) and calculate: ',  
        var2string(min(cfg$options$simulation_options$output_times, nsig_f=1, nsig_e=2)),  
        var2string(max(cfg$options$simulation_options$output_times, nsig_f=1, nsig_e=2))))
  vp(cfg, sprintf('   range                        = |max(state)|-|min (state)|)'))
  vp(cfg, sprintf('   absolute maximum observation = |max(state)| '))
  vp(cfg, sprintf('   percent offset               = 100*(|max|-|min|)/|max|'))
  vp(cfg, sprintf(' '))
  vp(cfg, sprintf(' The values where the range  > offset_tol = %.3e', offset_tol))
  vp(cfg, sprintf(' will be flagged for potential steady state offset.'))
  vp(cfg, sprintf(' '))

  NA_NaN_FLAG = FALSE

  for(sname in names(cfg$options$mi$states)){
     state = som$simout[[sname]]
     if(any(is.nan(state)) | any(is.na(state))){

       NA_NaN_FLAG = TRUE
       res$states_simulation_NA_NaN = c(res$states_simulation_NA_NaN, sname)
     }
     else {
       state_max = max(abs(state))
       # if the state has a value other than zero 
       # we look at it a little more closely
       if(state_max > 0){
         offset = abs(range(state)[2]-range(state)[1])
         pct_offset = offset/state_max*100
         if( offset > offset_tol){
           if(!simulation_offset_found){
             vp(cfg, sprintf(' Possible steady state offset'))
             vp(cfg, sprintf(' range       |             | Percent     | state'))
             vp(cfg, sprintf(' |max|-|min| | max(|state|)| Offset      | name '))
             vp(cfg, sprintf(' -------------------------------------------------'))
             simulation_offset_found = TRUE                               
          }
          vp(cfg, sprintf(' %.3e   | %.3e   | %.3e   | %s', offset, state_max, pct_offset, sname))
          res$states_simulation = c(res$states_simulation, sname)
         }
       }
     }
  }

  if(NA_NaN_FLAG){
    vp(cfg, "The following states contained NaN and/or NA values")
    vp(cfg,paste(res$states_simulation_NA_NaN, collapse=", ") )
  }


  # if we find derivative offsets we create a data frame with just the offsets
  # called and stick it in:  res$derivative_tc
  if(derivative_offset_found){
    for(pname in res$states_derivative){
      #creating a data frame of just the timescales
      tmpdf = som$simout[  , c(paste("ts.", names(cfg$options$time_scales), sep=""))]
      # Adding the state_name and state_values
      tmpdf = cbind(tmpdf, VALUE = som$simout[[pname]], STATE=pname)
      if(is.null(res$derivative_tc)){
        res$derivative_tc = tmpdf
      } else {
        res$derivative_tc = rbind(res$derivative_tc, tmpdf)
      }
    }
  }

  res$steady_state = !simulation_offset_found & !derivative_offset_found
  res$som = som

res}


#'@export
#'@title Make ggplot Figure Pretty
#'@description Takes a ggplot object and alters the line thicknesses and makes
#' other cosmetic changes to make it more appropriate for exporting. 
#'
#'@param purpose either \code{"present"} (default), \code{"print"} or \code{"shiny"}
#'@param fo ggplot figure object
#'@param y_tick_minor Boolean value to control grid lines
#'@param y_tick_major Boolean value to control grid lines
#'@param x_tick_minor Boolean value to control grid lines
#'@param x_tick_major Boolean value to control grid lines
#'
#'@return ggplot object 
#'
#'@examples
#'library("ggplot2")
#'df = data.frame(x = seq(0.01,10,.01),
#'                y = seq(0.01,10,.01)^2)
#'p       = ggplot(df, aes(x=x, y=y)) + geom_line()
#'# pretty up the axes
#'p       = prepare_figure(fo=p, purpose="print")
#'# pretty log10 y-axis 
#'p_logy  = gg_log10_yaxis(fo=p)
#'# pretty log10 x-axis 
#'p_logx  = gg_log10_xaxis(fo=p)
#'# pretty log10 yx-axis 
#'p_logxy = gg_axis(fo=p)
prepare_figure = function(purpose="present", fo,
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
# gg_axis
#'@export
#'@title Make Pretty ggplot x- or y-Axis Log 10 Scale
#'@description used to convert the x and y-axis of a ggplot to a log 10 scale that is more visually satisfying than the ggplot default.
#'
#'@param fo ggplot figure object
#'@param yaxis_scale  \code{TRUE} indicates that the y-axis should be log10 scaled
#'@param xaxis_scale  \code{TRUE} indicates that the x-axis should be log10 scaled
#'@param ylim_min     set to a number to define the lower bound of the y-axis
#'@param ylim_max     set to a number to define the upper bound of the y-axis
#'@param xlim_min     set to a number to define the lower bound of the x-axis
#'@param xlim_max     set to a number to define the upper bound of the x-axis
#'@param x_tick_label \code{TRUE} to show x tick labels, \code{FALSE} to hide the x tick labels
#'@param y_tick_label \code{TRUE} to show y tick labels, \code{FALSE} to hide the y tick labels
#'
#'@return ggplot object with formatted axis 
#'
#'@seealso \code{\link{gg_log10_xaxis}} and \code{\link{gg_log10_yaxis}}
#'
#'@examples
#'library("ggplot2")
#'df = data.frame(x = seq(0.01,10,.01),
#'                y = seq(0.01,10,.01)^2)
#'p       = ggplot(df, aes(x=x, y=y)) + geom_line()
#'# pretty up the axes
#'p       = prepare_figure(fo=p, purpose="print")
#'# pretty log10 y-axis 
#'p_logy  = gg_log10_yaxis(fo=p)
#'# pretty log10 x-axis 
#'p_logx  = gg_log10_xaxis(fo=p)
#'# pretty log10 yx-axis 
#'p_logxy = gg_axis(fo=p)
gg_axis  = function(fo, 
                     yaxis_scale  = TRUE,
                     xaxis_scale  = TRUE,
                     ylim_min     = NULL, 
                     ylim_max     = NULL, 
                     xlim_min     = NULL, 
                     xlim_max     = NULL, 
                     x_tick_label = TRUE,
                     y_tick_label = TRUE){


  # Defaulting the limits to null
  myxlim = NULL
  myylim = NULL

  # If any of the limits are null we build out the figure object so we can
  # pull the limits from that object

  if(any(is.null(ylim_min),is.null(ylim_min),is.null(xlim_min), is.null(xlim_max))){
    fob = ggplot_build(fo) }

  #
  # Finding the xlim values
  #
  if(any(is.null(xlim_min), is.null(xlim_max))){
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

  data_xlim = c(xlim_min, xlim_max)

  #
  # Finding the ylim values
  #
  if(any(is.null(ylim_min), is.null(ylim_max))){
    # looping through the figure object and pulling out all of the y data
    # to get the bounds on the y data
    ydata = c()
    for(didx in 1:length(fob$data)){
      # For geom_line/geom_point data
      if("y" %in% names(fob$data[[didx]])){
        ydata = c(ydata, fob$data[[didx]]$y)
      }

      # For geom_ribbon data
      if("ymin" %in% names(fob$data[[didx]])){
        ydata = c(ydata, fob$data[[didx]]$ymin)
      }
      if("ymax" %in% names(fob$data[[didx]])){
        ydata = c(ydata, fob$data[[didx]]$ymax)
      }
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

  data_ylim = c(ylim_min, ylim_max)

  #
  # Formatting the y axis
  #
  if(yaxis_scale){
    if(!is.null(data_ylim)){
    
      # Creating the major ticks
      ytick_major =  10^(floor(log10(data_ylim[1])):ceiling(log10(data_ylim[2])))
     
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
                                     oob = scales::squish_infinite,
                                     trans        = 'log10',
                                     labels       = eval(parse(text="scales::trans_format('log10', scales::math_format(10^.x))")))
      }
      else{
        fo = fo + scale_y_continuous(breaks       = ytick_major,
                                     minor_breaks = ytick_minor,
                                     trans        = 'log10',
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
      xtick_major =  10^(floor(log10(data_xlim[1])):ceiling(log10(data_xlim[2])))

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
                                    #limits       = myxlim,
                                     labels       = eval(parse(text="scales::trans_format('log10', scales::math_format(10^.x))")))
      }
      else{
        fo = fo + scale_x_continuous(breaks       = xtick_major,
                                     minor_breaks = xtick_minor,
                                     trans        = 'log10',
                                    #limits       = myxlim,
                                     labels       = NULL)
      }
    }
    fo = fo + annotation_logticks(sides='tb') 
  }


  fo = fo + coord_cartesian(xlim=myxlim, ylim=myylim, default=TRUE, clip="on")

fo}
#/gg_axis
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# gg_log10_yaxis
#'@export
#'@title Make Pretty ggplot y-Axis Log 10 Scale
#'@description Wrapper for \code{\link{gg_axis}} to create a log 10 y-axis
#'
#'@param fo ggplot figure object
#'@param ylim_min     set to a number to define the lower bound of the y-axis
#'@param ylim_max     set to a number to define the upper bound of the y-axis
#'@param x_tick_label \code{TRUE} to show x tick labels, \code{FALSE} to hide the x tick labels
#'@param y_tick_label \code{TRUE} to show y tick labels, \code{FALSE} to hide the y tick labels
#'
#'@return ggplot object with formatted axis 
#'@seealso \code{\link{gg_axis}} and \code{\link{gg_log10_xaxis}}
#'@examples
#'library("ggplot2")
#'df = data.frame(x = seq(0.01,10,.01),
#'                y = seq(0.01,10,.01)^2)
#'p       = ggplot(df, aes(x=x, y=y)) + geom_line()
#'# pretty up the axes
#'p       = prepare_figure(fo=p, purpose="print")
#'# pretty log10 y-axis 
#'p_logy  = gg_log10_yaxis(fo=p)
#'# pretty log10 x-axis 
#'p_logx  = gg_log10_xaxis(fo=p)
#'# pretty log10 yx-axis 
#'p_logxy = gg_axis(fo=p)
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
# gg_log10_xaxis
#'@export
#'@title Make Pretty ggplot x-Axis Log 10 Scale
#'@description Wrapper for \code{\link{gg_axis}} to create a log 10 x-axis
#'
#'@param fo ggplot figure object
#'@param xlim_min     set to a number to define the lower bound of the x-axis
#'@param xlim_max     set to a number to define the upper bound of the x-axis
#'@param x_tick_label \code{TRUE} to show x tick labels, \code{FALSE} to hide the x tick labels
#'@param y_tick_label \code{TRUE} to show y tick labels, \code{FALSE} to hide the y tick labels
#'
#'@return ggplot object with formatted axis 
#'
#'@seealso \code{\link{gg_axis}} and \code{\link{gg_log10_xaxis}}
#'
#'@examples
#'library("ggplot2")
#'df = data.frame(x = seq(0.01,10,.01),
#'                y = seq(0.01,10,.01)^2)
#'p       = ggplot(df, aes(x=x, y=y)) + geom_line()
#'# pretty up the axes
#'p       = prepare_figure(fo=p, purpose="print")
#'# pretty log10 y-axis 
#'p_logy  = gg_log10_yaxis(fo=p)
#'# pretty log10 x-axis 
#'p_logx  = gg_log10_xaxis(fo=p)
#'# pretty log10 yx-axis 
#'p_logxy = gg_axis(fo=p)
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


#---------------------------------------------------------------------------
#ubiquity_name_check
#'@title Check Names of Cohorts, Analyses, Reports, etc.
#'@description  Checks names specified for different analysis aspects (cohorts,
#' analyses, reports, etc.) to make sure that they start with a letter and
#' contain only letters, numbers and _
#'
#'@keywords internal
#'
#'@param test_name string containing the name to be tested
#'
#'@return List with Boolean element \code{isgood} that is \code{TRUE} when the name tests correct, \code{FALSE} when it fails. The element \code{msgs} contains a verbose message on why it fails.
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
#/ubiquity_name_check
#---------------------------------------------------------------------------

#'@export
#'@title Implementation of the \code{linspace} Function from Matlab
#'@description Creates a vector of n elements equally spaced apart.
#'
#'@param a initial number
#'@param b final number  
#'@param n number of elements  (integer >= 2)
#'
#'@return vector of numbers from \code{a} to \code{b} with
#'\code{n} linearly spaced apart
#'@examples
#' linspace(0,100, 20)
linspace = function(a, b, n=100){
   isgood = TRUE

   n = as.integer(n)
   
   if(!is.integer(n)){
     isgood = FALSE }

   if(n < 2){
     isgood = FALSE }

   if(!isgood){
     message("#> linspace error:")
     message("#> n should be a positive integer >= 2 ")
     message("#> defaulting to 100")
     n = 100
   }

   step = (b-a)/(n-1)
   return(seq(a,b,step))

}

#'@export
#'@title Implementation of the \code{logspace} Function from Matlab
#'@description Creates a vector of n elements logarithmically spaced apart.
#'
#'
#'@param a initial number
#'@param b final number  
#'@param n number of elements  (integer >=2)
#'
#'@return vector of numbers from \code{a} to \code{b} with
#'\code{n} logarithmically (base 10) spaced apart
#'
#'@examples
#' logspace(-2, 3,20)
logspace = function(a, b, n=100){
   isgood = TRUE

   n = as.integer(n)

   if(!is.integer(n)){
     isgood = FALSE }

   if(n < 2){
     isgood = FALSE }

   if(!isgood){
     message("#> logspace error:")
     message("#> n should be a positive integer >= 2 ")
     message("#> defaulting to 100")
     n = 100
   }

   step = (b-a)/(n-1)
   linseq = seq(a,b,step)
   return(10^linseq)
}

# -------------------------------------------------------------------------
# system_define_cohorts_nm  -  Defining cohorts from a NONMEM dataset
#'@export
#'@title Define Cohorts from NONMEM Input File
#'@description This function allows the user to define cohorts automatically
#' from a NONMEM dataset
#'@param cfg ubiquity system object    
#'@param DS Name of the dataset loaded using \code{system_load_data}
#'@param col_ID Column of unique subject identifier
#'@param col_CMT Compartment column
#'@param col_DV Column with observations or \code{’.’} for input
#'@param col_TIME Column with system time of each record
#'@param col_AMT Infusion/dose amounts (these need to be in the same units specified in the system.txt file)
#'@param col_RATE Rate of infusion or \code{’.’} for bolus
#'@param col_EVID EVID (0 - observation, 1 dose)
#'@param col_GROUP Column name to use for defining similar cohorts when generating figures.
#'@param filter List used to filter the dataset or \code{NULL} if the whole dataset is to be used (see filter rules or  \code{\link{nm_select_records}} or a description of how to use this option)
#'@param INPUTS List mapping input information in the dataset to names used in the system.txt file
#'@param OBS List mapping obseravation information in the dataset to nams used in the system.txt file
#'
#'
#'@return ubiquity system object with cohorts defined.
#'
#'@details
#'
#'\bold{NOTE: to use this function it is necessary that a timescale be define for the system time scale. For example, if the system time scale was days, something like the following is needed:}
#'\preformatted{<TS:days> 1}
#' 
#' Include all records in the dataset
#'\preformatted{filter = NULL}
#' 
#' Include only records matching the following filter
#'\preformatted{filter = list()
#'filter$COLNAME = c()}
#' 
#' Mapping information: 
#' 
#' The inputs mapping information (\code{INPUTMAP}) is alist with a field for each type of input:
#' input:
#'\itemize{
#' \item \code{bolus} List with a name for each bolus state in the dataset (\code{<B:?>}): each bolus name should have a \code{CMT_NUM} field indicating the compartment number for that state
#' \item \code{infusion_rates} List with a name for each rate in the dataset (\code{<R:?>}): each rate name should have a \code{CMT_NUM} field indicating the compartment number for that state
#' \item \code{covariates} List with for each covariate in the dataset (\code{<CV:?>}): each covariate name should have a \code{col_COV} indicating the column in the database that contains that covariate
#'}
#'From a coding perspective it looks like this:
#'\preformatted{INPUTMAP = list()
#'INPUTMAP$bolus$SPECIES$CMT_NUM            =  1
#'INPUTMAP$infusion_rates$RATE$CMT_NUM      =  1
#'INPUTMAP$covariates$CVNAME$col_COV        = 'CNAME'}
#'
#'The observation mapping information (\code{OBSMAP}) is a list with elements for each output as
#'described in for system_define_cohort. Each output is a list with the following names:
#'\itemize{
#'  \item variance Variance model for this output
#'  \item CMT Compartment number mapping observations for this output
#'  \item output Name of the output (\code{<O>}) corresponding with the observations
#'  \item missing Value indicating a missing observation or \code{NULL}
#'}
#'From a coding perspective it looks like this:
#'\preformatted{OBSMAP = list()
#'OBSMAP$ONAME=list(variance     = 'PRED^2',
#'                  CMT          =  1,
#'                  output       = '<O>',
#'                  missing      =  NULL )}
#'@seealso Estimation vignette (\code{vignette("Estimation", package = "ubiquity")})
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
                                    OBS       =  NULL){

vp(cfg, sprintf('Defining cohorts from NONMEM dataset'), fmt="h2")
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
            BOLUS_TIME_SCALE = NULL
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
            RATE_TIME_SCALE = NULL
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

            # Adding the rate for he current subject to the subinputs rate 
            subinputs$infusion_rates[[name]] = RATE_VECT
          } else {
            vp(cfg, sprintf("Warning: Subject >%s< rate >%s< no inputs found in dataset", toString(sid), name ))
          }
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

    #
    # If we have subjects we'll add them:
    #
    if(length(ALLSUBS) > 0){
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
              vp(cfg, sprintf('         than one value. Grouping was not applied for this subject'))
            }
             
          }
        }
       # Adding the cohort
       cfg = system_define_cohort(cfg, cohort)
      }
    } else {
      vp(cfg, sprintf('Error:   No valid subjects were found in the dataset'))
      vp(cfg, sprintf('         No cohorts were defined'))
      isgood = FALSE
    }

  } else {
    isgood = FALSE
  }

  
  if(!isgood){
    vp(cfg, "ubiquity::system_define_cohorts_nm()") }

cfg}
# /system_define_cohorts_nm 
# -------------------------------------------------------------------------


#'@export
#'@title Fetch System Timescale
#'@description Reads through the system information and tries to determine the
#' system time scale (the timescale that has a value of 1)
#'
#'@param cfg ubiquity system object    
#'
#'@return Name of the system timescale or \code{NULL} if it was not found
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
#'@keywords internal
#'@title Check NONMEM Dataset for Automatic Definitions  
#'@description Checks the dataset against the information specified by \code{\link{system_define_cohorts_nm}} for validity
#'
#'@param cfg ubiquity system object    
#'
#'@param DS Name of the dataset loaded using \code{system_load_data}
#'@param col_ID Column of unique subject identifier
#'@param col_CMT Compartment column
#'@param col_DV Column with observations or \code{’.’} for input
#'@param col_TIME Column with system time of each record
#'@param col_AMT Infusion/dose amounts (these need to be in the same units specified in the system.txt file)
#'@param col_RATE Rate of infusion or \code{’.’} for bolus
#'@param col_EVID EVID (0 - observation, 1 dose)
#'@param col_GROUP Column name to use for defining similar cohorts when generating figures.
#'@param filter List used to filter the dataset or \code{NULL} if the whole dataset is to be used (see filter rules or  \code{\link{nm_select_records}} or a description of how to use this option)
#'@param INPUTS List mapping input information in the dataset to names used in the system.txt file
#'@param OBS List mapping obseravation information in the dataset to names used in the system.txt file
#'
#'@return list with the following elements 
#' \itemize{
#'\item{"isgood"} Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE})
#'\item{"mywarning"} Boolean variable indicating warnings (\code{TRUE}) or no warnings (\code{FALSE})
#'\item{"dsraw"} Dataframe with the filtered raw data that was used
#'\item{"input_records"} Rows from \code{dsraw} containing the input information
#'\item{"obs_records"} Rows from \code{dsraw} containing the observation information
#'\item{"sids"} Subject ids found in \code{dsraw}
#'\item{"TSsys"} system time scale used in the dataset
#'}
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
  col_val = NULL
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
    vp(cfg, "ubiquity::system_nm_check_ds()")
  }
result}
# /system_nm_check_ds 
# -------------------------------------------------------------------------
# system_rpt_estimation
#'@export
#'@title Generate a Report from Parameter Estimation
#'@description This will take the output generated during a parameter estimation and append those results to a specified report.
#'
#'@param cfg ubiquity system object    
#'@param rptname report name (\code{"default"})
#'@param analysis_name string containing the name of the estimation analysis and used as a prefix to store the results
#'
#'@return ubiquity system object with estimation report appended
#'
#'@seealso \code{\link{system_rpt_read_template}}, the reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
#'and the estimation vignette (\code{vignette("Estimation", package = "ubiquity")})
system_rpt_estimation = function (cfg,
                               rptname        = "default",
                               analysis_name  = NULL){
# Pulling the output directory from the ubiquity object
output_directory = cfg[["options"]][["misc"]][["output_directory"]]

isgood = TRUE

if(is.null(analysis_name)){
 isgood = FALSE
 vp(cfg, " No analysis_name was specified")
}

# pulling out the onbrand object
obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)

if(is.null(obnd)){
  isgood = FALSE
  vp(cfg, "onbrand::system_fetch_rpt_onbrand_object returned NULL")
} else{
  if(obnd[["isgood"]]){
    # If the onbrand object is good we pull out the report type:
    rpttype = obnd[["rpttype"]]
  } else{
    # If there is something wrong with the onbrand object we set isgood to
    # false, dump an error and try to attach any messages we can
    isgood = FALSE
    vp(cfg, "Bad onbrand object:")
    if(!is.null(obnd[["msgs"]])){
      vp(cfg, obnd[["msgs"]])
    }
  }
}

if(isgood){
  # File names where the estimation results should be stored:
  fname_estimate = file.path(output_directory, paste(analysis_name, ".RData",          sep=""))
  fname_grobs    = file.path(output_directory, paste(analysis_name, "_pr.RData",       sep=""))
  fname_SI_text  = file.path(output_directory, paste(analysis_name, "-sessionInfo.txt", sep=""))
  vp(cfg, paste("Appending estimation results to report"), fmt="h2")
  vp(cfg, paste("  Report:   ", rptname,            sep=""))
  vp(cfg, paste("  Type:     ", rpttype,            sep=""))
  vp(cfg, paste("  Analysis: ", analysis_name,      sep=""))
  #---------------------------
  pe      = NULL
  pest    = NULL
  grobs   = NULL
  SI_text = NULL
  if(file.exists(fname_estimate)){
    vp(cfg, paste("Loading estimation results from file:", fname_estimate))
    # Loads the variable pe and pest
    load(fname_estimate)
  } else {
    vp(cfg, paste("Unable to load the estimate results from file:", fname_estimate))
  }
  if(file.exists(fname_grobs)){
    vp(cfg, paste("Loading the figures from file:", fname_grobs))
    # Loads the variablegrobs 
    load(fname_grobs)
  } else {
    vp(cfg, paste("Unable to load the figures from file:", fname_grobs))
  }
  if(file.exists(fname_SI_text)){
    vp(cfg, paste("Loading the session information from file:", fname_SI_text))
    SI_text = readLines(fname_SI_text)
  } else {
    vp(cfg, paste("Unable to load the session information from file:", fname_SI_text))
  }

  #---------------------------
  # Parameter estimate table
  petab = NULL
  if(!is.null(pe[["report"]][["parameters_est"]])){
    # pulling out the parameters table
    petab = as.data.frame(pe[["report"]][["parameters_est"]])
    # Trimming off the last row and last column
    petab = petab[1:(nrow(petab)-1),1:(ncol(petab)-1)]
    # Removing the guess column
    petab = petab[,c(1,3:ncol(petab))]
  
    ptab       = list()
    ptab$table = petab
    ptab$header_top = list(pname    = "Parameter", 
                           estimate = "Estimate",
                           cvpct    = "CV Percent", 
                           cilb     = "Lower Bound", 
                           ciub     = "Upper Bound", 
                           units    = "Units")
  }
  #---------------------------
  if("PowerPoint" == rpttype){
    if(file.exists(fname_estimate)){
      #
      # Adding a slide with the parameter estimates:
      #  this is triggered when confidence intervals were able to be
      #  calculated 
      if(!is.null(pe$report$parameters_est)){
        cfg = system_rpt_add_slide(cfg, 
          rptname  = rptname,
          template = "content_text",
          elements = list(
             title=
               list(content = "Parameter Estimates",
                    type    = "text"),
             content_body=
               list(content = ptab,
                    type    = "flextable")))
      }
    }
    if(file.exists(fname_grobs)){
      # Looping through each output and creating a slide for the timecourse
      # and the obs vs pred figures
      for(output in grobs$outputs){
        if(is.ggplot(grobs$timecourse[[output]]) & is.ggplot(grobs$obs_pred[[output]])){
          cfg = system_rpt_add_slide(cfg, 
            rptname  = rptname,
            template = "two_content_text",
            elements = list(
               title=
                 list(content = paste(output),
                      type    = "text"),
               content_left=
                 list(content = grobs[["timecourse"]][[output]],
                      type    = "ggplot"),
               content_right=
                 list(content = grobs[["obs_pred"]][[output]],
                      type    = "ggplot")))
        }
      }
    }


    # Summarizing estimation details
    if(!is.null(pe$report$parameters_est)){

      elist = c("1", paste0("Objective: ", pe[["obj"]]),
                "1", paste0("Objective: ", pe[["conv"]][["desc"]]))

      cfg = system_rpt_add_slide(cfg, 
        rptname  = rptname,
        template = "content_list",
        elements = list(
           title=
             list(content = "Estimation Details",
                  type    = "text"),
           content_body=
             list(content = elist,
                  type    = "list")))
    }
  }
  #---------------------------
  if("Word" == rpttype){
    #---------------------------
    # Parameter estiamtes
    if(file.exists(fname_estimate)){
      # Adding a table with the parameter estimates:
      if(!is.null(pe$report$parameters_est)){
        cfg = system_rpt_add_doc_content(cfg=cfg,
          rptname       = rptname,
          type          = "text",
          content       = list(style   = "Normal",
                               text    = "Parameter Estimates"))

        # Adding notes to the parameters table:
        ptab[["notes"]] = pe[["conv"]][["desc"]]
        cfg = system_rpt_add_doc_content(cfg=cfg,
          rptname       = rptname,
          type          = "flextable",
          content       = ptab)

        cfg = system_rpt_add_doc_content(cfg=cfg,
            rptname     = rptname,  
            type        = "break")
      }
    }


    #---------------------------
    # VPCs
    if(file.exists(fname_grobs)){
      # Looping through each output and creating a slide for the timecourse
      # and the obs vs pred figures
      for(output in grobs$outputs){
        if(is.ggplot(grobs$timecourse[[output]]) & is.ggplot(grobs$obs_pred[[output]])){
          cfg = system_rpt_add_doc_content(cfg=cfg, 
            rptname       = rptname,  
            type          = "ggplot",
            content       = list(image   = grobs[["timecourse"]][[output]],
                                 height  = 4.7))
 
          cfg = system_rpt_add_doc_content(cfg=cfg, 
            rptname       = rptname,  
            type          = "ggplot",
            content       = list(image   = grobs[["obs_pred"]][[output]], 
                                 height  = 4.7))
        }
      }
      cfg = system_rpt_add_doc_content(cfg=cfg, 
          rptname       = rptname,  
          type          = "break")
    }
 
    #---------------------------
    # variance/covariance matrix 
    if(!is.null(pe$statistics_est$covariance)){
      # If the column headers get too big we switch to landscape:
      if(stringr::str_length(paste(names(pe$estimate), collapse=" ")) > 40){
        cfg = system_rpt_add_doc_content(cfg=cfg, 
            rptname  = rptname,  
            type     = "section",
            content  = list(section_type  ="portrait"))
      }
 
      vcv =  signif(pe$statistics_est$covariance, digits=3)
      colnames(vcv) <- names(pe$estimate)
      rownames(vcv) <- names(pe$estimate)
      vcv = as.data.frame(vcv)
 
      tcontent = list()
      tcontent$table     =  vcv
      tcontent$header    = TRUE 
      tcontent$first_row = TRUE 
      tcontent$caption   = "Variance/Covariance Matrix"
      
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "table",
        content       = tcontent)        
 
      # If the column headers get too big we switch to landscape:
      if(stringr::str_length(paste(names(pe$estimate), collapse=" ")) > 40){
        cfg = system_rpt_add_doc_content(cfg=cfg, 
            rptname  = rptname,  
            type     = "section",
            content  = list(section_type  ="landscape",
                            height        = 8,
                            width         = 10))

      } else {
        cfg = system_rpt_add_doc_content(cfg=cfg, 
            rptname       = rptname,  
            type          = "break")
      }
    }
 
    #---------------------------
    # Estimation metadata
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Heading_1",
                             text    = "Estimation Details"))

      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Normal",
                             text    = "Loaded from files:"))
 
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Code",
                             text    = fname_estimate))
 
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Code",
                             text    = fname_grobs))
 
    # Appending cohort details
    cfg = system_rpt_add_doc_content(cfg=cfg, 
      type          = "text",
      content       = list(style   = "Heading_2",
                           text    = "Cohort Overview:"))
    for(line in pe[["cohort_view"]]){
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Code",
                             text    = line))
    }
 
    # Appending contents of system file
    cfg = system_rpt_add_doc_content(cfg=cfg, 
      type          = "text",
      content       = list(style   = "Heading_2",
                           text    = "System File:"))
    for(line in pe$system_file){
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Code",
                             text    = line))
    }
    # Appending the sessionInfo()
    if(!is.null(SI_text)){
      cfg = system_rpt_add_doc_content(cfg=cfg, 
        type          = "text",
        content       = list(style   = "Heading_2",
                             text    = "sessionInfo()"))
       for(line in SI_text){
         cfg = system_rpt_add_doc_content(cfg=cfg, 
           type          = "text",
           content       = list(style   = "Code",
                                text    = line))
       }
    }
  }
  #---------------------------
}

if(!isgood){
  vp(cfg, "ubiquity::system_rpt_estimation()")
  vp(cfg, "Unable to generate estimation report, see above for details") 
  stop()
  }

cfg}
#/system_rpt_estimation
# -------------------------------------------------------------------------

#'@export 
#'@title Simulate With Titration or Rule-Based Inputs
#'@description Provides an interface to \code{\link{run_simulation_ubiquity}}
#'  to start and stop simulations and apply rules to control dosing and state-resets.
#'@param SIMINT_p list of system parameters
#'@param SIMINT_cfg ubiquity system object    
#'@param SIMINT_dropfirst when \code{TRUE} it will drop the first sample point (prevents bolus doses from starting at 0)
#'
#'@return som
#'@seealso \code{\link{system_new_tt_rule}}, \code{\link{system_set_tt_cond}} and the titration vignette (\code{vignette("Titration", package = "ubiquity")})
run_simulation_titrate  <- function(SIMINT_p, SIMINT_cfg, SIMINT_dropfirst=TRUE){
  return(eval(parse(text=paste0("auto_run_simulation_titrate_", 
                    SIMINT_cfg[["options"]][["misc"]][["c_libfile_base"]], 
                    "(SIMINT_p, SIMINT_cfg, SIMINT_dropfirst)"))))
}

#-------------------------------------------------------------------------
#'@title Makes Forcing Function From Times and Values
#'@keywords internal
#'@description Takes a list of times, values, and an interpolation method
#'
#'
#'@param times  time values for the forcing function  
#'@param values magnitude for each time (same length of time)  
#'@param type string indicating the type of forcing function can be one of the following:
#' \itemize{
#'       \item  \code{"step"} for constant values that switch to new values at the times
#'       \item  \code{"linear"} to linearly interpolate between the points
#'        }
#'@param output_times vector of simulation output times
#'@param sample_delta_mult multiplier used to control the magnitude of spacing around event times
#'
#'@return matrix with two columns: first column is a vector of times and the second column is a vector of values
make_forcing_function = function(times, values, type, output_times, sample_delta_mult=1e-3){

if("step" == type){

 # The delta here is the switching time between steps. Below calculates it as
 # .1% of the smallest time between steps. 
 # delta         = 250000*.Machine$double.eps
  delta         = 250000000*.Machine$double.eps 
 if(length(times) > 1){
    offsets = ( times[2:length(times)] - times[1:length(times)-1])
    delta = sample_delta_mult*min(offsets)
 } 

 counter = 1
 while( counter <= length(times)){
  if(counter == 1){
    myforce = matrix(ncol=2,byrow=TRUE,data=c(times[counter], values[counter]))
  } else{
    # if(times[counter] == 0){
    #   delta         = 250*.Machine$double.eps
    # } else {
    #   delta         = 250*.Machine$double.eps*times[counter]
    # }
    # delta         = 250000*.Machine$double.eps


    # just before the switching time it takes the previous value
    myforce = (rbind(myforce, c((times[counter]-delta), values[counter-1])))
    # just afterwards it takes on the next value
    myforce = (rbind(myforce, c((times[counter]+delta), values[counter])))
  }
  counter = counter +1
 }

 # if the last switching time occurs before the end of the simulation
 # then we extend the last rate specified to the end of the simulation
 if(tail(myforce[,1], n=1) < tail(output_times, n=1)){
   myforce = (rbind(myforce, c((tail(output_times, n=1)), tail(values, n=1) )))
   }
}else  if("linear" == type){
   myforce = cbind(times, values)
 # if the last switching time occurs before the end of the simulation
 # then we extend the last rate specified to the end of the simulation
 if(tail(myforce[,1], n=1) < tail(output_times, n=1)){
   myforce = (rbind(myforce, c((tail(output_times, n=1)), tail(values, n=1) )))
 }
}

return(myforce)
}

#-------------------------------------------------------------------------
#'@title Define Sample Times Around Events 
#'@keywords internal
#'@description  When events, such as bolus doses, are applied to the system
#' rapid changes can occur. If the system is not sampled heavily around these
#' times, these changes may be missed in the output profiles. Based on the total
#' duration of the sample times, extra samples can be added near these events.
#' 
#'@param tvals vector of event times
#'@param ot    simualtion output times
#'@param sample_delta_mult multiplier used to control the magnitude of spacing around and following event times
#'
#'@return vector of event times and added samples
#'
#'@details 
#'
#'For more information on setting options for population simulation see the
#'stochastic section of the \code{\link{system_set_option}} help file.
#'
#'
sample_around = function(tvals, ot, sample_delta_mult=1e-6){

# removing any duplicates
tvals = unique(tvals)
# calculating the total simulation time 
# and using that as a basis for simulations
tlength = abs(max(ot) - min(ot))
tsample = tvals #c()
delta   = sample_delta_mult*tlength
ffollow = 0.10 # percent to follow effects of event
nfollow = 40   # number of sample times
vfollow = seq(0, tlength*ffollow, tlength*ffollow/nfollow)
for(tval in tvals){
  # This samples just before and just after the sample time
  tsample = c(tsample, (tval -delta), (tval + delta), (tval + 50*delta), (tval + 100*delta))

  # now adding ffolow percent of the total time to the end
  tsample = c(tsample, (vfollow + tval + 150*delta))
}

return(tsample)
}
#-------------------------------------------------------------------------
#'@title Require Suggested Packages 
#'@keywords internal
#'@description  Used to ensure packages are loaded as they are needed for the
#' stand alone distribution of ubiquity. If the ubiquity package is being used this
#' function simply returns 'TRUE' if the packages are installed and FALSE if
#' if not.
#' 
#'@param pkgs character vector of package names to check
#'
#'@return Boolean result of the loaded (stand alone) or installed (package) status for all of the packages
system_req <- function(pkgs){
  res_pkg  = NULL
  res_pkgs = c()
  for(pkg in pkgs){
    # If we're running as a stand alone script (i.e. the ubiquity package
    # hasn't been loaded, then we require packages
    if(!("ubiquity" %in% (.packages()))){
      eval(parse(text=sprintf("res_pkg = require(%s, quietly=TRUE)", pkg))) 
      res_pkgs = c(res_pkgs, res_pkg)
    } else {
      # otherwise we just return a Boolean value 
      # indicating if the package is installed 
      if(system.file(package=pkg) == ""){
        res_pkgs = c(res_pkgs, FALSE)
      } else {
        res_pkgs = c(res_pkgs, TRUE)
      }
    }
  }
all(res_pkgs)}
#-------------------------------------------------------------------------
#'@export
#'@title Check For Perl and C Tools 
#'@description  Check the local installation for perl and verify C compiler is installed and working.
#'  
#'@param checklist list with names corresponding to elements of the system to check.
#'@param verbose enable verbose messaging   
#'
#'@return List fn result of all packages 
#'@examples
#'\donttest{
#' invisible(system_check_requirements())
#'}
system_check_requirements <- function(checklist = list(perl    = list(check   = TRUE, perlcmd = "perl"),
                                                       C       = list(check   = TRUE)), 
                                                  verbose   = TRUE){

  res = list()

  if(verbose == TRUE){
    message("#> system_check_requirements()")}
                       

  # Checking Perl
  if("perl" %in% names(checklist)){
    res$perl = TRUE

    if(verbose == TRUE){ message("#> Testing perl, looking for a perl interpreter")}
    # First we see if we can find the interpreter
    if(as.character(Sys.which(checklist$perl$perlcmd)) != ""){
      if(verbose == TRUE){ message("#> Perl interpreter found, now testing it")}
      # if we find the interpreter we try to run a simple perl command
      perl_test_cmd = "perl -e  \"print 'perl works';\""
      
      perl_test_cmd_result = "" 

      perl_test_cmd_result_numeric = system(perl_test_cmd, ignore.stdout=TRUE)

      # If the numeric result is 0 then it the command executed 
      if( perl_test_cmd_result_numeric  == 0){
        if(verbose == TRUE){ message("#>    > Success: Perl runs, everything should be good")}
        res$perl = TRUE
        # perl_test_cmd_result_string = system(perl_test_cmd, intern = TRUE)
      } else {
        res$perl = FALSE
        if(verbose == TRUE){ message("#>    > Failure: Execution of perl test failed")}
      }
    } else {
      res$perl   = FALSE
      if(verbose == TRUE){
        message("#> Unable to find perl")
        message("#> ")
        if(.Platform$OS.type == "windows"){
          message("#> On Windows you will need to install a perl distribution.")
          message("#> Windows testing for ubiquity is done with strawberry perl:")
          message("#> http://strawberryperl.com ")
          message("#> ")
          message("#> After you've installed perl you may need to update")
          message("#> the PATH through the Control Panel (Environment Variables) ")
        
        }
        if(.Platform$OS.type == "unix"){
          message("#> On Unix (Linux, Mac OS, etc) perl should come standard.")
        }
      }
    }
  }


cfile = "
/* file mymod.c */
#include <R.h>
static double parms[1];
#define k1 parms[0]

/* initializer  */
void initmod(void (* odeparms)(int *, double *))
{
    int N=1;
    odeparms(&N, parms);
}
void derivs (int *neq, double *t, double *y, double *ydot,
             double *yout, int *ip)
{
    if (ip[0] <1) error(\"nout should be at least 1\");
    ydot[0] = -k1*y[0];
    yout[0] = y[0];
}
/* END file mymod.c */
"
  if("C" %in% names(checklist)){

    # if the model exists from before we unload it
    if(('mymod' %in% names(getLoadedDLLs()))){
      dyn.unload(getLoadedDLLs()$mymod[["path"]])}

    # temporary working direcotry
    twd = normalizePath(tempdir(), winslash = "/")
    dyn_file = file.path(twd, paste("mymod", .Platform$dynlib.ext, sep = ""))
    c_file   = file.path(twd, "mymod.c")
    o_file   = file.path(twd, "mymod.o")
    # Cleaning up any model files from previous run
    if(file.exists(dyn_file)){
       file.remove(dyn_file)}
    if(file.exists(c_file)){
       file.remove(c_file) }
    if(file.exists(o_file)){
       file.remove(o_file) }



    # Making the c file
    fileConn<-file(c_file)
    writeLines(cfile, fileConn)
    close(fileConn)
    
    # Compiling the C file
    if(verbose == TRUE){ message("#> Attempting to compile C file")}
    compile_result = system(paste(file.path(R.home("bin"), "R")," CMD SHLIB ", c_file), ignore.stderr=TRUE, ignore.stdout=TRUE)
    #compile_result = system(paste("R CMD SHLIB ", c_file), ignore.stderr=TRUE, ignore.stdout=TRUE)

    if(compile_result == 0){
      if(verbose == TRUE){ message("#>    > Success: C file compiled")}
      # loading it

      if(verbose == TRUE){ message("#> Loading the library ")}

        load_result = FALSE
        tryCatch(
         { 
          load_result = dyn.load(dyn_file)
          load_result = TRUE
         },
          warning = function(w) { },
          error = function(e) { })
      
        if(load_result){
          if(verbose == TRUE){ message("#>    > Success: C library loaded")}
          # running the model
          parms <- c(k1 = 0.04)
          Y     <- c(y1 = 10.0)
          times <- seq(0,10,.1)
          out <- ode(Y, times, func = "derivs", parms = parms,
                     dllname = "mymod",
                     initfunc = "initmod", nout = 1, outnames = "Conc")
          
          # unloading the model
          dyn.unload(getLoadedDLLs()$mymod[["path"]])

          res$C = TRUE
        } else {
          if(verbose == TRUE){ message("#>    > Failure: Unable to load the C library")}
          res$C = FALSE
        }
    } else {
      if(verbose == TRUE){ message("#>    > Failure: Unable to compile C file")}
      res$C = FALSE
    }
       
  }
res}

#-------------------------------------------------------------------------
#'@title Calculate AUC for Sparse Data 
#'@keywords internal
#'@description 
#' This is an implementation of Bailors method for calculating AUCs with
#' sparse sampling. It is taken from the following publication:
#'
#' Nedelman, J. R., Gibiansky, E., & Lau, D. T. (1995). Applying Bailer's
#' method for AUC confidence intervals to sparse sampling Pharmaceutical
#' Research, 12(1), 124-128.
#'
#'@param conc_data data frame containing the sparse data 
#'@param dsmap list with names specifying the columns:
#' \itemize{
#'  \item \code{NTIME}       Nominal time since last dose;  \code{"NTIME"} (default)
#'  \item \code{CONC}        Concentration data;  \code{"CONC"} (default)
#'  \item \code{ID}          Subject ID;  (\code{"ID"} (default)
#' }
#'@return list with the following elements
#' \itemize{
#'  \item \code{isgood}   Boolean value indicating the result of the function call
#'  \item \code{AUC}      Mean AUC
#'  \item \code{var_AUC}  Variance of the AUC
#'  \item \code{msgs}     Sequence of strings contianing a description of any problems 
#'  \item \code{obss}     Internal of observations
#'  \item \code{times}    Sequence of time corresponding to the rows of \code{obs}
#'  \item \code{r}        number of observations at each time point (rows correspond to rows of \code{obs})
#' }
AUC_Bailers_method = function(conc_data  = NULL, 
                              dsmap      = list(NTIME       = "NTIME", 
                                                CONC        = "CONC", 
                                                ID          = "ID")){
res     = list() 
msgs    = c()
isgood  = TRUE
        
AUC     = NULL
var_AUC = NULL 
r       = NULL
obs     = NULL
times   = NULL



# Making sure that the conc_data input is a data frame
if(!is.data.frame(conc_data)){
  isgood = FALSE
  msgs = c(msgs, "conc_data must be a data frame")
}
    

req_cols = c("NTIME", "CONC", "ID")

# Checking the contents of dsmap
for(cname in req_cols){
  if(!(cname %in% names(dsmap))){
    isgood = FALSE 
    msgs = c(msgs, paste("column: >", cname, "< not foundin dsmap", sep=""))
  }
}

# making sure that the columns specified in dsmap are found in conc_data
if(isgood){
  for(cname in names(dsmap)){
    if(!(dsmap[[cname]] %in% names(conc_data))){
      isgood = FALSE
      msgs = c( msgs, paste("column: >", dsmap[[cname]], "< not found in conc_data", sep = ""))
    }
  }
}

# Calculating the AUC
if(isgood){
  IDs     = unique(conc_data[[dsmap$ID]])
  IDs_str = paste("sub_", IDs, sep="")
  Times   = sort(unique(conc_data[[dsmap$NTIME]]))

  K = length(Times)
  R = length(IDs)

  # Putting the data into sparse matrix form:
  #  - A column for each ID
  #  - Row for each time


  obs   = matrix(ncol=R, nrow=K, data = -1)
  colnames(obs) <- IDs_str

  # Walking through the data to populating the concentrations
  for(ID in IDs){
    ID_str = paste("sub_", ID, sep="")
    for(Time in Times){
      if(nrow(conc_data[conc_data[[dsmap$NTIME]] == Time & conc_data[[dsmap$ID]] == ID, ]) == 1){
         concval = conc_data[conc_data[[dsmap$NTIME]] == Time & conc_data[[dsmap$ID]] == ID, ][[dsmap$CONC]]
         obs[Times == Time, ID_str] = concval
      }
      if(nrow(conc_data[conc_data[[dsmap$NTIME]] == Time & conc_data[[dsmap$ID]] ==  ID, ]) > 1){
         isgood = FALSE
         msgs = c(msgs, paste("At time", Time, " subject ", ID, " had more than 1 observation", sep="" ))
      }
    }
  }

  # calculating the weight vector
  #   w1 = (t(2)   -   t(1)  )/2 
  #   wk = (t(k+1) -   t(k-1))/2    k = [2,K-1];
  #   wk = (t(K)   -   t(K-1))/2 
  
  w   = rep(0, K)
  r   = rep(0, K)
  u   = rep(0, K)
  ssq = rep(0, K)

  w[1] = (Times[2]-Times[1  ])/2
  w[K] = (Times[K]-Times[K-1])/2

  for(k_idx in c(1:K)){

    # Calculating the weights for the numbers in between
    # the first and last elements
    if((k_idx > 1) & (k_idx < K)){
      w[k_idx] = (Times[k_idx+1]-Times[k_idx-1])/2
    }
    # nonzero elements for the row:
    nz_elements =  as.numeric(obs[k_idx, obs[k_idx,] > 0 ])

    # number of samples per time point
    r[k_idx] = length(nz_elements)

    # mean concentration for the current time point
    u[k_idx] = mean(nz_elements)

    # variance of the time point
    ssq[k_idx] = var(nz_elements)

  }

  AUC     = sum(w*u)
  var_AUC = sum(w^2*ssq/r)
}

if(!isgood){
  msgs = c(msgs, "ubiquity::AUC_Bailers_method()")
}

res$AUC     = AUC
res$var_AUC = var_AUC
res$r       = r
res$isgood  = isgood
res$msgs    = msgs
res$obs     = obs
res$times   = times

res}


#-------------------------------------------------------------------------
#'@export 
#'@title Automatic NCA
#'@description Performs NCA in an automated fashion 
#'
#'@param cfg ubiquity system object
#'@param dsname name of dataset loaded with (\code{\link{system_load_data}})
#'@param NCA_options specify a list of options for PKNCA to overwrite the
#'   defaults (default \code{NULL} will use defaults). For example if you want to
#'   set the maximum extrapolation of AUCinf to 10% and the minimum R-squared for
#'   half-life half-life of 0.8 you would use: \code{list(max.aucinf.pext=10, min.hl.r.squared=.9)}
#'@param NCA_min minimum number of points required to perform NCA for a given subset (default \code{4})
#'@param analysis_name string containing the name of the analysis (default 'analysis') to archive to files and reference results later
#'@param dsfilter list of names corresponding to the column names in the dataset and values are a sequence indicating values to keep (default \code{NULL}. Multiple names are and-ed together. For example the following would keep all of the records where dose is 1, 2, or 5 and the dose_number is 1
#'\preformatted{
#'  dsfilter = list(dose=c(1,2,5), dose_number = c(1))
#'}
#'@param extrap_C0 Boolean variable to enable automatic determination of initial drug concentration if no value is specified; the rules used by WinNonlin will be used: 
#' \itemize{
#'   \item If the route is \code{"iv infusion"} or \code{"extra-vascular"} and the data is single dose data, then a concentration of zero will be used. If repeat dosing is used, the minimum value from the previous dosing interval will be used.
#'   \item If the route is \code{"iv bolus"} then log-linear regression of the number of observations specified by \code{extrap_N} will be used. If the slope of these points is positive the first positive observation will be used as an estimate of C0
#'}
#'@param extrap_N number of points to use for back extrapolation (default \code{2}); this number can be overwritten for each subject using the \code{BACKEXTRAP} column in the dataset
#'@param sparse   Boolean variable used to indicate data used sparse sampling and the analysis should use the average at each time point (the \code{SPARSEGROUP} column must be specified in the \code{dsmap} below)
#'@param dscale factor to multiply the dose to get it into the same units as concentration (default \code{1}):
#' if you are dosing in mg/kg and your concentrations is in ng/ml, then \code{dscale = 1e6}
#'@param dsmap list with names specifying the columns in the dataset (* required): 
#' \itemize{
#'  \item \code{TIME}*       Time since the first dose; \code{"TIME"} (default)
#'  \item \code{NTIME}*      Nominal time since last dose;  \code{"NTIME"} (default)
#'  \item \code{CONC}*       Concentration data;  \code{"CONC"} (default)
#'  \item \code{DOSE}*       Dose given;  (\code{"DOSE"} (default)
#'  \item \code{ID}*         Subject ID;  (\code{"ID"} (default)
#'  \item \code{ROUTE}*      Route of administration;  \code{"ROUTE"} (default), can be either \code{"iv bolus"}, \code{"iv infusion"} or \code{"extra-vascular"}. Variants such as \code{"IV_bolus"} and \code{"extravascular"} should work as well.
#'  \item \code{DOSENUM}     Numeric dose (starting at 1) used for grouping multiple dose data; optional, \code{NULL} (default) for single dose data)
#'  \item \code{BACKEXTRAP}  Specifying the number of points to use to extrapolate the initial concentration for "iv bolus" dosing; optoinal f \code{NULL} (default) will use the value defined in \code{extrap_N} (note this value must be <= NCA_min)
#'  \item \code{SPARSEGROUP} Column containing a unique value grouping cohorts for pooling data. Needed when \code{sparse} is set to \code{TRUE}; optional, \code{NULL} (default)
#' }
#'@param dsinc (NOT CURRENTLY IMPLEMENTED) optional character vector of columns from the dataset to include in the output summary (default \code{NULL})
#'@return cfg ubiquity system object with the NCA results and if the analysis name is specified:
#' \itemize{
#'     \item{output/analysis_name-nca_summary-pknca.csv} NCA summary 
#'     \item{output/analysis_name-pknca_summary.csv} Raw output from PKNCA with subject and dose number columns appended 
#'     \item{output/analysis_name-nca_data.RData} objects containing the NCA summary and a list with the ggplot grobs
#' }
#'@seealso Vignette on NCA (\code{vignette("NCA", package = "ubiquity")}) 
system_nca_run = function(cfg, 
                          dsname            = "PKDS", 
                          dscale            = 1,
                          NCA_options       = NULL,
                          NCA_min           = 4,
                          analysis_name     = "analysis",
                          dsfilter          = NULL,
                          extrap_C0         = TRUE,
                          extrap_N          = 2,
                          sparse            = FALSE,
                          dsmap             = list(TIME        = "TIME", 
                                                   NTIME       = "NTIME", 
                                                   CONC        = "CONC", 
                                                   DOSE        = "DOSE", 
                                                   ID          = "ID", 
                                                   ROUTE       = "ROUTE", 
                                                   DOSENUM     = NULL, 
                                                   BACKEXTRAP  = NULL,
                                                   SPARSEGROUP = NULL),
                          dsinc             = NULL){

  # stores the report objects
  rptobjs = list()
  isgood = TRUE

  # Pulling the output directory from the ubiquity object
  output_directory = cfg[["options"]][["misc"]][["output_directory"]]

  invisible(system_req("PKNCA"))
  invisible(system_req("ggplot2"))

  #---------------------------------------
  # Checking the user input
  #saving files
  if(!(ubiquity_name_check(analysis_name)$isgood)){
    isgood=FALSE
    vp(cfg, paste("The analysis_name >", analysis_name, " is not valid", sep=""))
    vp(cfg, paste( ubiquity_name_check(analysis_name)$msg[1]))
  }

  # Checking the NCA options
  if(!is.null(NCA_options)){
    # First we check to make sure there aren't any specified options that
    # don't exist. To do this we pull the defaults from PKNCA
    NCA_options_all = PKNCA::PKNCA.options()

    # First we check to make sure the user specified valid options
    if(all(names(NCA_options) %in% names(NCA_options_all))){
      for(NCA_option in names(NCA_options)){
        # valid options are then set individually
        eval(parse(text=paste("PKNCA::PKNCA.options(",NCA_option,'=NCA_options[["',NCA_option,'"]])', sep="")))
      }
    } else {
      isgood = FALSE
      vp(cfg, paste("Error: NCA_options were specified but are not valid >",  paste(names(NCA_options)[!(names(NCA_options) %in% names(NCA_options_all))], collapse= ", "), "<", sep=""))
    }
  }

  if(dsname %in% names(cfg[["data"]])){
    DS = cfg[["data"]][[dsname]][["values"]]
    # If a filter has been specified then we apply it to the dataset
    if(!is.null(dsfilter)){
      # First we make sure the column names exist
      for(cn in names(dsfilter)){
        if(!(cn %in% names(DS))){
          isgood = FALSE
          vp(cfg, paste("Error: Subset column >", cn, "< was not found in the provided dataset", sep=""))
        }
      }
      # If it does then we apply the filter
      for(cn in names(dsfilter)){
        DS = DS[DS[[cn]] %in% dsfilter[[cn]], ]
      }
    }
  } else {
    isgood = FALSE
    vp(cfg, paste("Error: Dataset >", dsname, "< was not found use system_load_data() to create this dataset", sep=""))
  }

  # Now we check the dataset to make sure there are records. This catches
  # issues in the dataset itself or potential problems with applying filters
  if(nrow(DS) <1){
    isgood = FALSE
    vp(cfg, paste("Error: Dataset >", dsname, "< is empty", sep=""))
    vp(cfg, paste("Check the orignal dataset or the filters that were specified", sep=""))
  }

  # Creating the subsetting column
  if(is.null(dsmap[["DOSENUM"]])){
    DS[["SI_DOSENUM"]] = 1
  } else {
    if(dsmap[["DOSENUM"]] %in% names(DS)){
      DS[["SI_DOSENUM"]] =  DS[[dsmap[["DOSENUM"]]]]
    } else {
      isgood = FALSE
      vp(cfg, paste("Error: DOSENUM column >", dsmap[["DOSENUM"]], "< was not found in the provided dataset", sep=""))
    }

  }



  # Adding columns to account for normal vs sparse analysis
  if(sparse){
    if(is.null(dsmap[["SPARSEGROUP"]])){
      isgood = FALSE
      vp(cfg, paste("Error: The sparse option is set to >TRUE< but no grouping column was specified in the dsmap.", sep=""))
    } else {
      # Initializing the internal ID and concentration columns
      DS[["SI_ID"]]   = -1
      DS[["SI_CONC"]] = -1

      # Now populating those internal ID and concentration columns
      SI_ID = 1
      for(SPARSEGROUP in unique(DS[[dsmap[["SPARSEGROUP"]]]])){
        # Storing the ID for the sparse group
        DS[DS[[dsmap[["SPARSEGROUP"]]]] == SPARSEGROUP, ][["SI_ID"]] = SI_ID

        # Averaging the concentrations for this group at each time point.
        for(TIME_AVE in unique( DS[DS[[dsmap[["SPARSEGROUP"]]]] == SPARSEGROUP, ][[dsmap[["TIME"]]]])){
          DS[DS[[dsmap[["SPARSEGROUP"]]]] == SPARSEGROUP &DS[[dsmap[["TIME"]]]]== TIME_AVE, ][["SI_CONC"]] = 
                  mean(DS[DS[[dsmap[["SPARSEGROUP"]]]] == SPARSEGROUP & DS[[dsmap[["TIME"]]]]== TIME_AVE, ][[dsmap[["CONC"]]]])
        }
        SI_ID = SI_ID + 1
      }
    }
  } else {
    # For a normal subject by subject analysis these 
    # columns remain the same:
    DS[["SI_ID"]]   = DS[[dsmap[["ID"]]]]
    DS[["SI_CONC"]] = DS[[dsmap[["CONC"]]]]
  }

  # Checking extrapolation information
  if(!is.null(dsmap[["BACKEXTRAP"]])){
    if(dsmap[["BACKEXTRAP"]] %in% names(DS)){
      if(is.integer(DS[[dsmap[["BACKEXTRAP"]]]])){
        if(max(DS[[dsmap[["BACKEXTRAP"]]]]) > NCA_min){
          isgood = FALSE
          vp(cfg, paste("Error: Values in BACKEXTRAP column >", dsmap[["BACKEXTRAP"]], "< should be <= NCA_min >", NCA_min, "<", sep=""))
        } 
      } else {
        isgood = FALSE
        vp(cfg, paste("Error: BACKEXTRAP column >", dsmap[["BACKEXTRAP"]], "< should contain only integers", sep=""))
      }
    } else {
      isgood = FALSE
      vp(cfg, paste("Error: BACKEXTRAP column >", dsmap[["BACKEXTRAP"]], "< was not found in the provided dataset", sep=""))
    }
  }

  # Checking the obs information
  dscols = c("TIME", "NTIME", "CONC", "DOSE", "ROUTE", "ID")
  for(cn in dscols){
    # Checking to see if correct names were specified in obs
    if(cn %in% names(dsmap)){
      # Now making sure they exist in the dataset
      if(!(dsmap[[cn]] %in% names(DS))){
        isgood = FALSE
        vp(cfg, paste("Error: Dataset column ", dsmap[[cn]], " was not found in the dataset ",dsname, sep=""))
      }
    } else {
      isgood = FALSE
      vp(cfg, paste("Error: Dataset column >", cn, "< was not found",sep=""))
      vp(cfg, paste("        dsmap$",cn, ' = "colname"',sep=""))
    }
  }

  if(isgood){
    # checking the concentrations to make sure they are all greater than zero
    if(any(DS[[dsmap[["CONC"]]]] <=0)){
      vp(cfg, paste("Error: After filtering the data set some of the"))
      vp(cfg, paste("       concentration values are less than or equal to zero"))
      isgood=FALSE
    }

    #----
    # Routes can be specified using:                             The transformations below convert them into
    # IV_bolus, iv bolus, IV bolus, iv bolus                -->  iv bolus
    # IV_infusion, iv infusion, IV infusion, iv infusion    -->  iv infusion
    # extravascular, extra-vascular                         -->  extra-vascular
    # Converting all route specifications to lower case
    DS[[dsmap[["ROUTE"]]]] = stringr::str_to_lower(DS[[dsmap[["ROUTE"]]]])
    # Stripping any underscores after the iv
    DS[[dsmap[["ROUTE"]]]] = gsub(DS[[dsmap[["ROUTE"]]]], pattern="^iv_",          replacement="iv ")
    # making sure extra vascular is consistent as well
    DS[[dsmap[["ROUTE"]]]] = gsub(DS[[dsmap[["ROUTE"]]]], pattern="extravascular", replacement="extra-vascular")
                            
    # Allowed routes:
    ROUTES_GOOD = c("iv infusion", "extra-vascular", "iv bolus")

    # Route in the dataset:
    ROUTES_DS   =  unique(DS[[dsmap[["ROUTE"]]]])

    # If there are routes that are not in ROUTES_GOOD we throw an error:
    if(length(setdiff(ROUTES_DS, ROUTES_GOOD)) > 0){
      vp(cfg, paste("Error: the following routes are not allowed:",  paste(setdiff(ROUTES_DS, ROUTES_GOOD), collapse = ", ")))
      vp(cfg, paste("       should be either",  paste(ROUTES_GOOD, collapse = ", ")))
      isgood=FALSE
    
    }
    #----
  }

  # calculating the dose in the same mass units as concentration
  if(isgood){
    # The as.character --> as.numeric is used here in case DOSE has been read in
    # as a factor:
    DS[["SI_DOSE"]] = as.numeric(as.character(DS[[dsmap[["DOSE"]]]]))*dscale
  }


  # checking the data set columns to include in the summary output 
  if(!is.null(dsinc)){
    for(cn in dsinc){
      if(!(cn %in% names(DS))) {
        isgood = FALSE
        vp(cfg, paste("Error: Column name >", cn, "< to include in summary output was not found in the dataset",sep=""))
      }
    }
  }


  #---------------------------------------
  
  # these will store the summary information:
  NCA_sum       = NULL
  PKNCA_raw_all = NULL
  grobs_sum = list()

  # If everything checks out we'll go through and perform NCA on the
  # individuals
  if(isgood){
    # Sorting the dataset first by the subject (ID) and then by the time (TIME)
    eval(parse(text=paste("DS = DS[with(DS, order(SI_ID, ", dsmap[["ID"]], ",", dsmap[["TIME"]],")),]", sep="")))

    # Setting text based on the analysis type
    if(sparse){
      ID_label = "Group"
    } else {
      ID_label = "Subject"
    }

    # Storing these strings to be used in reporting:
    cfg[["nca"]][[analysis_name]]$text$ID_label = ID_label

    # Storing all of the analysis inputs to be available when reporting
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$dsname          =  dsname              
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$dscale          =  dscale             
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$NCA_min         =  NCA_min            
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$analysis_name   =  analysis_name      
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$dsfilter        =  dsfilter           
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$extrap_C0       =  extrap_C0          
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$extrap_N        =  extrap_N           
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$sparse          =  sparse             
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$dsmap           =  dsmap              
    cfg[["nca"]][[analysis_name]][["ana_opts"]]$dsinc           =  dsinc              

    # Looping through each subject ID
    subs  = unique(DS[["SI_ID"]])

    # Getting the uppoer and lower bounds on the whole dataset
    ylim_min = min(DS[[dsmap[["CONC"]]]])
    ylim_max = max(DS[[dsmap[["CONC"]]]])

    vp(cfg, "Running NCA", fmt="h2")
    for(sub in subs){
      # This is the entire dataset for the subject
      SUBDS = DS[DS[["SI_ID"]] == sub,]
      sub_str = paste("sub_", sub, sep="")


      # Figure with full time course for the subject/group
      ptmp = ggplot()
      if(sparse){
        eval(parse(text=paste("ptmp = ptmp + geom_point(data=SUBDS, aes(x=",dsmap[["TIME"]],", y=", dsmap[["CONC"]],"),  shape=16, color='grey' )", sep="")))
      } else {
        eval(parse(text=paste("ptmp = ptmp +  geom_line(data=SUBDS, aes(x=",dsmap[["TIME"]],", y=", dsmap[["CONC"]],",   group=",dsmap[["ID"]], ")           , color='grey' )", sep="")))
        eval(parse(text=paste("ptmp = ptmp + geom_point(data=SUBDS, aes(x=",dsmap[["TIME"]],", y=", dsmap[["CONC"]],"),  shape=16, color='grey' )", sep="")))
      }

      ptmp = prepare_figure(fo=ptmp, purpose="present")
      ptmp = gg_log10_yaxis(fo=ptmp) #, ylim_max=ylim_max, ylim_min=ylim_min)

      # Next we process each of the doses   
      dosenum_all = unique(SUBDS[["SI_DOSENUM"]])
      #
      # JMH adding sparse stuff here:
      #
      for(dosenum in dosenum_all){
        # this is subject or group data for the given dose number
        dosenum_str = paste("dose_", dosenum, sep="")

        # This contains all of the rows for the current dose number
        TMP_SS_DN  = SUBDS[SUBDS[["SI_DOSENUM"]] == dosenum, ]


        # If this is a sparse sampling analysis we remove redundant time
        # points so we have one concentration per time point for the current
        # dose number
        if(sparse){
          SUBDS_DN = NULL
          # For each unique time we pull of the first row
          for(TIME in sort(unique(TMP_SS_DN[[dsmap[["TIME"]]]]))){
            if(is.null(SUBDS_DN)){
              SUBDS_DN = TMP_SS_DN[TMP_SS_DN[[dsmap[["TIME"]]]] == TIME, ][1,]
            } else {
              SUBDS_DN = rbind(SUBDS_DN, TMP_SS_DN[TMP_SS_DN[[dsmap[["TIME"]]]] == TIME, ][1,])
            }
          }
        } else {
          # Otherwise we just use all of the rows:
          SUBDS_DN = TMP_SS_DN
        }


        # By default we process the current subject/dose combination
        PROC_SUBDN = TRUE

        # pulling out the route for the subject/group
        ROUTE = SUBDS_DN[[dsmap[["ROUTE"]]]][1]

        # But we check a few things first:
        # Checking to make sure dose is unique
        if(length(unique(SUBDS_DN[[dsmap[["DOSE"]]]]))>1){
          PROC_SUBDN = FALSE
          vp(cfg, paste(ID_label, ": >", sub, "< Dose ", dosenum, " had more than 1 value in the dose column",sep=""))
          vp(cfg, paste("    Dose column >", dsmap[["DOSE"]], "< has values: ", paste(unique(SUBDS_DN[[dsmap[["DOSE"]]]]), collapse=", "), sep=""))
        }

        # Make sure there are enough observations:
        if(nrow(SUBDS_DN) < NCA_min){
          PROC_SUBDN = FALSE
          vp(cfg, paste(ID_label, ": >", sub, "< Dose ", dosenum, " had less than ", NCA_min, " observations (NCA_min)",sep=""))
        }

        # This will hold the NCA summary information for the current
        # subject/dose subset
        if(PROC_SUBDN){
          tmpsum = list()
          
          # Tmax and Cmax are taken directly from the dataset. The min() below
          # selects the first time that Cmax is observed if there are multiple
          # occurrences of the Cmax
          Cmax            = max(SUBDS_DN[["SI_CONC"]])
          Tmax            = min(SUBDS_DN[SUBDS_DN[["SI_CONC"]] == Cmax, ][[dsmap[["NTIME"]]]])

          # Finding the predose conc 
          # By default it's zero:
          PREDOSE_CONC = 0.0
          # first we look for observations with time values before the first
          # observation of the current subset
          if(any(SUBDS[[dsmap[["TIME"]]]] < min(SUBDS_DN[[dsmap[["TIME"]]]]))){
            # This gets the subject dataset leading up to the current subset
            PREDOSEDS = SUBDS[SUBDS[[dsmap[["TIME"]]]] < min(SUBDS_DN[[dsmap[["TIME"]]]]), ]

            # Pulling out the values at the last time point
            PREDOSEDS = PREDOSEDS[PREDOSEDS[[dsmap[["TIME"]]]] == max(PREDOSEDS[[dsmap[["TIME"]]]]), ]

            # Now we pluck off the last value:
            PREDOSE_CONC = PREDOSEDS[nrow(PREDOSEDS), ][["SI_CONC"]]
          }

          # The nominal time of this point will be 0, but in a multiple dose
          # setting the clock time will be different:
          C0_NTIME = 0
          C0_TIME  = SUBDS_DN[[dsmap[["TIME"]]]][1] - SUBDS_DN[[dsmap[["NTIME"]]]][1]
          BACKEXTRAP_NTIME = NULL
          BACKEXTRAP_TIME  = NULL
          BACKEXTRAP_CONC  = NULL
          
          # Extrapolating C0 if extrapolation has been selected and the first
          # nominal time is not zero
          if(extrap_C0 & SUBDS_DN[[dsmap[["NTIME"]]]][1] != 0){

            if(ROUTE %in% c("iv bolus")){
              if(is.null(dsmap[["BACKEXTRAP"]])){
                BACKEXTRAP_N     = extrap_N
              } else {
                # Using subjects-specific number of points to extrapolate
                BACKEXTRAP_N     = SUBDS_DN[[dsmap[["BACKEXTRAP"]]]][1]
              }
              # Time, nominal time and concentrations sequences used for
              # extrapolation
              BACKEXTRAP_NTIME = SUBDS_DN[[dsmap[["NTIME"]]]][1:BACKEXTRAP_N]
              BACKEXTRAP_TIME  = SUBDS_DN[[dsmap[["TIME"]]]] [1:BACKEXTRAP_N]
              BACKEXTRAP_CONC  = SUBDS_DN[["SI_CONC"]]       [1:BACKEXTRAP_N]

              # This does least squares fitting of the ln of the concentration
              # data:
              BACKEXTRAP_TH    = calculate_halflife(BACKEXTRAP_NTIME, BACKEXTRAP_CONC)

              # Pulling out the slope and intercept:
              BACKEXTRAP_SLOPE     = as.numeric(BACKEXTRAP_TH[["mod"]][["coefficients"]][2])
              BACKEXTRAP_INTERCEPT = as.numeric(BACKEXTRAP_TH[["mod"]][["coefficients"]][1])

              if(BACKEXTRAP_SLOPE < 0){
                # Because we're using nominal time to perform the regression the
                # intercept is the natural log of the C0:
                C0 = exp(BACKEXTRAP_INTERCEPT)
              } else {
                C0 = BACKEXTRAP_CONC[1]
              }
            }
            if(ROUTE %in% c("iv infusion", "extra-vascular")){
              # Here we set the C0 value to the PREDOSE_CONC calculated above:
              C0 = PREDOSE_CONC
            }
          } else {
            # Otherwise we return NA for C0 
            C0 = NA
          }
          
          # This defines the standard output
          tmpsum[["ID"]]              = sub
          tmpsum[["Nobs"]]            = nrow(SUBDS_DN)
          tmpsum[["Dose_Number"]]     = dosenum
          tmpsum[["Dose"]]            = SUBDS_DN[[dsmap[["DOSE"]]]][1]
          tmpsum[["Dose_CU"]]         = SUBDS_DN[["SI_DOSE"]][1]
          tmpsum[["cmax"]]            = Cmax
          tmpsum[["tmax"]]            = Tmax 
          tmpsum[["half.life"]]       = NA
          tmpsum[["Vp_obs"]]          = NA
          tmpsum[["vss.obs"]]         = NA
          tmpsum[["vss.pred"]]        = NA
          tmpsum[["C0"]]              = C0  
          tmpsum[["cl.obs"]]          = NA
          tmpsum[["cl.pred"]]         = NA
          tmpsum[["auclast"]]         = NA
          tmpsum[["aucinf.pred"]]     = NA
          tmpsum[["aucinf.obs"]]      = NA

          # If we're performing a sparse analysis we add the elements 
          # to hold the results from Bailer's analysis
          if(sparse){
            tmpsum[["AUCBailer"]]       = NA
            tmpsum[["AUCBailer_var"]]   = NA
          }


          if(sparse){
            # Performing sparse analysis using Bailers method 
            # The data frame used here is TMP_SS_DN which is all of the data
            # for the current dose number 
            res_Bailers  =  AUC_Bailers_method(conc_data  = TMP_SS_DN, 
                                               dsmap      = list(NTIME       = dsmap[["NTIME"]],
                                                                 CONC        = dsmap[["CONC"]], 
                                                                 ID          = dsmap[["ID"]]))
            # Appending the results to the summary table
            if(res_Bailers[["isgood"]]){
              tmpsum[["AUCBailer"]]       = res_Bailers[["AUC"]]
              tmpsum[["AUCBailer_var"]]   = res_Bailers[["var_AUC"]]
            }

          }
          # Creating data frames for NCA
          if(extrap_C0){
            # If we have extrapolation selected we add the first time point to
            # the NCA dataset:
            NCA_CONCDS = data.frame(NTIME =   c(C0_NTIME, SUBDS_DN[[dsmap[["NTIME"]]]]),
                                    TIME  =   c(C0_TIME,  SUBDS_DN[[dsmap[["TIME"]]]]),
                                    CONC  =   c(C0,       SUBDS_DN[["SI_CONC"]]),
                                    ID    = sub)
          } else {
            # Otherwise we just use the data from the dataframe:
            NCA_CONCDS = data.frame(NTIME =   SUBDS_DN[[dsmap[["NTIME"]]]],
                                    TIME  =   SUBDS_DN[[dsmap[["TIME"]]]],
                                    CONC  =   SUBDS_DN[["SI_CONC"]],
                                    ID    = sub)
          }
          NCA_DOSEDS = data.frame(NTIME =   min(NCA_CONCDS[["NTIME"]]),
                                  DOSE  = SUBDS_DN[["SI_DOSE"]][1],
                                  ID    = sub)

          # Calculating the observed plasma concentration 
          #
          #                Dose in conc units
          # Vp_obs = ------------------------------
          #           Corrected first observed conc
          if(ROUTE %in% c("iv bolus")){
            Vp_obs = SUBDS_DN[["SI_DOSE"]][1]/(SUBDS_DN[["SI_CONC"]][1])
          } else {
            Vp_obs = NA
          }

          time_start = min(NCA_CONCDS[["NTIME"]]) 
          time_stop  = max(NCA_CONCDS[["NTIME"]])

          # Checking for duplicated times
          if(any(duplicated(NCA_CONCDS[["NTIME"]]))){
            vp(cfg, paste(ID_label, ": >", sub, "< Dose ", dosenum, " the following time values were repeated", sep="")) 
            vp(cfg, paste("NTIME: ", unique(NCA_CONCDS[duplicated(NCA_CONCDS[["NTIME"]]), ][["NTIME"]]), " (nominal time, ",  dsmap[["NTIME"]], " in the dataset)", sep=""))
            vp(cfg, paste("TIME:  ", unique(NCA_CONCDS[duplicated(NCA_CONCDS[["NTIME"]]), ][["NTIME"]]), " (actual time, ",   dsmap[["TIME"]],  " in the dataset)", sep=""))
            vp(cfg, "This can happen when:")
            vp(cfg, "  - If you are using back extrapolation to time zero and you have data at time zero")
            vp(cfg, "  - If you have mulitple analytes measured at the same time points. You can use the dsfilter to run NCA on these analytes separately.")
            # Skipping the subject/dose number
            PROC_SUBDN = FALSE
          }

          # These are the default inputs that must be true
          PKNCA_outputs = c("half.life",    "aucall",     "auclast",   "vss.obs",   
                            "vss.pred",     "cl.pred",    "cl.obs",    "aucinf.pred",
                            "aucinf.obs")

          # Creating intervals for PKNCA
          PKNCA_intervals = data.frame(start = time_start, end=time_stop)
          for(PKNCA_output in PKNCA_outputs){
            PKNCA_intervals[[PKNCA_output]] = TRUE
          }
          # Now we set cmax and tmax to FALSE because those are calculated
          # based on the raw observed data above and may be incorrect because
          # we could be sending extrapolated C0 data into PKNCA
          PKNCA_intervals[["cmax"]] = FALSE
          PKNCA_intervals[["tmax"]] = FALSE
          
          if(PROC_SUBDN){
            NCA.conc = PKNCA::PKNCAconc(NCA_CONCDS, CONC~NTIME|ID)
            NCA.dose = PKNCA::PKNCAdose(NCA_DOSEDS, DOSE~NTIME|ID)
            NCA.data = PKNCA::PKNCAdata(data.conc = NCA.conc,
                                        data.dose = NCA.dose,
                                        intervals = PKNCA_intervals)
            NCA.res =  PKNCA::pk.nca(NCA.data)

            # Packing all of the outputs into the temporary dataframe
            for(PKNCA_output in unique(NCA.res$result$PPTESTCD)){
              # For some reason when tmax is set to false it still returns it
              # so we explicitly skip those outputs here:
              if(!(PKNCA_output %in% c("cmax", "tmax"))){
                tmpsum[[PKNCA_output]] =  NCA.res$result[NCA.res$result$PPTESTCD == PKNCA_output,   ]$PPORRES
              }
            }
            # Adding Vp_obs
            tmpsum$Vp_obs        =  Vp_obs

            # Pulling out the parameter meta data
            NCA_pmeta = cfg[["options"]][["nca_meta"]][["parameters"]]

            # Getting the meta data for nca parameters
            cfg[["options"]][["nca_meta"]]
            
            # Storing the raw results
            PKNCA_raw_tmp              = NCA.res$result
            PKNCA_raw_tmp[["sub"]]     = sub
            PKNCA_raw_tmp[["dosenum"]] = dosenum
            
            if(is.null(PKNCA_raw_all)){
               PKNCA_raw_all = PKNCA_raw_tmp
            } else {
               PKNCA_raw_all = rbind(PKNCA_raw_all,  PKNCA_raw_tmp)
            }
            
            # Summarizing everything for the current subject/dose to be used in
            # report generation later
            # These are used in PowerPoint
            lctmp = c(1, paste(NCA_pmeta[["Nobs"]][["label"]],": "        , var2string(tmpsum$Nobs           , nsig_e=2, nsig_f=0), sep=""),
                      1, paste(NCA_pmeta[["Dose"]][["label"]],": "        , var2string(tmpsum$Dose           , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["Dose_CU"]][["label"]],": "     , var2string(tmpsum$Dose_CU        , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["cmax"]][["label"]],": "        , var2string(tmpsum$cmax           , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["C0"]][["label"]],": "          , var2string(tmpsum$C0             , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["tmax"]][["label"]],": "        , var2string(tmpsum$tmax           , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["half.life"]][["label"]],": "   , var2string(tmpsum$half.life      , nsig_e=2, nsig_f=2), sep=""),
                      1, paste("Time interval: "                          , toString(time_start), '-', toString(time_stop))) 
            rctmp = c(1, paste(NCA_pmeta[["Vp_obs"]][["label"]],": "      , var2string(tmpsum$Vp_obs         , nsig_e=2, nsig_f=2), sep=""),
                      1, paste(NCA_pmeta[["vss.obs"]][["label"]],": "     , var2string(tmpsum$vss.obs        , nsig_e=2, nsig_f=2), sep=""),
                      1, paste(NCA_pmeta[["vss.pred"]][["label"]],": "    , var2string(tmpsum$vss.pred       , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["cl.obs"]][["label"]],": "      , var2string(tmpsum$cl.obs         , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["cl.pred"]][["label"]],": "     , var2string(tmpsum$cl.pred        , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["auclast"]][["label"]],": "     , var2string(tmpsum$auclast        , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["aucinf.pred"]][["label"]],": " , var2string(tmpsum$aucinf.pred    , nsig_e=2, nsig_f=2), sep=""), 
                      1, paste(NCA_pmeta[["aucinf.obs"]][["label"]],": "  , var2string(tmpsum$aucinf.obs     , nsig_e=2, nsig_f=2), sep=""))

            if(sparse){                                        
              lctmp = c(lctmp, 1,  paste(NCA_pmeta[["AUCBailer"]][["label"]],": "        , var2string(tmpsum$AUCBailer      , nsig_e=2, nsig_f=2), sep=""))
              rctmp = c(rctmp, 1,  paste(NCA_pmeta[["AUCBailer_var"]][["label"]],": "    , var2string(tmpsum$AUCBailer_var  , nsig_e=2, nsig_f=2), sep=""))
            }

            # Generic tabular content for Word reporting
            all = data.frame(c1=matrix(ncol=2, data=lctmp, byrow=TRUE)[,2], c2=matrix(ncol=2, data=rctmp, byrow=TRUE)[,2])
            
            # storing the actual values to be used in the reporting
            rptobjs[[sub_str]][[dosenum_str]]$dosenum = dosenum
            rptobjs[[sub_str]][[dosenum_str]]$sub     = sub    
            rptobjs[[sub_str]][[dosenum_str]]$lc      = lctmp
            rptobjs[[sub_str]][[dosenum_str]]$rc      = rctmp
            rptobjs[[sub_str]][[dosenum_str]]$all     = all

            # adding pass-through columns
            if(!is.null(dsinc)){
              for(DSCOL in dsinc){
                tmpsum[[DSCOL]] = SUBDS_DN[[DSCOL]][1]
              }
            }
            
            tmpsum = as.data.frame(tmpsum)
            if(is.null(NCA_sum)){
               NCA_sum = tmpsum
            } else {
               NCA_sum = rbind(tmpsum, NCA_sum)
            }
            
            
            # Creating shaded region:
            ptmp = eval(parse(text="ptmp + geom_ribbon(data=NCA_CONCDS, aes(x=TIME, ymax=CONC), ymin=0, color=NA, fill='green', alpha=.09)"))
            # Overlaying the concentration values used
            ptmp = eval(parse(text="ptmp + geom_point(data=NCA_CONCDS, aes(x=TIME, y=CONC), shape=1,           color='green')"))
            ptmp = eval(parse(text="ptmp +  geom_line(data=NCA_CONCDS, aes(x=TIME, y=CONC), linetype='dashed', color='green')"))
            
            # Adding extrapolation information
            if(extrap_C0){
              # Showing extrapolation points and line:
              if(!is.null(BACKEXTRAP_TIME) & !is.null(BACKEXTRAP_CONC)){
                BACKEXTRAP_DF  = data.frame(TIME=c(C0_TIME, BACKEXTRAP_TIME), CONC=c(C0, BACKEXTRAP_CONC))
                ptmp = eval(parse(text="ptmp + geom_point(data=BACKEXTRAP_DF, aes(x=TIME, y=CONC), shape   =1,      , color='orange')"))
                ptmp = eval(parse(text="ptmp +  geom_line(data=BACKEXTRAP_DF, aes(x=TIME, y=CONC), linetype='dotted', color='orange')"))
              } 
            
              # Showing C0 with a solid point
              BACKEXTRAP_DF  = data.frame(TIME=c(C0_TIME), CONC=c(C0))
              ptmp = eval(parse(text="ptmp + geom_point(data=BACKEXTRAP_DF, aes(x=TIME, y=CONC), shape   =16,      , color='orange')"))
            
            }
          }
        } 
        if(!PROC_SUBDN){
          vp(cfg, "Skipping this subject/dose combination")
        }
      }

      # Adding PK plot here
      grobs_sum[[sub_str]] = ptmp
    }

    # Sorting the NCA table by ID then Dose_Number
    # If NCA_sum is null then something is up
    if(is.null(NCA_sum)){
      cfg[["nca"]][[analysis_name]] = NULL
      vp(cfg, paste("NCA for ", analysis_name, " failed. This can happen when none of the subjects ", sep=""))
      vp(cfg, paste("or groups have enough data for NCA_min", sep=""))

    } else {
      NCA_sum = NCA_sum[ with(NCA_sum, order(Dose_Number, ID)), ]
      
      pkncaraw_file  = file.path(output_directory, paste(analysis_name, "-pknca_raw.csv" , sep=""))
      csv_file       = file.path(output_directory, paste(analysis_name, "-nca_summary-pknca.csv" , sep=""))
      data_file      = file.path(output_directory, paste(analysis_name, "-nca_data.RData" , sep=""))
      write.csv(NCA_sum,       file=csv_file,      row.names=FALSE, quote=FALSE)
      write.csv(PKNCA_raw_all, file=pkncaraw_file, row.names=FALSE, quote=FALSE)
      save(grobs_sum, NCA_sum, file=data_file)
      
      cfg[["nca"]][[analysis_name]]$grobs_sum     = grobs_sum
      cfg[["nca"]][[analysis_name]]$NCA_sum       = NCA_sum
      cfg[["nca"]][[analysis_name]]$data_raw      = DS
      cfg[["nca"]][[analysis_name]]$PKNCA_raw     = PKNCA_raw_all
      cfg[["nca"]][[analysis_name]]$rptobjs       = rptobjs      
      
      vp(cfg, paste("NCA results for ", analysis_name, " written to", sep=""))
      vp(cfg, paste("  Summary output:   ", csv_file,      sep=""))
      vp(cfg, paste("  R objects:        ", data_file,     sep=""))
      vp(cfg, paste("  PKNCA raw output: ", pkncaraw_file, sep=""))
    }
  } else {
     vp(cfg, "ubiquity::system_nca_run()")
     vp(cfg, "Errors were found see messages above for more information")
  }


cfg}
#-------------------------------------------------------------------------
#'@export 
#'@title Summarize NCA Results in Tabular Format
#'@description Creates tabular summaries of NCA results
#'
#'@param cfg ubiquity system object
#'@param analysis_name string containing the name of the analysis (default \code{'analysis'}) that was previously run
#'@param treat_as_factor sequence of column names to be treated as factors (default \code{c("ID", "Dose_Number", "Dose")}). Use this to report values without added decimals. 
#'@param params_include vector with names of parameters to include (default c("ID", "cmax", "tmax", "auclast"))
#'@param params_header  list with names of parameters followed by a vector of headers. You can use the placeholder "<label>" to include the standard label (e.g. list(cmax=c("<label>", "(ng/ml)"))), with a default of \code{NULL}.
#'@param label_format string containing the format in which headers and labels are being specified, either \code{"text"}, or \code{"md"} (default \code{NULL} assumes \code{"text"} format)
#'@param rptname report name (either PowerPoint or Word) that this table will be used in (\code{"default"})
#'@param summary_stats list with strings as names containing placeholders for
#' summary statistics and the values indicate the parameters to apply those
#' statistics to. for example, if you want to calculate mean and standard deviation of
#' AUClast you could use \code{list("<MEAN> (<STD>)"=c("auclast")}. This would create
#' a row at the bottom of the table with this information for just the listed
#' parameters. To split this up across two rows just do the following:
#' \code{list("<MEAN>"=c("auclast"), "<STD>"=c("auclast"))}. Any NA values
#' will be ignored when calculating statistics.  The allowed
#' summary statistics are the mean (<MEAN>), median (<MEDIAN>), standard
#' deviation (<STD>), standard error (<SE>), and the number of observations
#' used to calculate statistics. (<N>). The default value of \code{NULL}
#' prevents any summary statistics from being included.
#'@param summary_labels list containing the mapping of summary statistics
#' defined by \code{summary_stats} with their text labels in the output tables: 
#' \preformatted{
#' list(MEAN   = "Mean", 
#'      STD    = "Std Dev", 
#'      MEDIAN = "Median", 
#'      N      = "N obs", 
#'      SE     = "Std Err.")}
#'@param summary_location column where to put the labels (e.g. Mean (Std)) for
#' summary statistic. The default (\code{NULL}) will leave these labels off.
#' If you set this to the "ID" column it will put them under the subject IDs.
#'@param digits number of significant digits to report (3) or \code{NULL} to prevent rounding
#'@param ds_wrangle 
#'\preformatted{
#'  ds_wrangle = list(Dose=c(30), Dose_Number = c(1))
#'}
#'@return list with the following elements
#' \itemize{
#'   \item{isgood} Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE}) if the call is successful the following will be defined (\code{NULL} 
#'   \item{nca_summary} dataframe containing the summary table with headers and any summary statistics appended to the bottom
#'   \item{nca_summary_ft} same information in the \code{nca_summary} ouput as a flextable object
#'   \item{components}  list with the elements of the summary table each as dataframes (header, data, and summary)
#' }
#'@param table_theme flextable theme see the flextable package for available themes, and set to \code{NULL} to prevent themes from being applied. (default=\code{"theme_zebra"})
#'@seealso Vignette on NCA (\code{vignette("NCA", package = "ubiquity")}) 
system_nca_summary = function(cfg, 
                          analysis_name     = "analysis",
                          treat_as_factor   = c("ID", "Dose_Number", "Dose"),
                          params_include    = c("ID", "cmax", "tmax", "auclast"),
                          params_header     = NULL,
                          rptname           = "default",
                          label_format      = NULL,
                          summary_stats     = NULL,
                          summary_labels    = list(MEAN   = "Mean", 
                                                   STD    = "Std Dev", 
                                                   MEDIAN = "Median", 
                                                   N      = "N obs", 
                                                   SE     = "Std Err."),
                          summary_location  = NULL, 
                          ds_wrangle        = NULL,
                          digits            = 3,
                          table_theme       = "theme_zebra"
                          ){

invisible(system_req("magrittr"))
invisible(system_req("dplyr"))
invisible(system_req("flextable"))
# Setting defaults for the function
isgood        = TRUE
echo_nca_cols = FALSE      # This is used to show the columns of the NCA if the user has specified columns that do not exist
nca_all       = NULL       # NCA results for the specified analysis

rows_header  = NULL
rows_data    = NULL
rows_summary = NULL
sum_table    = NULL
sum_table_ft = NULL

# Making sure label_format has a value
if(is.null(label_format)){
  label_format = "text"
}

if((analysis_name %in% names(cfg[["nca"]]))){
  NCA_all       = cfg[["nca"]][[analysis_name]]
  NCA_sum       = NCA_all[["NCA_sum"]]
  # inheriting different aspects of the analysis
  dsmap  = NCA_all[["ana_opts"]][["dsmap"]]
  if(!is.null(ds_wrangle)){
    # Truing to evaluate the user specified data wrangling code:
    tcres = tryCatch(
     { 
        eval(parse(text=ds_wrangle))
      list(NCA_sum=NCA_sum, isgood=TRUE)},
      error = function(e) {
      list(value=e, isgood=FALSE)})
    }

   # Capturing results or errors:
   if(tcres$isgood){
     NCA_sum = NCA_sum
   } else {
     isgood = FALSE
     vp(cfg, "Error evaluating ds_wrangle option:")
     # This should push the actual error message out to the user:
     vp(cfg, tcres$value$message)
   }

} else {
  isgood = FALSE
  vp(cfg, paste("The NCA analysis >", analysis_name, "< was not found", sep=""))
}

if(is.null(params_include)){
  isgood = FALSE
  vp(cfg, "The input params_include is NULL, it needs to be a list. For example")
  vp(cfg, "if you want to include the half-life and and label it t1/2 you ")
  vp(cfg, "could do the following: ")
  vp(cfg, 'params_include = list(halflife="t1/2")')
} else {
  if(isgood){
    # Checking to make sure the included parameters are actually included
    # in the NCA output
    if(!all(params_include %in%  names(NCA_sum))){
      isgood        = FALSE
      echo_nca_cols = TRUE
      vp(cfg, paste("The following parameter(s) were found in params_include but were not found in the NCA output:"))
      vp(cfg, paste(params_include[!(params_include %in%  names(NCA_sum))], collapse=", "))
    }
  }
}


if(!is.null(params_header)){
  if(!all(names(params_header)  %in%  names(NCA_sum))){
    isgood        = FALSE
    echo_nca_cols = TRUE
    vp(cfg, paste("The following parameter(s) were found in params_header but were not found in the NCA output:"))
    vp(cfg, paste(names(params_header)[!(names(params_header) %in%  names(NCA_sum))], collapse=", "))
  }
}

if(!is.null(treat_as_factor)){
  if(!all(treat_as_factor  %in%  names(cfg[["options"]][["nca_meta"]][["parameters"]]))){
    isgood        = FALSE
    echo_nca_cols = TRUE
    vp(cfg, paste("The following parameter(s) were found in treat_as_factor but are not valid NCA outputs:"))
    vp(cfg, paste(treat_as_factor[!(treat_as_factor %in%  names(cfg[["options"]][["nca_meta"]][["parameters"]]))], collapse=", "))
  } else {
    for(cn in treat_as_factor){
      if(cn %in% names(NCA_sum)){
        NCA_sum[[cn]] = as.factor(NCA_sum[[cn]])
      }
    }
  }
}

if(!is.null(summary_stats)){
  # First we pass through summary statistics and check:
  #  - that the specified outputs exist
  for(summary_stat in names(summary_stats)){
    if(!all(summary_stats[[summary_stat]] %in% names(NCA_sum))){
      isgood        = FALSE
      echo_nca_cols = TRUE
      vp(cfg, paste("For the summary statistic >", summary_stat, "< the following columns", sep=""))
      vp(cfg, paste("are listed but not present in the NCA analysis: ", sep=""))
      vp(cfg, paste(summary_stats[[summary_stat]][!(summary_stats[[summary_stat]] %in% names(NCA_sum))], collapse = ", "))
    }
  }
}






# If all of the checks above have passed then we can start building the
# table
if(isgood){
  #------------------------------------------
  # Defining headers:
  # maximum number of headers defaults to 1
  max_head = 1
  for(pname in params_include){
    # Current parameter label
    if(is.null(cfg[["options"]][["nca_meta"]][["parameters"]][[pname]][["label"]])){
      # Passthrough parameters will not have labels so those default to the
      # column names. Users will have to provide those headers explicitly
      plabel = pname
    } else {
      if(label_format == "md"){
        plabel = c(cfg[["options"]][["nca_meta"]][["parameters"]][[pname]][["md"]])
      } else {
        plabel = c(cfg[["options"]][["nca_meta"]][["parameters"]][[pname]][["label"]])
      }
    }
    # If a parameter isn't mentioned in the header variable then we populate the
    # header for that parameter with the default label
    if(!(pname %in% names(params_header))){
      params_header[[pname]] = c("<label>")
    }
    # Substituting placeholders
    for(hidx in 1:length(params_header[[pname]])){
      params_header[[pname]][hidx] =  gsub(pattern="<label>", replacement=plabel, params_header[[pname]][hidx])
    }
    # Getting the maximum header length
    max_head = max(length(params_header[[pname]]), max_head)
  }

  #------------------------------------------
  # Filtering out only the columns we want to keep:
  rows_data  = NCA_sum[, params_include]

  #------------------------------------------
  # Now we construct the header data frame:
  rows_header = NULL
  for(pname in params_include){
    # Padding headers
    if((max_head - length(params_header[[pname]])) > 0){
      params_header[[pname]] = c(params_header[[pname]], rep("", (max_head - length(params_header[[pname]]))))
    }
    rows_header[[pname]] = params_header[[pname]] 
  }
  rows_header = as.data.frame(rows_header)
  


  #------------------------------------------
  # We the construct any summary statistics
  if(!is.null(summary_stats)){
    # first we create an empty data frame with the same heading structure as
    # the rows_data data frame
    rows_summary = rows_data[0,]

    # Summary statistics will be stored as character values:
    rows_summary = data.frame(lapply(rows_summary, as.character), stringsAsFactors=FALSE)

    for(summary_stat in names(summary_stats)){
      # Appending a row at a time:
      ridx = nrow(rows_summary) + 1

      # Creating an empty row:
      rows_summary[ridx,] = "" 

      # First we update the label for this row
      if(!is.null(summary_location)){
        sstemp = summary_stat
        if(!is.null(summary_labels)){
          for(sname in names(summary_labels)){
            sstemp = gsub(paste("<", sname, ">",sep=""), summary_labels[[sname]], sstemp)  
          }
        }
        rows_summary[ridx,][[summary_location]] = sstemp
      }  

      # For each parameter in summary statistics we create the template
      # then substitute the summary statistics
      for(pname in summary_stats[[summary_stat]]){
        pdata   = rows_data[[pname]][!is.na(rows_data[[pname]])]
        pss_all = data.frame(MEAN     =   mean(pdata),
                             MEDIAN   = median(pdata),
                             N        = length(pdata),
                             SE       =     sd(pdata)/length(pdata),
                             STD      =     sd(pdata))
              
        sstemp = summary_stat
        for(pss in names(pss_all)){
          pss_val = pss_all[[pss]]
          # Applying any significant digits
          if(!is.null(digits)){
            pss_val = signif(pss_val, digits)
          }

          # converting to a string:
          pss_val = toString(pss_val)

          # JMH convert to scientific notation?

          # Substituting statistics: 
          sstemp = gsub(paste("<", pss, ">",sep=""), pss_val, sstemp)  
        }
        # placing the summary statistics in the correct columns
        rows_summary[ridx,][[pname]] = sstemp
      }
    }
  }

  #------------------------------------------
  # We apply rounding/significant figures
  if(!is.null(digits)){
    for(pname in names(rows_data)){
      if(is.numeric(rows_data[[pname]])){
        rows_data[[pname]] = signif(rows_data[[pname]], digits)
      }
    }
  }
  #------------------------------------------
  # Now we stack everything together
  for(pname in names(rows_data)){
    tmpcol = NULL
    if(!is.null(rows_header)){
      tmpcol = c(tmpcol, as.character(rows_header[[pname]]))
    }
    tmpcol = c(tmpcol, as.character(rows_data[[pname]]))
    if(!is.null(rows_summary)){
      tmpcol = c(tmpcol, as.character(rows_summary[[pname]]))
    }
    sum_table[[pname]] = tmpcol
  }
  sum_table = as.data.frame(sum_table)
  #------------------------------------------
  # Creating the flextable object
  sum_table_ft = 
       flextable::flextable(rows_data)                       %>% 
       flextable::delete_part(part = "header")               %>%
       flextable::add_header(values =as.list(rows_header))  

  # If the user specified a summary row we add that here:
  if(!is.null(summary_stats)){
    sum_table_ft = sum_table_ft %>% flextable::add_footer(values =as.list(rows_summary)) 
  } 
  if(!is.null(table_theme)){
    eval(parse(text=paste("sum_table_ft = sum_table_ft %>% flextable::", table_theme, "()", sep="")))
  }
  #------------------------------------------
  # Applying markdown formatting
  if(label_format == "md"){
     # Pulling out the default format for the Table element. 
     # First we get the onbrand object:
     obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)
     default_format_table = onbrand::fetch_md_def(obnd, 'Table_Labels')$md_def
     # Applying markdown to headers
     if(!is.null(rows_header)){
       for(pname in names(rows_header)){
          sum_table_ft = flextable::compose(sum_table_ft,
                            j     = pname,                                                    
                            part  = "header",                                                          
                            value = onbrand::md_to_oo(strs= rows_header[[pname]], default_format=default_format_table)$oo)
       }
     }

     # Applying markdown to footers
     if(!is.null(summary_stats)){
       for(pname in names(rows_summary)){
          sum_table_ft = flextable::compose(sum_table_ft,
                            j     = pname,                                                    
                            part  = "footer",                                                          
                            value = onbrand::md_to_oo(strs= rows_summary[[pname]], default_format=default_format_table)$oo)
       }
     }
  }

  sum_table_ft = flextable::autofit(sum_table_ft)
  #------------------------------------------
}


# If we fil we drop an error indicating the function we died in:
if(!isgood){
  if(echo_nca_cols){
    vp(cfg, paste("To view the available NCA outputs for different analyses you can run the following:"))
    vp(cfg, paste('ubiquity::system_view(cfg, "nca", verbose=TRUE)'))
  }
  vp(cfg, "ubiquity::system_nca_summary()")
  vp(cfg, "Errors were found see messages above for more information")
}
res = list(isgood         = isgood,
           nca_summary    = sum_table,
           nca_summary_ft = sum_table_ft,
           components  = list(header  = rows_header,
                              data    = rows_data,
                              summary = rows_summary))

res}


#-------------------------------------------------------------------------
#'@export 
#'@title List NCA parameters, text names and descriptions
#'@description Provides a verbose information about NCA parameters 
#'
#'@param cfg ubiquity system object
#'@return List with the following elements:
#'
#' \itemize{
#'   \item \code{isgood} Boolean value indicating the success of the function call.
#'   \item \code{parameters} List with element names for each standard column header for NCA output. Each element name is a list with the following elements:
#'   \itemize{
#'     \item \code{label} Textual descriptor of the parameter.
#'     \item \code{description} Verbose description of the parameter.
#'     \item \code{from} Text indicating the source of the parameter (either PKNCA or ubiquity).
#'     }
#'   }
#'@seealso Vignette on NCA (\code{vignette("NCA", package = "ubiquity")}) 
system_nca_parameters_meta  = function(cfg){
   

isgood = TRUE

# Since almost all of the parameters come from PKNCA we start by labeling 

# The following outputs from PKNCA were skipped because they seem to be repeats
# of previous outputs
# "aucint.inf.obs"       
# "aucint.inf.obs.dose" 
# "aucint.inf.pred"      
# "aucint.inf.pred.dose" 
# "aucinf.obs.dn"       
# "aucinf.pred.dn"       

res_PKNCA = list(
   auclast                 = list(label = "AUC last"               ,  md = "AUC~last~"                            ),
   aucall                  = list(label = "AUC all"                ,  md = "AUC~all~"                             ),
   aumclast                = list(label = "AUMC last"              ,  md = "AUMC~last~"                           ),
   aumcall                 = list(label = "AUMC all"               ,  md = "AUMC~all~"                            ),
   aumcint.last            = list(label = "AUMC last (interval)"   ,  md = "AUMC~last (interval)~"                ),
   aumcint.last.dose       = list(label = "AUMC last (dose)"       ,  md = "AUMC~last (dose)~"                    ),
   aumcint.all             = list(label = "AUMC all"               ,  md = "AUMC~all~"                            ),
   aumcint.all.dose        = list(label = "AUMC all (dose)"        ,  md = "AUMC~all (dose)~"                     ),
   auclast.dn              = list(label = "AUC last/Dose"          ,  md = "AUC~last~/Dose"                       ),
   aucall.dn               = list(label = "AUC all/Dose"           ,  md = "AUC~all~/Dose"                        ),
   aumclast.dn             = list(label = "AUMC last/Dose"         ,  md = "AUMC~last~/Dose"                      ),
   aumcall.dn              = list(label = "AUMC all/Dose"          ,  md = "AUMC~all~/Dose"                       ),
   tmax                    = list(label = "Tmax"                   ,  md = "T~max~"                               ),
   tlast                   = list(label = "Tlast"                  ,  md = "T~last~"                              ),
   tfirst                  = list(label = "Tfirst"                 ,  md = "T~first~"                             ),
   clast.obs               = list(label = "C (last)"               ,  md = "C~last~"                              ),
   cl.last                 = list(label = "CL (last)"              ,  md = "CL~last~"                             ),
   f                       = list(label = "Fbio"                   ,  md = "F~b~"                                 ),
   mrt.last                = list(label = "MRT (last)"             ,  md = "MRT~last~"                            ),
   mrt.iv.last             = list(label = "MRT (IV, last)"         ,  md = "MRT~last,IV~"                         ),
   vss.last                = list(label = "Vss"                    ,  md = "V~ss~"                                ),
   vss.iv.last             = list(label = "Vss IV"                 ,  md = "V~ss,IV~"                             ),
   cav                     = list(label = "Cave"                   ,  md = "C~ave~"                               ),
   ctrough                 = list(label = "Ctr"                    ,  md = "C~tr~"                                ),
   ptr                     = list(label = "Peak/Trough"            ,  md = "Peak/Trough"                          ),
   tlag                    = list(label = "Tlag"                   ,  md = "T~lag~"                               ), 
   deg.fluc                = list(label = "Fluctuation"            ,  md = "Fluctuation"                          ), 
   swing                   = list(label = "Cmin Swing"             ,  md = "C~min,Swing~"                         ), 
   ceoi                    = list(label = "C (EOI)"                ,  md = "C~EOI~"                               ), 
   ae                      = list(label = "Ex (amt)"               ,  md = "Ex~amt~"                              ), 
   clr.last                = list(label = "CL (R,last)"            ,  md = "CL~R,last~"                           ), 
   clr.obs                 = list(label = "CL (R,obs)"             ,  md = "CL~R,obs~"                            ), 
   clr.pred                = list(label = "CL (R,pred)"            ,  md = "CL~R,pred~"                           ), 
   fe                      = list(label = "Ex (fr)"                ,  md = "Ex~fr~"                               ), 
   half.life               = list(label = "Half-life"              ,  md = "t~1/2~"                               ), 
   adj.r.squared           = list(label = "R-Sq (adj)"             ,  md = "R-Sq~adj~"                            ), 
   r.squared               = list(label = "r-squared"              ,  md = "R^2^"                                 ), 
   lambda.z                = list(label = "Term Rate"              ,  md = "<ff:symbol>l</ff>~z~"                 ), 
   lambda.z.time.first     = list(label = "T first (Term Rate)"    ,  md = "T first <ff:symbol>l</ff>~z~"         ),
   lambda.z.n.points       = list(label = "N Half-life"            ,  md = "N <ff:symbol>l</ff>~z~"               ),
   clast.pred              = list(label = "Clast (pred)"           ,  md = "C~last,pred~"                         ),
   span.ratio              = list(label = "Frac Half-life"         ,  md = "Fr t~1/2~"                            ),
   cmax.dn                 = list(label = "Cmax/Dose"              ,  md = "C~max~/Dose"                          ),
   cmin.dn                 = list(label = "Cmin/Dose"              ,  md = "C~min~/Dose"                          ),
   clast.obs.dn            = list(label = "Clast (obs)/Dose"       ,  md = "C~last,obs~/Dose"                     ),
   clast.pred.dn           = list(label = "Clast (pred)/Dose"      ,  md = "C~last,pred~/Dose"                    ),
   cav.dn                  = list(label = "Cave/Dose"              ,  md = "C~ave~/Dose"                          ),
   ctrough.dn              = list(label = "Ctr/Dose"               ,  md = "C~tr~/Dose"                           ),
   thalf.eff.last          = list(label = "Halflife (eff)"         ,  md = "t~1/2,eff~"                           ),
   thalf.eff.iv.last       = list(label = "Halflife (eff,IV)"      ,  md = "t~1/2,eff,IV~"                        ),
   kel.last                = list(label = "kel"                    ,  md = "k~el~"                                ),
   kel.iv.last             = list(label = "kel (iv)"               ,  md = "kel~iv~"                              ),
   aucinf.obs              = list(label = "AUC (inf,obs)"          ,  md = "AUC~inf,obs~"                         ),
   aucinf.pred             = list(label = "AUC (inf,pred)"         ,  md = "AUC~inf,pred~"                        ),
   aumcinf.obs             = list(label = "AMUC (inf,obs)"         ,  md = "AMUC~inf,obs~"                        ),
   aumcinf.pred            = list(label = "AMUC (inf,pred)"        ,  md = "AMUC~inf,pred~"                       ),
   aucminf.obs.dn          = list(label = "AMUC (inf,obs)/Dose"    ,  md = "AMUC~inf,obs~/Dose"                   ),
   aucminf.pred.dn         = list(label = "AMUC (inf,pred)/Dose"   ,  md = "AMUC~inf,pred~/Dose"                  ),
   aucpext.obs             = list(label = "AUC Extrap (obs,%)"     ,  md = "AUC~Extrap,obs~(%)"                   ),
   aucpext.pred            = list(label = "AUC Extrap (pred,%)"    ,  md = "AUC~Extrap,pred~(%)"                  ),
   cl.obs                  = list(label = "CL (obs)"               ,  md = "CL~obs~"                              ),
   cl.pred                 = list(label = "CL (pred)"              ,  md = "CL~pred~"                             ),
   mrt.obs                 = list(label = "MRT (obs)"              ,  md = "MRT~obs~"                             ),
   mrt.pred                = list(label = "MRT (pred)"             ,  md = "MRT~pred~"                            ),
   mrt.iv.pred             = list(label = "MRT (pred,IV)"          ,  md = "MRT~pred,IV~"                         ),
   mrt.iv.obs              = list(label = "MRT (obs,IV)"           ,  md = "MRT~obs,IV~"                          ),
   mrt.md.pred             = list(label = "MRT (pred,MD)"          ,  md = "MRT~pred,MD~"                         ),
   mrt.md.obs              = list(label = "MRT (obs,MD)"           ,  md = "MRT~obs,MD~"                          ),
   vz.obs                  = list(label = "Vz (obs)"               ,  md = "Vz~obs~"                              ),   
   vz.pred                 = list(label = "Vz (pred)"              ,  md = "Vz~pred~"                             ),   
   vss.obs                 = list(label = "Vss (obs)"              ,  md = "Vss~obs~"                             ),   
   vss.pred                = list(label = "Vss (pred)"             ,  md = "Vss~pred~"                            ),   
   vss.iv.obs              = list(label = "Vss (obs,IV)"           ,  md = "Vss~obs,IV~"                          ),   
   vss.iv.pred             = list(label = "Vss (pred,IV)"          ,  md = "Vss~pred,IV~"                         ),   
   vss.md.obs              = list(label = "Vss (obs,MD)"           ,  md = "Vss~obs,MD~"                          ),   
   vss.md.pred             = list(label = "Vss (pred,MD)"          ,  md = "Vss~pred,MD~"                         ),   
   vd.obs                  = list(label = "Vd (obs,MD)"            ,  md = "Vd~obs,MD~"                           ),   
   vd.pred                 = list(label = "Vd (pred,MD)"           ,  md = "Vd~pred,MD~"                          ),   
   thalf.eff.obs           = list(label = "Half-life (obs,eff)"    ,  md = "t~1/2,obs,eff~"                       ),   
   thalf.eff.pred          = list(label = "Half-life (pred,eff)"   ,  md = "t~1/2,pred,eff~"                      ),   
   thalf.eff.iv.obs        = list(label = "Half-life (obs,eff,IV)" ,  md = "t~1/2,obs,eff,IV~"                    ),   
   thalf.eff.iv.pred       = list(label = "Half-life (pred,eff,IV)",  md = "t~1/2,pred,eff,IV~"                   ),   
   kel.last.obs            = list(label = "kel (obs)"              ,  md = "k~el,obs~"                            ),
   kel.last.pred           = list(label = "kel (pred)"             ,  md = "k~el,pred~"                           ),
   kel.iv.obs              = list(label = "kel (obs,IV)"           ,  md = "k~el,obs,IV~"                         ),
   kel.iv.pred             = list(label = "kel (pred,IV)"          ,  md = "k~el,pred,IV~"                        ))

PKNCA_interval_cols = PKNCA::get.interval.cols()

# Populating the from and description fields
for(pkparam in names(res_PKNCA)){
  res_PKNCA[[pkparam]][["description"]] = PKNCA_interval_cols[[pkparam]][["desc"]]
  res_PKNCA[[pkparam]][["from"]]        = "PKNCA"
}


res_ubiquity = list(
  ID              = list(label       = "ID",       
                         md          = "ID",
                         description = "Subject (serial sampling) or Group ID (sparse sampling)",
                         from        = "ubiquity"),
  Dose_Number     = list(label       = "Dose Num",       
                         md          = "Dose~N~",
                         description = "Dose number",
                         from        = "ubiquity"),
  cmax            = list(label       = "Cmax",       
                         md          = "C~max~",
                         description = "Maximum observed concentration",
                         from        = "ubiquity"),
# tmax            = list(label       = "Tmax",       
#                        md          = "T~max~",
#                        description = "Time of maximum observed concentration",
#                        from        = "ubiquity"),
  Nobs            = list(label       = "Nobs",       
                         md          = "N~obs~",
                         description = "Number of observations",         
                         from        = "ubiquity"),
  Dose            = list(label       = "Dose",       
                         md          = "Dose",
                         description = "Dose in dosing units",
                         from        = "ubiquity"),
  Dose_CU         = list(label       = "Dose (CU)",       
                         md          = "Dose~CU~",
                         description = "Dose in concentration units",
                         from        = "ubiquity"),
  C0              = list(label       = "C0 Extrap",
                         md          = "C~0,ext~",
                         description = "Time zero extrapolated concentration",
                         from        = "ubiquity"),
  Vp_obs          = list(label       = "Vp (obs)",
                         md          = "V~p,obs~",
                         description = "Plasma volume of IV dose based on first observed concentration", 
                         from        = "ubiquity"),
  AUCBailer       = list(label       = "AUC (sparse)",
                         md          = "AUC~sparse~",
                         description = "AUC to last time point using Bailers method",
                         from        = "ubiquity"),
  AUCBailer_var   = list(label       = "AUC (sparse) var",
                         md          = "Var(AUC~sparse~)",
                         description = "Variance of AUC to last time point using Bailers method",
                         from        = "ubiquity"))


if(!isgood){
  vp(cfg, "ubiquity::system_nca_parameters_meta()")
  vp(cfg, "Errors were found see messages above for more information")
}

res = list(isgood     = isgood,
           parameters = c(res_PKNCA, res_ubiquity))

res}




#-------------------------------------------------------------------------
#'@export 
#'@title Fetch NCA Results
#'@description Fetches the NCA summary from the ubiquity system object.
#'
#'@param cfg ubiquity system object
#'@param analysis_name string containing the name of the NCA analysis (default \code{'analysis'})
#'
#'@return List with a data frame of the NCA results (\code{NCA_sum}), the raw
#' output from PKNCA (\code{PKNCA_results}), and also a list element indicating the
#' overall success of the function call (\code{isgood})
#'@seealso Vignette on NCA (\code{vignette("NCA", package = "ubiquity")}) 

system_fetch_nca = function(cfg,
                             analysis_name = "analysis"){

  isgood        = TRUE
  NCA_summary   = NULL
  PKNCA_results = NULL

  if((analysis_name %in% names(cfg[["nca"]]))){
    NCA_summary   = cfg[["nca"]][[analysis_name]][["NCA_sum"]]
    PKNCA_results = cfg[["nca"]][[analysis_name]][["PKNCA_raw"]]
  } else {
    isgood = FALSE
    vp(cfg, paste("The NCA analysis >", analysis_name, "< was not found", sep=""))
    vp(cfg, "ubiquity::system_fetch_nca()")
    vp(cfg, "Errors were found see messages above for more information")
  }

  res = list(isgood        = isgood,
             NCA_summary   = NCA_summary,
             PKNCA_results = PKNCA_results)
res}

#-------------------------------------------------------------------------
#'@export 
#'@title Columns in NCA Analysis
#'@description Show the columns available in a given NCA analysis
#'@param cfg ubiquity system object
#'@param analysis_name string containing the name of the NCA analysis (default \code{'analysis'})
#'@return list with the following elements:
#' \itemize{
#'    \item \code{isgood} Boolean variable to identify if the function
#'        executed properly (\code{TRUE}) or if there were any errors
#'        (\code{FALSE})
#'    \item \code{NCA_col_summary} dataframe with the columns from the
#'        analysis in \code{analysis_name} (\code{col_name} - NCA short name,
#'        \code{from} - where the parameter was derived from, \code{label} - verbose
#'        text label for the column, and \code{description}, verbose text description
#'        of the parameter.
#'    \item \code{len_NCA_col}     maximum length of the \code{col_name} column
#'    \item \code{len_from}        maximum length of the \code{from} column
#'    \item \code{len_label}       maximum length of the \code{label} column
#'    \item \code{len_description} maximum length of the \code{description} column
#' }
#'@seealso Vignette on NCA (\code{\link{system_nca_parameters_meta}}) 
system_fetch_nca_columns = function(cfg, 
                                   analysis_name = "analysis"){

isgood = TRUE
NCA_col_summary = NULL
NCA_cols        = NULL
len_NCA_col     = 0
len_label       = 0
len_from        = 0
len_description = 0
  

if((analysis_name %in% names(cfg[["nca"]]))){
  NCA_all       = cfg[["nca"]][[analysis_name]]
  NCA_sum       = NCA_all[["NCA_sum"]]
  # inheriting different aspects of the analysis
  dsmap  = NCA_all[["ana_opts"]][["dsmap"]]

} else {
  isgood = FALSE
  vp(cfg, paste("The NCA analysis >", analysis_name, "< was not found", sep=""))
}

if(isgood){
  NCA_cols = names(cfg[["nca"]][[analysis_name]][["NCA_sum"]])

  # Packing them all into a data frame:
  for(NCA_col in NCA_cols){

    if(NCA_col %in% names(cfg[["options"]][["nca_meta"]][["parameters"]])){
      label       =  cfg[["options"]][["nca_meta"]][["parameters"]][[NCA_col]][["label"]]
      description =  cfg[["options"]][["nca_meta"]][["parameters"]][[NCA_col]][["description"]]
      from        =  cfg[["options"]][["nca_meta"]][["parameters"]][[NCA_col]][["from"]]
     
      # Getting the length of strings to print output below
      len_NCA_col     = max(c(len_NCA_col,     nchar(NCA_col)))
      len_from        = max(c(len_from,        nchar(from)))
      len_label       = max(c(len_label,       nchar(label)))
      len_description = max(c(len_description, nchar(description)))
     
      NCA_col_summary = rbind(NCA_col_summary,
           data.frame(col_name    = NCA_col,
                      from        = from,
                      label       = label,
                      description = description))
    } else {
      vp(cfg, paste("Warning the column >", NCA_col, "< was found in the NCA results but is not a general defined parameter", sep=""))
    }
  }
}

# If we fil we drop an error indicating the function we died in:
if(!isgood){
  vp(cfg, "ubiquity::system_nca_view_columns()")
  vp(cfg, "Errors were found see messages above for more information")
}

res = list(isgood          = isgood,
           NCA_col_summary = NCA_col_summary,
           len_NCA_col     = len_NCA_col,
           len_from        = len_from,
           len_label       = len_label,
           len_description = len_description)

res}

#-------------------------------------------------------------------------
#'@export 
#'@title Report NCA   
#'@description Appends the results of NCA to a report
#'
#'@param cfg ubiquity system object
#'@param rptname report name (either PowerPoint or Word) 
#'@param analysis_name string containing the name of the NCA analysis (default \code{'analysis'})
#'@param rows_max maximum number of rows per slide when generating tabular data
#'@param table_headers Boolean variable to add descriptive headers to output tables (default \code{TRUE})
#'@return cfg ubiquity system object with the NCA results appended to the specified report and if the analysis name is specified:
#'@seealso Vignette on NCA (\code{vignette("NCA", package = "ubiquity")}) 
system_rpt_nca = function(cfg, 
                          rptname       = "default",
                          analysis_name = "analysis",
                          rows_max      = 10,
                          table_headers = TRUE){

isgood = TRUE

if(is.null(analysis_name)){
 isgood = FALSE
 vp(cfg, " No analysis_name was specified")
}

# pulling out the onbrand object
obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)

if(is.null(obnd)){
  isgood = FALSE
  vp(cfg, "onbrand::system_fetch_rpt_onbrand_object returned NULL")
} else{
  if(obnd[["isgood"]]){
    # If the onbrand object is good we pull out the report type:
    rpttype = obnd[["rpttype"]]
  } else{
    # If there is something wrong with the onbrand object we set isgood to
    # false, dump an error and try to attach any messages we can
    isgood = FALSE
    vp(cfg, "Bad onbrand object:")
    if(!is.null(obnd[["msgs"]])){
      vp(cfg, obnd[["msgs"]])
    }
  }
}



if(isgood){
  if((analysis_name %in% names(cfg$nca))){
    vp(cfg, "Appending NCA results to report", fmt="h2")
    vp(cfg, paste("  Report:   ", rptname,            sep=""))
    vp(cfg, paste("  Type:     ", rpttype,            sep=""))
    vp(cfg, paste("  Analysis: ", analysis_name,      sep=""))
  } else {
    isgood = FALSE
    vp(cfg, paste("The NCA analysis >", analysis_name, "< was not found", sep=""), fmt="danger")
  }
}


if(isgood){
  # Defining the elements to be used locally
  rptobjs   =  cfg$nca[[analysis_name]]$rptobjs
  grobs_sum =  cfg$nca[[analysis_name]]$grobs_sum
  NCA_sum   =  cfg$nca[[analysis_name]]$NCA_sum
  ID_label  =  cfg$nca[[analysis_name]]$text$ID_label
  ana_opts  =  cfg$nca[[analysis_name]]$ana_opts

  if(ana_opts$sparse){
    ana_type = "Naive-pooled NCA"      
  } else {
    ana_type = "NCA of individual data"
  }

  overview = paste(ana_type, " from ", ana_opts$dsname, " (", 
  cfg$data[[ana_opts$dsname]]$data_file$name, "). For each ", tolower(ID_label),
  " and dose the NCA parameters will be summarized. For each  ", tolower(ID_label), 
  " the full time-course will be shown in grey, the data used for each analysis will be shown in green, the observed AUC will be shown by a green shaded region, and extrapolated values and data used for extrapolation will be shown in orange",
  sep="")

  if(rpttype == "PowerPoint"){
    cfg = system_rpt_add_slide(cfg, 
      rptname  = rptname,
      template = "content_text",
      elements = list(
         title=
           list(content = "NCA Overview",
                type    = "text"),
         content_body=
           list(content = overview,
                type    = "text")))
  } else if(rpttype == "Word"){

    cfg = system_rpt_add_doc_content(cfg=cfg,
      rptname       = rptname,
      type          = "text",
      content       = list(style   = "Normal",
                           text    = overview))

    cfg = system_rpt_add_doc_content(cfg=cfg,
      rptname       = rptname,
      type          = "text",
      content       = list(style   = "Heading_1",
                           text    = paste("NCA broken down by", ID_label, "and dose")))
  }
  # Creating subject level slides for each dose and a summary plot
  for(sub_str in names(rptobjs)){
    #---------------------------------
    # First we add the summary level information for the current dose
    for(dosenum_str in names(rptobjs[[sub_str]])){
      dosenum = rptobjs[[sub_str]][[dosenum_str]]$dosenum
      sub     = rptobjs[[sub_str]][[dosenum_str]]$sub
      if(rpttype == "PowerPoint"){
        cfg = system_rpt_add_slide(cfg, 
          rptname  = rptname,
          template = "two_content_list",
          elements = list(
             title=
               list(content = paste(ID_label,": ", sub, ",  Dose: ", dosenum, sep=""),
                    type    = "text"),
             content_left=
               list(content = rptobjs[[sub_str]][[dosenum_str]]$lc,
                    type    = "list"),
             content_right=
               list(content = rptobjs[[sub_str]][[dosenum_str]]$rc,
                    type    = "list")))
      } else if(rpttype == "Word"){
        tcontent = list()
        tcontent$table     = rptobjs[[sub_str]][[dosenum_str]]$all 
        tcontent$header    = FALSE
        tcontent$first_row = FALSE
        tcontent$caption   = paste(ID_label,": ", sub, ",  Dose: ", dosenum, sep="")
        cfg = system_rpt_add_doc_content(cfg=cfg,
          rptname       = rptname,
          type          = "table",
          content       = tcontent)
      }
    }
    #---------------------------------
    # Now we plot the timecourse for visual confirmation
      if(rpttype == "PowerPoint"){

        cfg = system_rpt_add_slide(cfg, 
          rptname  = rptname,
          template = "content_text",
          elements = list(
            title =
              list(content = paste(ID_label,": ", sub, sep=""),
                   type    = "text"),
            content_body =
              list(content = grobs_sum[[sub_str]],
                   type    = "ggplot")))
      } else if(rpttype == "Word"){
        cfg = system_rpt_add_doc_content(cfg=cfg,
          rptname       = rptname,
          type          = "ggplot",
          content       = list(image   = grobs_sum[[sub_str]],
                               height  = 4.0,
                               width   = 6,
                               caption = paste(ID_label,": ", sub, sep="")))
           
        cfg = system_rpt_add_doc_content(cfg=cfg,
          rptname       = rptname,
          type          = "break")
      }
    #---------------------------------
  }

  # Cleaning up the summary level information
  NCA_sum$Dose_Number =  as.factor(NCA_sum$Dose_Number)
  NCA_sum$Dose_CU     = var2string(NCA_sum$Dose_CU    , nsig_e=2, nsig_f=2)
  NCA_sum$cmax        = var2string(NCA_sum$cmax       , nsig_e=2, nsig_f=2)
  NCA_sum$half.life   = var2string(NCA_sum$half.life  , nsig_e=2, nsig_f=2)
  NCA_sum$Vp_obs      = var2string(NCA_sum$Vp_obs     , nsig_e=2, nsig_f=2)
  NCA_sum$vss.obs     = var2string(NCA_sum$vss.obs    , nsig_e=2, nsig_f=2)
  NCA_sum$vss.pred    = var2string(NCA_sum$vss.pred   , nsig_e=2, nsig_f=2)
  NCA_sum$cl.obs      = var2string(NCA_sum$cl.obs     , nsig_e=2, nsig_f=2)
  NCA_sum$cl.pred     = var2string(NCA_sum$cl.pred    , nsig_e=2, nsig_f=2)
  NCA_sum$auclast     = var2string(NCA_sum$auclast    , nsig_e=2, nsig_f=2)
  NCA_sum$aucinf.pred = var2string(NCA_sum$aucinf.pred, nsig_e=2, nsig_f=2)
  NCA_sum$aucinf.obs  = var2string(NCA_sum$aucinf.obs , nsig_e=2, nsig_f=2)

  #-----------------------------------
  # Tabular results in PowerPoint
  # Stepping through the results 
  offset = 0
  if(rpttype == "PowerPoint"){
    while(offset < nrow(NCA_sum)){
      # Determing the rows to report in this iteration
      rstop = offset+rows_max
      if(nrow(NCA_sum) < rstop){
        rstop = nrow(NCA_sum) }
      row_report = c((offset+1):rstop)
     
      # Stepping the offset:
      offset = offset+rows_max
     
      # Summary tables
      tab1 = list()
      tab1$table = NCA_sum[row_report,c(1:5, 6:11) ]
      if(table_headers){
        tab1$merge_header = FALSE
        tab1$header_top = list(
                  ID          = ID_label    , 
                  Nobs        = "N"         ,
                  Dose_Number = "Dose"      ,
                  Dose        = "Dose"      ,
                  Dose_CU     = "Dose"      ,
                  cmax        = "Cmax"      ,
                  tmax        = "Tmax"      , 
                  half.life   = "Halflife"  ,
                  Vp_obs      = "Vp"        ,
                  vss.obs     = "Vss"       ,
                  vss.pred    = "Vss"       )
        
        tab1$header_middle = list(
                  ID          = ""          ,
                  Nobs        = "Obs"       ,
                  Dose_Number = "Number"    ,
                  Dose        = "Dataset"   ,
                  Dose_CU     = "Conc Units",
                  cmax        = ""          ,
                  tmax        = ""          , 
                  half.life   = ""          ,
                  Vp_obs      = "Observed"  ,
                  vss.obs     = "Observed"  ,
                  vss.pred    = "Predicted" )
      }
      
      tab2 = list()
      tab2$table = NCA_sum[row_report,c(1:5, 12:17) ]
      if(table_headers){
        tab2$merge_header = FALSE
        tab2$header_top = list(
                  ID          = ID_label    ,
                  Nobs        = "N"         ,
                  Dose_Number = "Dose"      ,
                  Dose        = "Dose"      ,
                  Dose_CU     = "Dose"      ,
                  C0          = "C0"        ,
                  cl.obs      = "CL"        ,
                  cl.pred     = "CL"        ,
                  auclast     = "AUC"       ,
                  aucinf.pred = "AUC"       ,
                  aucinf.obs  = "AUC"       )
        
        tab2$header_middle = list(
                  ID          = ""          ,
                  Nobs        = "Obs"       ,
                  Dose_Number = "Number"    ,
                  Dose        = "Dataset"   ,
                  Dose_CU     = "Conc Units",
                  C0          = "Extrap"    , 
                  cl.obs      = "Obs"       ,
                  cl.pred     = "Pred"      ,
                  auclast     = "Last"      ,
                  aucinf.pred = "Inf(Pred)" ,
                  aucinf.obs  = "Inf(Obs)" )
      }
  
  
    
    # Splitting the table across two slides
      cfg = system_rpt_add_slide(cfg, 
        rptname  = rptname,
        template = "content_text",
        elements = list(
           title=
             list(content = "NCA Summary",
                  type    = "text"),
           content_body=
             list(content = tab1,
                  type    = "flextable")))

      cfg = system_rpt_add_slide(cfg, 
        rptname  = rptname,
        template = "content_text",
        elements = list(
           title=
             list(content = "NCA Summary",
                  type    = "text"),
           content_body=
             list(content = tab2,
                  type    = "flextable")))
    }
  
  }
  #-----------------------------------
  # Tabular results in Word
  if(rpttype == "Word"){
    taball = list()
    taball$table = NCA_sum[,c(1:4,6:17) ]
    if(table_headers){
      taball$merge_header  = FALSE
      taball$table_autofit = TRUE
      taball$table_theme   ='theme_zebra'
      taball$caption = "NCA Summary"
      taball$header_top = list(
                ID          = ID_label    , 
                Nobs        = "N"         ,
                Dose_Number = "Dose"      ,
                Dose        = "Dose"      ,
            #   Dose_CU     = "Dose"      ,
                cmax        = "Cmax"      ,
                tmax        = "Tmax"      , 
                half.life   = "Halflife"  ,
                Vp_obs      = "Vp"        ,
                vss.obs     = "Vss"       ,
                vss.pred    = "Vss"       ,
                C0          = "C0"        ,
                cl.obs      = "CL"        ,
                cl.pred     = "CL"        ,
                auclast     = "AUC"       ,
                aucinf.pred = "AUC"       ,
                aucinf.obs  = "AUC"       )
 
      
      taball$header_middle = list(
                ID          = ""          ,
                Nobs        = "Obs"       ,
                Dose_Number = "Number"    ,
                Dose        = ""          ,
             #  Dose_CU     = "CU"        ,
                cmax        = ""          ,
                tmax        = ""          , 
                half.life   = ""          ,
                Vp_obs      = "Obs"       ,
                vss.obs     = "Obs"       ,
                vss.pred    = "Pred"      ,
                C0          = "Extrap"    , 
                cl.obs      = "Obs"       ,
                cl.pred     = "Pred"      ,
                auclast     = "Last"      ,
                aucinf.pred = "Inf(Pred)" ,
                aucinf.obs  = "Inf(Obs)" )
    }
 
   # Flipping to landscape because this will be a pretty wide table.

   cfg = system_rpt_add_doc_content(cfg=cfg,
     rptname       = rptname,
     type          = "section",
     content       = list(section_type="continuous"))
   cfg = system_rpt_add_doc_content(cfg=cfg,
     rptname       = rptname,
     type          = "text",
     content       = list(style   = "Heading_1",
                          text    = paste("Summarized results for each", ID_label)))
   cfg = system_rpt_add_doc_content(cfg=cfg,
     rptname       = rptname,
     type          = "flextable",
     content       = taball)
   cfg = system_rpt_add_doc_content(cfg=cfg,
     rptname       = rptname,
     type          = "section",
     content       = list(section_type="landscape"))

  }
  #-----------------------------------
} else {
   vp(cfg, "ubiquity::system_report_nca()")
   vp(cfg, "Errors were found see messages above for more information")
   stop()
}
  
cfg}


#-------------------------------------------------------------------------
#'@export 
#'@title Initialize GLP study design
#'@description Creates a new GLP study design
#'
#'@param cfg ubiquity system object
#'@param study_title  String containing descriptive information about the study
#'@param study_name   short name used to identify the study in other functions  (\code{"default"})
#'@return cfg ubiquity system object with the study initialized 
system_glp_init   = function(cfg, study_title = 'Study Title', study_name='default'){


   if(ubiquity_name_check(study_name)$isgood){
     # If a report name has been specified but the report 
     # doesn't exist then we initialize it here:
     cfg$glp[[study_name]]$study_title = study_title
     cfg$glp[[study_name]]$scenarios   = list()
   } else {
     vp(cfg, "An invalid study name has been specified")
     vp(cfg, ubiquity_name_check(study_name)$msg)
     vp(cfg, "ubiquity::system_glp_init()")
     vp(cfg, "Errors were found see messages above for more information")
   }

cfg}

# #-------------------------------------------------------------------------
# #'@export 
# #'@title Report GLP Study  
# #'@description Append GLP study design a report
# #'
# #'@param cfg ubiquity system object
# #'@param study_title  String containing descriptive information about the study
# #'@param study_name   short name used to identify the study in other functions  (\code{"default"})
# #'@param rptname      short name used to identify the report to attach results to the study in other functions (\code{"default"})
# #'@return cfg ubiquity system object with the study report information added
# system_report_glp   = function(cfg, study_title = 'Study Title', study_name='default', rptname  = 'default'){
# 
# 
#   isgood = TRUE 
# 
#   # Supported report types
#   rpttypes = c("PowerPoint")
#   rpttype = NULL
# 
#   if((rptname %in% names( cfg$reporting$reports))){
#     rpttype = cfg$reporting$reports[[rptname]]$rpttype
#     if(!(rpttype %in% rpttypes)){
#       isgood = FALSE
#       vp(cfg, paste("GLP Study Design reporting does not support this format >", rpttype, ">", sep=""))
#     }
#   }else{
#     isgood = FALSE
#     vp(cfg, paste("The report >", rptname, "< has not been defined.", sep=""))
#     vp(cfg, paste("Initialize the report to use report generation: ", sep=""))
#     vp(cfg, paste("cfg = system_rpt_read_template(cfg, rptname='", rptname, "')", sep=""))
#   }
# 
#   if(!(study_name %in% names(cfg$glp))){
#     isgood = FALSE
#     vp(cfg, paste("The glp study >", study_name, "< has not been defined.", sep=""))
#     vp(cfg, paste("You need to first Initialize the study: ", sep=""))
#     vp(cfg, paste("cfg = system_glp_init(cfg, study_name='", study_name, "')", sep=""))
#     vp(cfg, paste("Then you must add a scenario using ubiquity::system_glp_scenario().", sep=""))
#   }
# 
#   if(isgood){
#     # Appending to PowerPoint reports
#     if("PowerPoint" == rpttype){
#       vp(cfg, "")
#       vp(cfg, "Appending GLP Tox design to report")
#       vp(cfg, paste("  Report:   ", rptname,      sep=""))
#       vp(cfg, paste("  Study:    ", study_name,   sep=""))
#       # We loop through each scenario and process them:
#       for(scenario in names(cfg$glp[[study_name]]$scenarios)){
#         vp(cfg, paste("  Scenario: ", scenario,   sep=""))
# 
#         # Pulling out the current scenario:
#         SCEN = cfg$glp[[study_name]]$scenarios[[scenario]]
#       
#         # Adding a section for the current scenario:
#         cfg = system_report_slide_section(cfg, rptname=rptname, title=scenario)
#       
#         # Summary slide for the scenario
#         cfg = system_report_slide_content(cfg,
#                 rptname            = rptname,
#                 title              = paste("GLP", SCEN$elements$tox_species, "Study Design"),
#                 content_type       = "list",   
#                 content            = SCEN$hdpsum)
# 
# 
#         #-------------------------------------------
#         # Human PK and AUC slides for the top dose:
#         cfg = system_report_slide_two_col(cfg,
#                 rptname            = rptname,
#                 title              = paste("Human Projections:", SCEN$elements$pres_human_max_dose_str),
#                 left_content_type  = "ggplot",
#                 left_content       = SCEN$human_PK$figure,
#                 right_content_type = "ggplot",
#                 right_content      = SCEN$human_AUC$figure)
#         if(!is.null(SCEN$human_PK$figure_annotated)){
#           cfg = system_report_slide_two_col(cfg,
#                   rptname            = rptname,
#                   title              = paste("Human Projections:", SCEN$elements$pres_human_max_dose_str),
#                   left_content_type  = "ggplot",
#                   left_content       = SCEN$human_PK$figure_annotated,
#                   right_content_type = "ggplot",
#                   right_content      = SCEN$human_AUC$figure_annotated)
#         }
#         #-------------------------------------------
# 
#         #-------------------------------------------
#         # Tox PK and AUC slides to cover top dose with margins
#         cfg = system_report_slide_two_col(cfg,
#                 rptname            = rptname,
#                 title              = paste(SCEN$elements$tox_species, "Projections:", SCEN$elements$pres_tox_dose_str),
#                 left_content_type  = "ggplot",
#                 left_content       = SCEN$tox_PK$figure,
#                 right_content_type = "ggplot",
#                 right_content      = SCEN$tox_AUC$figure)
#      
#         if(!is.null(SCEN$tox_PK$figure_annotated)){
#           cfg = system_report_slide_two_col(cfg,
#                   rptname            = rptname,
#                   title              = paste(SCEN$elements$tox_species, "Projections:", SCEN$elements$pres_tox_dose_str),
#                   left_content_type  = "ggplot",
#                   left_content       = SCEN$tox_PK$figure_annotated,
#                   right_content_type = "ggplot",
#                   right_content      = SCEN$tox_AUC$figure_annotated)
#         }
#         #-------------------------------------------
# 
#       
#         #-------------------------------------------
#         # Lastly we append the simulations to the report:
#         # Looping through each species
#         for(species in names(SCEN$sims)){
#           # All of the doses on the same plot
#           if(!is.null(SCEN$sims[[species]]$all_doses)){
#             cfg = system_report_slide_content(cfg,
#                     rptname            = rptname,
#                     title              = paste(species, "dosing every",  SCEN$sims[[species]]$all_doses$elements$glp_dose_interval_str),
#                     content_type       = "ggplot",   
#                     content            = SCEN$sims[[species]]$all_doses$figure)
#           }
#     
#           # Adding plots for individual doses
#           for(glp_dose_str in names(SCEN$sims[[species]]$individual)){
#             if(!is.null(SCEN$sims[[species]]$individual[[glp_dose_str]]$figure)){
#              cfg = system_report_slide_content(cfg,
#                      rptname            = rptname,
#                      title              = paste(species, "dosing", glp_dose_str, "every", SCEN$sims[[species]]$all_doses$elements$glp_dose_interval_str),
#                      content_type       = "ggplot",   
#                      content            = SCEN$sims[[species]]$individual[[glp_dose_str]]$figure)
#             }
#           }
#         }
#       }
#     }
#   }
# 
#   if(!isgood){
#     vp(cfg, ubiquity_name_check(study_name)$msg)
#     vp(cfg, "ubiquity::system_report_glp()")
#     vp(cfg, "Errors were found see messages above for more information")
#   }
#   
# 
# cfg}
#-------------------------------------------------------------------------
# JMH updated reporting functions
# Update these:
# system_report_nca        --> system_rpt_nca

# -------------------------------------------------------------------------
# system_rpt_add_template_details
#'@export
#'@title Generate Details about Report Template
#'@description Wrapper for the onbrand::template_details function, see the
#'help for that function for more information
#'
#'@param cfg ubiquity system object    
#'@param rptname Report name 
#'
#'@return list with template information, see
#'\code{\link[onbrand]{template_details}} for information on the structure of
#'this list.
#'
#'@seealso \code{\link[onbrand]{template_details}} and
#' Reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
system_rpt_template_details = function (cfg,
                        rptname  = "default"){

details = NULL

# pulling out the onbrand object
obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)

details = onbrand::template_details(obnd, verbose=FALSE)


# This will display the details to the console
if(!is.null(details[["txt"]])){
  vp(cfg, details[["txt"]])
}

# We add any messages that might have been generated
if(!is.null(details[["msgs"]])){
  vp(cfg, details[["msgs"]])
}

# If there were any issues we just 
if(!details[["isgood"]]){
  vp(cfg, "ubiquity::system_rpt_template_details")
  stop()
}

return(details)}
# -------------------------------------------------------------------------
# system_rpt_add_doc_content
#'@export
#'@title Adds Content to a Word Report
#'@description Appends content to an open ubiquity Word report.
#'
#'@param cfg ubiquity system object    
#'@param type Type of content to add. See the
#'  onbrand function \code{\link[onbrand]{report_add_doc_content}} 
#'  for the allowed content types.
#'@param content List with content to add to the report.  See the
#'  onbrand function \code{\link[onbrand]{report_add_doc_content}} 
#'  format of this list. 
#'@param rptname Report name 
#'
#'@return ubiquity system object with the content added to the specified
#'report
#'
#'@seealso \code{\link[onbrand]{report_add_doc_content}} and
#' Reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
system_rpt_add_doc_content = function (cfg,
                        type     = NULL,
                        content  = NULL,
                        rptname  = "default"){


isgood = TRUE

# pulling out the onbrand object
obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)

if(is.null(obnd)){
  isgood = FALSE
} else {

  # Adding the content
  obnd = onbrand::report_add_doc_content(obnd, type=type, content=content, verbose=FALSE)

  # checking for success
  if(obnd[["isgood"]]){
    # Now we return the onbrand object to the cfg 
    cfg = system_set_rpt_onbrand_object(cfg=cfg, obnd=obnd, rptname=rptname)
  } else {
    isgood = FALSE
    vp(cfg, obnd[["msgs"]])
  }
}

if(!isgood){
  vp(cfg, "ubiquity::system_rpt_add_doc_content()")
  stop()
}



return(cfg)}
# -------------------------------------------------------------------------
# system_rpt_add_slide
#'@export
#'@title Add Slide to a Powerpoint Report
#'@description Adds a slide to a ubiquity report.
#'
#'@param cfg ubiquity system object    
#'@param template Name of slide template to use 
#'@param elements List with content to populate placeholders in the slide. See the
#'  onbrand functions \code{\link[onbrand]{report_add_slide}} and
#'  \code{\link[onbrand]{add_pptx_ph_content}} for details on the expected
#'  format of this list. 
#'@param rptname Report name 
#'
#'@return ubiquity system object with the slide added to the specified
#'report
#'
#'@seealso \code{\link[onbrand]{report_add_slide}}, 
#'\code{\link[onbrand]{add_pptx_ph_content}}, and 
#' Reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
system_rpt_add_slide  = function (cfg,
                        template = NULL,
                        elements = NULL,
                        rptname  = "default"){
isgood = TRUE

# pulling out the onbrand object
obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)


if(is.null(obnd)){
  isgood = FALSE
} else {

  # Adding the slide
  obnd = onbrand::report_add_slide(obnd, template=template, elements=elements, verbose=FALSE)

  # checking for success
  if(obnd[["isgood"]]){
    # Now we return the onbrand object to the cfg 
    cfg = system_set_rpt_onbrand_object(cfg=cfg, obnd=obnd, rptname=rptname)
  } else {
    isgood = FALSE
    vp(cfg, obnd[["msgs"]])
  }
}

if(!isgood){
  vp(cfg, "ubiquity::system_rpt_add_slide()")
  stop()
}


return(cfg)}


# -------------------------------------------------------------------------
# system_fetch_rpt_officer_object
#'@export
#'@title Extracts the officer Object From the Specified ubiquity Report 
#'@description This will extract an officer object from the ubiqiuty system
#'object for the specified report name.
#'
#'@param cfg ubiquity system object    
#'@param rptname ubiquity report name 
#'
#'@return officer report object
#'
#'@seealso \code{\link{system_set_rpt_officer_object}}
system_fetch_rpt_officer_object = function (cfg,
                               rptname  = "default"){
isgood = TRUE
rpt    = NULL

obnd = system_fetch_rpt_onbrand_object(cfg=cfg, rptname=rptname)

if(is.null(obnd)){
  isgood = FALSE
} else {
  fres = onbrand::fetch_officer_object(obnd, verbose=FALSE)
  if(fres[[isgood]]){
    rpt = fres[["rpt"]]
  } else {
    isgood = FALSE
    vp(cfg, fres[["msgs"]])
  }
}



if(!isgood){
  vp(cfg, "ubiquity::system_fetch_rpt_officer_object()")
  stop()
}

return(rpt)}
# -------------------------------------------------------------------------
# system_set_rpt_officer_object
#'@export
#'@title Sets the officer Object for the Specified ubiquity Report 
#'@description This will replace the officer object in the ubiqiuty system
#'object for the specified report name with the value supplied.
#'
#'@param cfg ubiquity system object    
#'@param rpt officer report object
#'@param rptname ubiquity report name 
#'
#'@return ubiquity system object with the replaced officer object
#'
#'@seealso \code{\link{system_fetch_rpt_officer_object}}
system_set_rpt_officer_object = function (cfg,
                                rpt  = NULL,
                                rptname  = "default"){
isgood = TRUE


# Pulling out the onbrand object for the current report
obnd = system_fetch_rpt_onbrand_object(cfg, rptname)

if(is.null(obnd)){
  isgood = FALSE
} 


if(isgood){
  # If everything checks out we attach the rpt  object to the report:

  # First we attach the rpt to the onbrand object
  obnd = onbrand::set_officer_object(obnd, rpt=rpt, verbose=FALSE)
  if(obnd[["isgood"]]){
    # Now we return the onbrand object to the cfg 
    cfg = system_set_rpt_onbrand_object(cfg=cfg, obnd=obnd, rptname=rptname)
  } else {
    isgood = FALSE
    vp(cfg, obnd[["msgs"]])
  }
} 


if(!isgood){
  vp(cfg, "ubiquity::system_set_rpt_officer_object()")
  stop()
}

return(cfg)}
# -------------------------------------------------------------------------
# system_fetch_rpt_onbrand_object
#'@export
#'@title Extracts the onbrand Object From the Specified ubiquity Report 
#'@description This will extract an onbrand object from the ubiqiuty system
#'object for the specified report name.
#'
#'@param cfg ubiquity system object    
#'@param rptname ubiquity report name 
#'
#'@return onbrand report object
#'
#'@seealso \code{\link{system_set_rpt_onbrand_object}}
system_fetch_rpt_onbrand_object = function (cfg,
                               rptname  = "default"){
isgood = TRUE
obnd = NULL

if(rptname %in% names(cfg[["reporting"]][["reports"]])){
  if("obnd" %in% names(cfg[["reporting"]][["reports"]][[rptname]])){
    obnd =cfg[["reporting"]][["reports"]][[rptname]][["obnd"]]
  } else {
    vp(cfg, paste0("Unable to find the onbrand object in the report >", rptname, "<"))
    isgood = FALSE
  }

} else {
  vp(cfg, paste0("Unable to find the report >", rptname, "<"))
  isgood = FALSE
}

if(!isgood){
  vp(cfg, "ubiquity::system_fetch_rpt_onbrand_object()")
  stop()
}

return(obnd)}
# -------------------------------------------------------------------------
# system_set_rpt_onbrand_object
#'@export
#'@title Sets the onbrand Object for the Specified ubiquity Report 
#'@description This will reset the onbrand object in the ubiqiuty system
#'object for the specified report name.
#'
#'@param cfg ubiquity system object    
#'@param obnd onbrand report object
#'@param rptname ubiquity report name 
#'
#'@return ubiquity system object with onbrand report set
#'
#'@seealso \code{\link{system_fetch_rpt_onbrand_object}}
system_set_rpt_onbrand_object = function (cfg,
                                obnd = NULL,
                                rptname  = "default"){
isgood = TRUE

if(!(rptname %in% names(cfg[["reporting"]][["reports"]]))){
  vp(cfg, paste0("Unable to find the report >", rptname, "<"))
  isgood = FALSE
}

if(is.null(obnd)){
  vp(cfg, paste0("The obnd object was not specified."))
  isgood = FALSE
}


if(isgood){
  # If everything checks out we attach the obnd object to the report
  cfg[["reporting"]][["reports"]][[rptname]][["obnd"]] = obnd
} else {
  vp(cfg, "ubiquity::system_set_rpt_onbrand_object()")
  stop()
}

return(cfg)}
# -------------------------------------------------------------------------
# system_rpt_save_report   
#'@export
#'@title Save Report to a File
#'@description Saves a ubiquity report to the specified file.
#'
#'@param cfg ubiquity system object    
#'@param output_file File to save the report to (must be either .pptx or .docx
#'depending on the type of report)
#'@param rptname ubiquity report name 
#'
#' @return list with the follwoing elements 
#' \itemize{
#' \item{isgood} Boolean variable indicating success or failure
#' \item{msgs}   Verbose description of the save results
#'}
#'
#'@seealso Reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
system_rpt_save_report = function (cfg,
                        output_file = NULL,
                        rptname     = "default"){

isgood = TRUE

obnd = system_fetch_rpt_onbrand_object(cfg, rptname=rptname)
if(is.null(obnd)){
  isgood = FALSE
}


# Attempting to save the report:
if(isgood){
  res =  onbrand::save_report(obnd, output_file=output_file, verbose=FALSE)

  # setting the function status to the save status
  isgood = res[["isgood"]]
}

# Printing any messages here
vp(cfg, res[["msgs"]])

if(!isgood){
  vp(cfg, "ubiquity::system_rpt_save_report()")
  stop()
}

return(res)}

# -------------------------------------------------------------------------
# system_rpt_read_template
#'@export
#'@title Initialize a New Report
#'@description Creates a new officer report based either on the ubiquity
#' template or one specified by the user. Once created, content can then be
#' added. 
#'
#'@param cfg ubiquity system object    
#'@param template Type of internal template to use ("PowerPoint" or "Word") or path to template file. 
#'@param mapping Path to an onbrand yaml mapping file: If an internal ubiquity
#'template has been supplied, this argument will be ignored and the yaml file
#'from ubiquity will be used.
#'@param rptname report name 
#'
#'@return ubiquity system object with and empty report initialized
#'
#'@details 
#'   The `template` and `mapping` inputs can specify either the internal
#'   ubiquity templates or user-defined templates  If you specify `template`
#'   values of 'PowerPoint` or `Word` then the internal
#'   ubiquity templates for PowerPoint or Word will be used and the mapping
#'   information will be ignored.
#'
#'   If templates other than the values above are specified you will need also
#'   supply a yaml mapping file for an `onbrand` reporting template. The
#'   vignette below highlights how to go about creating these files. 
#'
#'@seealso Reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
#'@seealso Custom Office Template vignette (\code{vignette("Custom_Office_Templates", package="onbrand")})
system_rpt_read_template = function (cfg,
                               template = "PowerPoint",
                               mapping  = NULL,
                               rptname  = "default"){

isgood = TRUE


# Figuring out which template and mapping files
# This will use the internal ubiquity values
if(template %in% c("PowerPoint", "Word")){
  # This works out if we're using stand alone scripts or the ubiquity package
  # and then works out the file location details from there
  if( cfg$options$misc$distribution == "package"){
    mapping_file  = system.file("ubinc", "templates", "report.yaml", package="ubiquity")
    if(template == "PowerPoint"){
      template_file = system.file("ubinc", "templates", "report.pptx", package="ubiquity")
    } else {
      template_file = system.file("ubinc", "templates", "report.docx", package="ubiquity")
    }
  } else {
    mapping_file  = file.path("library", "templates", "report.yaml") 
    if(template == "PowerPoint"){
      template_file = file.path("library", "templates", "report.pptx") 
    } else {
      template_file = file.path("library", "templates", "report.docx") 
    }
  }

} else {
  # This will just pass the user input directly through
  template_file = template
  mapping_file  = mapping

  # Making sure the user has specified both files
  if(is.null(template_file) | is.null(mapping_file)){
    vp(cfg, "To use user-defined reporting templates you must supply both")
    vp(cfg, "a template file (.pptx or .docx) and a mapping file (.yaml).")
    isgood = FALSE
  }
  if(isgood){
    # JMH TODO compare yaml in user defined file to the yaml for the ubiquity
    # templates to make sure they have the same elements
  }
}


if(isgood){
  # Attempting to initialize the report
  obnd = onbrand::read_template(
        template = template_file,
        mapping  = mapping_file,
        verbose  = TRUE)
  
  # assigning the state of the obnd object to the state of the function
  isgood = obnd[["isgood"]]
}


if(isgood){
  # If everything loaded well then we save the object
  cfg[["reporting"]][["reports"]][[rptname]][["obnd"]] = obnd

  vp(cfg, paste0("Report initialized"), fmt="h2")
  vp(cfg, paste0("  Name:     ", rptname))
  vp(cfg, paste0("  Type:     ", obnd[["rpttype"]]))
  vp(cfg, paste0("  Template: ", template_file))
  vp(cfg, paste0("  Mapping:  ", mapping_file))

} else {
  vp(cfg, obnd[["msgs"]])
  vp(cfg, "ubiquity::system_rpt_read_template()")
  vp(cfg, sprintf("Report >%s< initialization failed.", rptname)) 
  stop()
}


return(cfg)
}
# -------------------------------------------------------------------------




#-------------------------------------------------------------------------
#'@export 
#'@title Calculate the halflife of data
#'@description  Determines the terminal halflife of a sequence of corresponding times and values with optional minimum and maximum times to censor data. 
#'
#'@param times - sequence of times
#'@param values - corresponding sequence of values
#'@param tmin - minimum time to include (\code{NULL})
#'@param tmax - maximum time to include  (\code{NULL})
#'@return List with the following names
#' \itemize{
#'   \item{thalf} Halflife in units of times above
#'   \item{mod} Result of lm used to fit the log transformed data
#'   \item{df} Dataframe with the data and predicted values at the time within tmin and tmax
#' }
#'@examples
#' x     = c(0:100)
#' y     = exp(-.1*x)
#' th    = calculate_halflife(times=x, values=y)
#' thalf = th$thalf 
calculate_halflife = function(times = NULL,
                     values = NULL,
                     tmin = NULL,
                     tmax = NULL){


  if(is.null(tmin)){
    tmin = min(times)
  }
  
  if(is.null(tmax)){
    tmax = max(times)
  }

  #creating a data frame
  tmpdf = data.frame(times    = times,
                     values   = values,
                     lnvalues = log(values))
  

  # Censoring the data to be between the min and max
  tmpdf = tmpdf[tmpdf$times >= tmin & tmpdf$times <= tmax, ]

  # performing the linear regression
  mod = stats::lm(data=tmpdf, stats::formula(lnvalues~times))

  # pulling out the slope and intercept:
  intercept =  summary(mod)$coefficients[1,1]
  slope     =  summary(mod)$coefficients[2,1]
  k         = -slope

  #
  # C = C0*e^(-kt)
  # 
  # half life --> C - 0.5 C0
  #
  # 0.5 = e^(-kt)
  #
  # ln(1/2) = -kt
  #
  # t = ln(1/2)/(-k)
  #
  thalf   = log(1/2)/(-k)


  lnvalues_pred = intercept + slope*tmpdf$times

  values_pred    = exp(lnvalues_pred)

  tmpdf = cbind(tmpdf, lnvalues_pred, values_pred)

  res = list()

  res$thalf = thalf
  res$mod   = mod
  res$df    = tmpdf
  

res}
#-------------------------------------------------------------------------

#  #-------------------------------------------------------------------------
#  #'@export 
#  #'@title Save results from a GLP Study design
#  #'@description Saves files associated with a GLP study. 
#  #'
#  #'@param cfg ubiquity system object
#  #'@param study_name name of the study to save (\code{"default"})
#  #'@param rptname      short name used to identify the report to attach results to the study in other functions (\code{default})
#  #'@param output_directory optional location to save results (default value of \code{NULL} will use the output folder specified at build time)
#  #'@param prefix optional string to prepend to files generated (default value of \code{NULL} will use \code{study_name})
#  #'@seealso \code{\link{system_glp_init}}, \code{\link{system_glp_scenario}}
#  #'@return List with the following names
#  #' \itemize{
#  #'   \item{isgood} Boolean variable indicating success (\code{TRUE}) or failure (\code{FALSE})
#  #'   \item{files} List with names of the files exported and values containing the paths to the files
#  #' }
#  system_glp_save = function(cfg, 
#                       study_name       = "default",
#                       rptname          = "default",
#                       output_directory = NULL,
#                       prefix           = NULL){
#  
#    # Pulling the output directory from the ubiquity object
#    if(is.null(output_directory)){
#    output_directory = cfg$options$misc$output_directory 
#    }
#  
#    isgood = TRUE
#    res = list()
#  
#    if(is.null(study_name)){
#      isgood = FALSE
#      vp(cfg, "No study_name specified")
#    } else if(is.null(cfg$glp[[study_name]])){
#      isgood = FALSE
#      vp(cfg, paste("The specified study_name '", study_name, "' does not exist"))
#    }
#  
#    if(isgood){
#      # If no prefix has been specified then we use study_name
#      if(is.null(prefix)){
#        prefix=study_name 
#      }
#  
#      # file names
#      ppt_file      = paste(prefix, "_report.pptx",    sep="")
#      sim_file      = paste(prefix, "_simulation.csv", sep="")
#      sum_file      = paste(prefix, "_summary.csv",    sep="")
#  
#      ppt_file_full = file.path(output_directory, ppt_file)
#      sim_file_full = file.path(output_directory, sim_file)
#      sum_file_full = file.path(output_directory, sum_file)
#  
#      vp(cfg, "")
#      vp(cfg, "Exporting GLP study")
#      vp(cfg, paste("  Study:            ", cfg$glp[[study_name]]$study_title))
#      vp(cfg, paste("  Output directory: ", output_directory))
#  
#      # Saving the report:
#      system_report_save(cfg, 
#         output_file = ppt_file_full,
#         rptname     = rptname)
#      res$files[[ppt_file]] = ppt_file_full
#  
#      # Saving the simulation timecourse 
#      if(!is.null(cfg$glp[[study_name]]$simall)){
#        write.csv(file         = sim_file_full, 
#                  quote        = FALSE, 
#                  row.names    = FALSE,
#                  cfg$glp[[study_name]]$simall)
#      res$files[[sim_file]] = sim_file_full
#      }
#  
#      # Saving the simulation summary information 
#      if(!is.null(cfg$glp[[study_name]]$simsum)){
#        write.csv(file         = sum_file_full, 
#                  quote        = FALSE, 
#                  row.names    = FALSE,
#                  cfg$glp[[study_name]]$simsum)
#      res$files[[sum_file]] = sum_file_full
#      }
#  
#      
#    }
#  
#    res$isgood = isgood
#  
#  res}
#-------------------------------------------------------------------------

#'@export 
#'@title Design GLP Study For a Scenario 
#'@description Identifies the top dose required in a GLP tox study in order to match human metrics (Cmax and AUCs) within a specified multiplier.
#'  
#' For a given set of human parameters the human doses required to hit the target Cmin and AUC (both or one) will be identified. The Cmax and AUC associated with the largest of those doses will be determined and the corresponding doses for a tox species (and provided parameters) will be determined for specific tox multipliers. 
#'  
#' Optionally, simulations can be be run by specifying doses for either/or the human or tox species. Sample times can also be specified to generate annotated figures and tables to be given to analysts to facilitate assay design. 
#'
#' The system file requires the following components:
#'
#'  - Output for the drug concentration
#'  - Output for the cumulative AUC
#'  - Bolus dosing defined in a specific compartment
#'  - Timescale specified for the system timescale  (e.g. if the timescale is hours then you need \code{<TS> hours = 1.0})
#'
#'
#'@param cfg ubiquity system object
#'@param output_Conc model output specified with \code{<O>} containing the concentration associated with drug exposure.
#'@param output_AUC  model output specified with \code{<O>} containing the cumulative exposure 
#'@param units_Conc units of concentration (\code{''})
#'@param units_AUC  units of AUC (\code{''})
#'@param timescale  system timescale specified with \code{<TS>} used for AUC comparisons and plotting
#'@param study_scenario  string containing a descriptive name for the tox study
#'@param human_sim_times user-specified simulation output times for humans (same timescale as the system)
#'@param study_name name of the study to append the scenario to set with \code{'system_glp_init()'} (\code{'default'}):
#'  When a report is initialized using \code{\link{system_rpt_read_template}} the report name is 'default' unless otherwise specified. To disable reporting set this to  \code{NULL}, and to use a different report specify the name here.
#'@param human_parameters list containing the human parameters 
#'@param human_bolus string containing the dosing state for human doses (specified with \code{<B:?>}) 
#'@param human_ndose number of human doses to simulate
#'@param human_dose_interval dosing interval in humans (time units specified with \code{<B:?>})
#'@param human_Cmin target Cmin in humans (corresponding to output_Conc above)
#'@param human_AUC  target AUC  in humans (corresponding to output_AUC  above)
#'@param human_sample_interval time interval in units specified by timescale above to evaluate the trough concentration and AUC (e.g c(1.99, 4.001) would consider the interval between 2 and 4)
#'@param human_sim_doses  optional list of doses into \code{human_bolus} to simulate (see Details below)
#'@param human_sim_samples optional list of sample times in units specified by timescale above to label on plots of simulated doses (the default \code{NULL} will disable labels)
#'@param tox_species optional name of the tox species (\code{"Tox"})
#'@param tox_sim_times user-specified simulation output times for the tox species (same timescale as the system)  
#'@param tox_parameters list containing the parameters for the tox species
#'@param tox_bolus string containing the dosing state for tox species doses (specified with \code{<B:?>})        
#'@param tox_ndose number of tox doses to simulate
#'@param tox_dose_interval dosing interval in the tox species (time units specified with \code{<B:?>})
#'@param tox_Cmax_multiple for each target (Cmin and AUC) the dose in the tox species will be found to cover this multiple over the projected Cmax in humans (10) 
#'@param tox_AUC_multiple for each target (Cmin and AUC) the dose in the tox species will be found to cover this multiple over the projected AUC in humans (10)
#'@param tox_sample_interval interval to consider the AUC and Cmax for comparing the human prediction to the tox multiple
#'@param tox_sim_doses  optional list of doses into \code{tox_bolus} to simulate (see Details below)
#'@param tox_sim_samples  optional list of sample times in units specified by timescale above to label on plots of simulated doses (the default \code{NULL} will disable labels)  
#'@param annotate_plots Boolean switch to indicate if \code{human_sim_samples} and \code{tox_sim_samples} should be labeled on  their respective plots (\code{TRUE})
#'
#'@details
#'  Both \code{human_sim_doses} and \code{tox_sim_doses} are lists with names
#'  corresponding to the label of the dose. Each element has an AMT and TIME
#'  element which corresponds to the dosing times and amounts in the units 
#'  specified with \code{<B:?>} in the system file.
#'
#'  For example if you wanted to simulate four weekly doses of 20 mg to a 70 kg 
#'  person and the units of bolus doses were days and mg/kg for the times and 
#'  amounts you would do the following:
#'
#'\preformatted{
#'  human_sim_doses = list()
#'  human_sim_doses[["20 mg QW"]]$TIME = c(     0,      7,     14,     21)
#'  human_sim_doses[["20 mg QW"]]$AMT  = c(0.2857, 0.2857, 0.2857, 0.2857)
#'}
#'@return cfg ubiquity system object with the scenario added if successful
system_glp_scenario = function(cfg,                                 
                             output_Conc          = NULL,         output_AUC         = NULL,         timescale        = NULL,   
                             units_Conc           = '',           units_AUC          = '',                            
                             study_scenario       = "Tox Study",  human_sim_times    = NULL,         study_name       = 'default',
                             human_parameters     = NULL,         human_bolus        = NULL,         human_ndose      = 1, 
                             human_dose_interval  = 1,            human_Cmin         = NULL,         human_AUC        = NULL,
                             human_sample_interval= NULL,         human_sim_doses    = NULL,         
                             human_sim_samples    = NULL,
                             tox_species          = 'Tox',        tox_sim_times      = NULL,
                             tox_parameters       = NULL,         tox_bolus          = NULL,         tox_ndose        = 1, 
                             tox_dose_interval    = 1,            tox_Cmax_multiple  = 10,           tox_AUC_multiple = 10,   
                             tox_sample_interval  = NULL,         tox_sim_doses      = NULL,         
                             tox_sim_samples      = NULL,
                             annotate_plots       = TRUE){



  isgood = TRUE


  #
  # checking the function inputs:
  #
  if(is.null(output_AUC)){
    isgood = FALSE
    vp(cfg, "You must specify a model output containing the AUC")
  } else if(!(output_AUC %in% names(cfg$options$mi$outputs))){
    isgood = FALSE
    vp(cfg, paste("output_AUC = ", output_AUC, " does not exist.", sep=""))
  }
  if(is.null(output_Conc)){
    isgood = FALSE
    vp(cfg, "You must specify a model output containing the concentration")
  } else if(!(output_Conc %in% names(cfg$options$mi$outputs))){
    isgood = FALSE
    vp(cfg, paste("output_Conc = ", output_Conc, " does not exist.", sep=""))
  }
  if(is.null(timescale)){
    isgood = FALSE
    vp(cfg, "You must specify a model timescale")
  } else if(!(timescale %in% names(cfg$options$time_scales))){
    isgood = FALSE
    vp(cfg, paste("timescale   = ", timescale,   " does not exist.", sep=""))
  }

  # Dosing compartments

  if(is.null(human_bolus)){
    isgood = FALSE
    vp(cfg, "You must specify a human dosing compartment")
  } else if(!(human_bolus %in% names(cfg$options$inputs$bolus$species))){
    isgood = FALSE
    vp(cfg, paste('Check the specified value for human_bolus  ', sep=""))
    vp(cfg, paste('Dosing into  "', human_bolus,   '" has not been defined. ', sep=""))
  }

  if(is.null(tox_bolus)){
    isgood = FALSE
    vp(cfg, "You must specify a tox dosing compartment")
  } else if(!(tox_bolus %in% names(cfg$options$inputs$bolus$species))){
    isgood = FALSE
    vp(cfg, paste('Check the specified value for tox_bolus  ', sep=""))
    vp(cfg, paste('Dosing into  "', tox_bolus,   '" has not been defined. ', sep=""))
  }

  #
  # Check human_AUC and human_Cmin (make sure one is not null)
  #
  if(is.null(human_AUC) & is.null(human_Cmin)){
    isgood = FALSE
    vp(cfg, "You must specify at least one of the following:")
    vp(cfg, "  human_AUC  = 1  # target exposure to achieve in humans")
    vp(cfg, "  human_Cmin = 1  # target concentration to achieve in humans")
  }

  if(is.null(human_sample_interval)){
    isgood = FALSE
    vp(cfg, "To match a target concentration in humans you need ")
    vp(cfg, "to define the interval to consider (timescale time units)")
    vp(cfg, " e.g.  human_sample_interval = c(0,1)  ")
  }

  if(is.null(tox_sample_interval)){
    isgood = FALSE
    vp(cfg, "To compare the max predicted concentration and AUC in humans to the tox")
    vp(cfg, "species you need to define the interval to consider (timescale time units)")
    vp(cfg, " e.g.  tox_sample_interval = c(0,1)  ")
  }

  if( study_scenario %in% names(cfg$glp[[study_name]]$scenarios)){
    isgood = FALSE
    vp(cfg, paste("The study_scenario >", study_scenario, "< already exists.", sep=""))
    vp(cfg, paste("Specify a different value for this scenario"))
  }


  # Getting list of simulation outputs to include:
  # Output the timescales
  sim_cols = c(paste("ts.", names(cfg$options$time_scales), sep=""))
  # Output concentration
  sim_cols = c(sim_cols,  output_Conc)



  # converting the dosing intervals from the units specified with <B> to the
  # system timescale
  tox_dose_interval_TSsys   = eval(parse(text=paste('tox_dose_interval*', cfg$options$inputs$bolus$times$scale, sep = "")))
  human_dose_interval_TSsys = eval(parse(text=paste('human_dose_interval*', cfg$options$inputs$bolus$times$scale, sep = "")))

  human_sim_times_include = c(0,
                              human_dose_interval_TSsys*(human_ndose+1),                      # including the end of the last dosing interval
                              human_sample_interval/cfg$options$time_scales[[timescale]])     # including the sample 

  tox_sim_times_include   = c(0, 
                              tox_dose_interval_TSsys*(tox_ndose+1),                               # including the end of the last dosing interval
                              tox_sample_interval/cfg$options$time_scales[[timescale]])       # including the Cmin interval

 
  if(is.null(human_sim_times)){
    # If no human simulation times were specified we use a smooth profile with
    # the include values above
    human_sim_times = sort(unique(c(linspace(min(human_sim_times_include), max(human_sim_times_include), 200), human_sim_times_include)))
  } else {
    # If they are included we add the include values abvoe
    human_sim_times = sort(unique(c(human_sim_times, human_sim_times_include)))
  }
 
  if(is.null(tox_sim_times)){
    # If no tox simulation times were specified we use a smooth profile with
    # the include values above
    tox_sim_times = sort(unique(c(linspace(min(tox_sim_times_include), max(tox_sim_times_include), 200), tox_sim_times_include)))
  } else {
    # If they are included we add the include values abvoe
    tox_sim_times = sort(unique(c(tox_sim_times, tox_sim_times_include)))
  }


  # Converting the specified timescale units (e.g. "hours") in to the output
  # column from the simulation (e.g. "ts.hours")
  timescale_col = paste('ts.', timescale, sep="")

  # If everything checked out above we start the analysis
  if(isgood){
    #------------------------------------------------------------------ 
    # storing the ggplot objects to be returned to the user
    # JMH remove: 
    SCEN     = list()
    # Default values for the human doses:
    HT = list()
    human_dose_BL   = 1      # Baseline used for calculations 
    tox_dose_BL     = 1      #                               
    HT$Cmin$dose    = NULL   # Dose to match Cmin target
    HT$AUC$dose     = NULL   # Dose to match AUC target


    TT = list()

    # Dose times for human and tox scenarios:
    human_dose_times  = 0:(human_ndose-1)*human_dose_interval
    tox_dose_times    = 0:(tox_ndose-1)*tox_dose_interval

    # Simulating the human baseline response
    human_dose_values_BL = rep(human_dose_BL, human_ndose)
    # Setting the human simulation times:
    cfg =system_set_option(cfg, group  = "simulation", 
                                option = "output_times", 
                                human_sim_times)
    cfg = system_zero_inputs(cfg) 
    cfg = system_set_bolus(cfg, state   = human_bolus, 
                                times   = human_dose_times, 
                                values  = human_dose_values_BL)
    som = run_simulation_ubiquity(human_parameters, cfg)

    simout_human_BL = som$simout

    #
    # Finding the doses and running simulations for human targets:
    #
    if(!is.null(human_AUC)){
      # Pulling out the rows corresponding to the AUC interval
      human_AUC_rows_BL = simout_human_BL[human_sample_interval[1] <= simout_human_BL[[timescale_col]] & simout_human_BL[[timescale_col]] <= human_sample_interval[2] , ]
      # Since the output_AUC should be a cumulative AUC we can just take the
      # max-min over the interval to get the AUC:
      human_AUC_BL      = max(human_AUC_rows_BL[[output_AUC]]) - min(human_AUC_rows_BL[[output_AUC]]) 
      # Based on the BL AUC and the desired AUC we calculate out what the
      # dose should be:
      HT$AUC$dose            = human_dose_BL*human_AUC/human_AUC_BL 

      # Now we simulate the system at the correct dose:
      human_dose_values_AUC  = rep(HT$AUC$dose    , human_ndose)
      cfg = system_zero_inputs(cfg) 
      cfg = system_set_bolus(cfg, state   = human_bolus, 
                                  times   = human_dose_times, 
                                  values  = human_dose_values_AUC)
      som = run_simulation_ubiquity(human_parameters, cfg)

      # Storing the simulation with the correct dose
      HT$AUC$simout     = som$simout
      # Storing the rows over when the AUC is determined:
      HT$AUC$simout_int = som$simout[human_sample_interval[1] <= som$simout[[timescale_col]] & som$simout[[timescale_col]] <= human_sample_interval[2] , ]
      # Finding the Cmax, Cmin and AUC 
      HT$AUC$Cmax       = max(som$simout[[output_Conc]])
      HT$AUC$Tmax       = som$simout[som$simout[[output_Conc]] == HT$AUC$Cmax, ][[timescale_col]][1]
      HT$AUC$Cmin       = min(HT$AUC$simout_int[[output_Conc]])
      HT$AUC$Tmin       = som$simout[som$simout[[output_Conc]] == HT$AUC$Cmin, ][[timescale_col]][1]
      HT$AUC$AUC_start  = min(HT$AUC$simout_int[[output_AUC]])
      HT$AUC$AUC_stop   = max(HT$AUC$simout_int[[output_AUC]])
      HT$AUC$TAUC_start = som$simout[som$simout[[output_AUC]] == HT$AUC$AUC_start, ][[timescale_col]][1]
      HT$AUC$TAUC_stop  = som$simout[som$simout[[output_AUC]] == HT$AUC$AUC_stop , ][[timescale_col]][1]
      HT$AUC$AUC        = HT$AUC$AUC_stop -  HT$AUC$AUC_start
    }

    if(!is.null(human_Cmin)){
      # Pulling out the rows corresponding to the Cmin interval
      human_Cmin_rows_BL = simout_human_BL[human_sample_interval[1] <= simout_human_BL[[timescale_col]] & simout_human_BL[[timescale_col]] <= human_sample_interval[2] , ]
      # Finding the minimum over that interval
      human_Cmin_BL   = min(human_Cmin_rows_BL[[output_Conc]])
      # Based on the BL Cmin and the desired Cmin we calculate out what the
      # dose should be:
      HT$Cmin$dose           = human_dose_BL*human_Cmin/human_Cmin_BL 
      # Now we simulate the system at the correct dose:
      human_dose_values_Cmin = rep(HT$Cmin$dose,    human_ndose)
      cfg = system_zero_inputs(cfg) 
      cfg = system_set_bolus(cfg, state   = human_bolus, 
                                  times   = human_dose_times, 
                                  values  = human_dose_values_Cmin)
      som = run_simulation_ubiquity(human_parameters, cfg)

      # Storing the simulation with the correct dose
      HT$Cmin$simout     = som$simout
      # Storing the rows over when the Cmin is determined:
      HT$Cmin$simout_int = som$simout[human_sample_interval[1] <= som$simout[[timescale_col]] & som$simout[[timescale_col]] <= human_sample_interval[2] , ]
      # Finding the Cmax, Cmin and AUC 
      HT$Cmin$Cmax       = max(som$simout[[output_Conc]])
      HT$Cmin$Tmax       = som$simout[som$simout[[output_Conc]] == HT$Cmin$Cmax, ][[timescale_col]][1]
      HT$Cmin$Cmin       = min(HT$Cmin$simout_int[[output_Conc]])
      HT$Cmin$Tmin       = som$simout[som$simout[[output_Conc]] == HT$Cmin$Cmin, ][[timescale_col]][1]
      HT$Cmin$AUC_start  = min(HT$Cmin$simout_int[[output_AUC]])
      HT$Cmin$AUC_stop   = max(HT$Cmin$simout_int[[output_AUC]])
      HT$Cmin$TAUC_start = som$simout[som$simout[[output_AUC]] == HT$Cmin$AUC_start, ][[timescale_col]][1]
      HT$Cmin$TAUC_stop  = som$simout[som$simout[[output_AUC]] == HT$Cmin$AUC_stop , ][[timescale_col]][1]
      HT$Cmin$AUC        = HT$Cmin$AUC_stop -  HT$Cmin$AUC_start

    }



    # Now we find the top human dose based on the metrics above:
    # human_dose_max has the top human dose
    human_dose_max = 0
    for(TGT_tmp in names(HT)){
      if(HT[[TGT_tmp]]$dose > human_dose_max){
        human_dose_max = HT[[TGT_tmp]]$dose 
        TGT = TGT_tmp
      }
    }
    # TGT should contain the human target (Cmin or AUC) 
    # that requires the largest human dose

    TGT_Cmax = HT[[TGT]]$Cmax*tox_Cmax_multiple
    TGT_AUC  = HT[[TGT]]$AUC *tox_AUC_multiple

    # First we find the baseline response:
    tox_dose_values_BL = rep(tox_dose_BL, tox_ndose)
    cfg =system_set_option(cfg, group  = "simulation", 
                                option = "output_times", 
                                tox_sim_times)
    cfg = system_zero_inputs(cfg) 
    cfg = system_set_bolus(cfg, state   = tox_bolus, 
                                times   = tox_dose_times, 
                                values  = tox_dose_values_BL)
    som = run_simulation_ubiquity(tox_parameters, cfg)

    simout_tox_BL        = som$simout
    simout_tox_BL_sample = simout_tox_BL[ tox_sample_interval[1] <= simout_tox_BL[[timescale_col]] & simout_tox_BL[[timescale_col]] <= tox_sample_interval[2] , ]

    # PUllingout the metrics over the sampling intervals
    tox_BL_AUC  = max(simout_tox_BL_sample[[output_AUC]]) - min(simout_tox_BL_sample[[output_AUC]])
    tox_BL_Cmax = max(simout_tox_BL_sample[[output_Conc]]) 
    # Now we determine the tox dose to match different metrics
    tox_dose_Cmax          = tox_dose_BL*TGT_Cmax/tox_BL_Cmax
    tox_dose_AUC           = tox_dose_BL*TGT_AUC /tox_BL_AUC
    
    # The tox dose should be the largest of these:
    tox_dose = max(c(tox_dose_Cmax, tox_dose_AUC))

    # simulating out the tox at the max tox dose
    tox_dose_values = rep(tox_dose, tox_ndose)
    cfg = system_set_bolus(cfg, state   = tox_bolus, 
                                times   = tox_dose_times, 
                                values  = tox_dose_values)
    som = run_simulation_ubiquity(tox_parameters, cfg)


    # Simulations for generating the figures
    TT$simout     = som$simout
    TT$simout_int = TT$simout[ tox_sample_interval[1] <= TT$simout[[timescale_col]] & TT$simout[[timescale_col]] <= tox_sample_interval[2] , ]
    TT$Cmin       = min(TT$simout_int[[output_Conc]])
    TT$Tmin       = TT$simout_int[TT$simout_int[[output_Conc]] == TT$Cmin, ][[timescale_col]][1]
    TT$Cmax       = max(TT$simout_int[[output_Conc]])
    TT$Tmax       = TT$simout_int[TT$simout_int[[output_Conc]] == TT$Cmax, ][[timescale_col]][1]
    TT$AUC_start  = min(TT$simout_int[[output_AUC]])
    TT$AUC_stop   = max(TT$simout_int[[output_AUC]])
    TT$AUC        = TT$AUC_stop  - TT$AUC_start
    TT$TAUC_start = TT$simout_int[TT$simout_int[[output_AUC]] == TT$AUC_start, ][[timescale_col]][1]
    TT$TAUC_stop  = TT$simout_int[TT$simout_int[[output_AUC]] == TT$AUC_stop , ][[timescale_col]][1]


    
    #------------------------------------------------------------------ 
    # Generating figures
    # 
    #   Human PK
    p = ggplot() 
    eval(parse(text=paste(" p = p + geom_line(data=HT[[TGT]]$simout, aes(x=", timescale_col, ",y=", output_Conc,"), color='blue')", sep="")))
    if(units_Conc != ""){
      p = p + ylab(paste('Concentration (', units_Conc,')', sep="")) 
    } else {
      p = p + ylab("Concentration")
    }
    p = p + xlab(paste('Time (', timescale,')', sep="")) 

    p = gg_log10_yaxis(fo=p)
    p = prepare_figure(fo=p, purpose="present")
    # Labeling important points:
    p = p + geom_vline(xintercept=human_sample_interval, linetype='dashed', color='gray')
    SCEN$human_PK$figure = p
    if(annotate_plots){
    
      p=p+geom_point(aes(           
                 x     =  HT[[TGT]]$Tmin,      
                 y     =  HT[[TGT]]$Cmin), color="orange")
      p=p+geom_point(aes(           
                 x     =  HT[[TGT]]$Tmax,      
                 y     =  HT[[TGT]]$Cmax), color="purple")
      p = p + ggrepel::geom_label_repel(aes(
                 x     =  HT[[TGT]]$Tmax, 
                 y     =  HT[[TGT]]$Cmax, 
                 label = paste("Cmax =", var2string(HT[[TGT]]$Cmax, nsig_e=1, nsig_f=2),  units_Conc)),
              force         = 5,
              box.padding   = 2.0, 
             #point.padding = 0.5,
              color="purple")
    
      p = p + ggrepel::geom_label_repel(aes(
                 x     =  HT[[TGT]]$Tmin, 
                 y     =  HT[[TGT]]$Cmin, 
                 label = paste("Cmin =", var2string(HT[[TGT]]$Cmin, nsig_e=1, nsig_f=2),  units_Conc)),
              force         = 5,
              box.padding   = 2.0, 
             #point.padding = 0.5,
              color="orange")
      SCEN$human_PK$figure_annotated = p
    } else {
      SCEN$human_PK$figure_annotated = NULL
    }


    #  
    #   Tox PK
    p = ggplot() 
    eval(parse(text=paste(" p = p + geom_line(data=TT$simout, aes(x=", timescale_col, ",y=", output_Conc,"), color='blue')", sep="")))
    if(units_Conc != ""){
      p = p + ylab(paste('Concentration (', units_Conc,')', sep="")) 
    } else {
      p = p + ylab("Concentration")
    }
    p = p + xlab(paste('Time (', timescale,')', sep="")) 

    p = gg_log10_yaxis(fo=p)
    p = prepare_figure(fo=p, purpose="present")
    # Labeling important points:
    p = p + geom_vline(xintercept=tox_sample_interval, linetype='dashed', color='gray')
    SCEN$tox_PK$figure = p
    if(annotate_plots){
    
      p=p+geom_point(aes(           
                 x     =  TT$Tmin,      
                 y     =  TT$Cmin), color="orange")
      p=p+geom_point(aes(           
                 x     =  TT$Tmax,      
                 y     =  TT$Cmax), color="purple")
      p = p + ggrepel::geom_label_repel(aes(
                 x     =  TT$Tmax, 
                 y     =  TT$Cmax, 
                 label = paste("Cmax =", var2string(TT$Cmax, nsig_e=1, nsig_f=2),  units_Conc)),
              force         = 5,
              box.padding   = 2.0, 
             #point.padding = 0.5,
              color="purple")
    
      p = p + ggrepel::geom_label_repel(aes(
                 x     =  TT$Tmin, 
                 y     =  TT$Cmin, 
                 label = paste("Cmin =", var2string(TT$Cmin, nsig_e=1, nsig_f=2),  units_Conc)),
              force         = 5,
              box.padding   = 2.0, 
             #point.padding = 0.5,
              color="orange")
     SCEN$tox_PK$figure_annotated = p
    } else {
     SCEN$tox_PK$figure_annotated = NULL
    }



    #
    # Human AUC
    p = ggplot() 
    eval(parse(text=paste(" p = p + geom_line(data=HT[[TGT]]$simout, aes(x=", timescale_col, ",y=", output_AUC,"), color='blue')", sep="")))
    if(units_AUC != ""){
      p = p + ylab(paste('Cumulative AUC (', units_AUC, ')', sep="")) 
    } else {
      p = p + ylab("Cumulative AUC")
    }
    p = p + xlab(paste('Time (', timescale,')', sep="")) 

    p = gg_log10_yaxis(fo=p)
    p = prepare_figure(fo=p, purpose="present")
    # Labeling important points:
    p = p + geom_vline(xintercept=human_sample_interval, linetype='dashed', color='gray')
    SCEN$human_AUC$figure = p
    if(annotate_plots){
      p=p+geom_point(aes(           
                 x     =  HT[[TGT]]$TAUC_start, 
                 y     =  HT[[TGT]]$AUC_start), color="orange")

      p=p+geom_point(aes(           
                 x     =  HT[[TGT]]$TAUC_stop,  
                 y     =  HT[[TGT]]$AUC_stop ), color="purple")

      p = p + ggrepel::geom_label_repel(aes(
                 x     =  HT[[TGT]]$TAUC_start, 
                 y     =  HT[[TGT]]$AUC_start, 
                 label = paste("", var2string(HT[[TGT]]$AUC_start, nsig_e=1, nsig_f=2),  units_AUC )),
              force         = 5,
              box.padding   = 2.0 , 
             #point.padding = 0.5, 
              color="orange")

      p = p + ggrepel::geom_label_repel(aes(
                 x     =  HT[[TGT]]$TAUC_stop,  
                 y     =  HT[[TGT]]$AUC_stop,  
                 label = paste("", var2string(HT[[TGT]]$AUC_stop,  nsig_e=1, nsig_f=2),  units_AUC )),
              force         = 5,
              box.padding   = 2.0 , 
             #point.padding = 0.5,
              color="purple")

      SCEN$human_AUC$figure_annotated = p
    } else {
      SCEN$human_AUC$figure_annotated = NULL
    }

    #
    # tox AUC
    p = ggplot() 
    eval(parse(text=paste(" p = p + geom_line(data=TT$simout, aes(x=", timescale_col, ",y=", output_AUC,"), color='blue')", sep="")))
    if(units_AUC != ""){
      p = p + ylab(paste('Cumulative AUC (', units_AUC, ')', sep="")) 
    } else {
      p = p + ylab("Cumulative AUC")}
    p = p + xlab(paste('Time (', timescale,')', sep="")) 

    p = gg_log10_yaxis(fo=p)
    p = prepare_figure(fo=p, purpose="present")
    # Labeling important points:
    p = p + geom_vline(xintercept=tox_sample_interval, linetype='dashed', color='gray')
    SCEN$tox_AUC$figure = p
    if(annotate_plots){
      p=p+geom_point(aes(           
                 x     =  TT$TAUC_start, 
                 y     =  TT$AUC_start), color="orange")

      p=p+geom_point(aes(           
                 x     =  TT$TAUC_stop,  
                 y     =  TT$AUC_stop ), color="purple")

      p = p + ggrepel::geom_label_repel(aes(
                 x     =  TT$TAUC_start, 
                 y     =  TT$AUC_start, 
                 label = paste("", var2string(TT$AUC_start, nsig_e=1, nsig_f=2),  units_AUC )),
              force         = 5,
              box.padding   = 2.0 , 
             #point.padding = 0.5, 
              color="orange")

      p = p + ggrepel::geom_label_repel(aes(
                 x     =  TT$TAUC_stop,  
                 y     =  TT$AUC_stop,  
                 label = paste("", var2string(TT$AUC_stop,  nsig_e=1, nsig_f=2),  units_AUC )),
              force         = 5,
              box.padding   = 2.0 , 
             #point.padding = 0.5,
              color="purple")

      SCEN$tox_AUC$figure_annotated = p
    } else {
      SCEN$tox_AUC$figure_annotated = NULL }


    #------------------------------------------------------------------ 
    # Adding tox study report elements
    hdpsum = c(1, "Human dose projections to match:")

    # Test for each target and if it's not null we add a bullet for the dosing
    # information related to that target:
    if(!is.null(human_Cmin)){
      hdpsum = c(hdpsum, 2, 
         paste("Cmin ", var2string(human_Cmin, nsig_e=2, nsig_f=2), 
         " ",
         units_Conc, 
         " (Dose:", 
         var2string(HT$Cmin$dose, nsig_e=2, nsig_f=2),
         " ",
         cfg$options$inputs$bolus$species[[human_bolus]]$units ,
         " every ",
         human_dose_interval,
         " ",
         cfg$options$inputs$bolus$times$units, ")", sep=""))
    }
    if(!is.null(human_AUC)){
      hdpsum = c(hdpsum, 2, 
         paste("AUC ", var2string(human_AUC, nsig_e=2, nsig_f=2), 
         " ",
         units_AUC, 
         " (Dose:", 
         var2string(HT$AUC$dose, nsig_e=2, nsig_f=2),
         " ",
         cfg$options$inputs$bolus$species[[human_bolus]]$units ,
         " every ",
         human_dose_interval,
         " ",
         cfg$options$inputs$bolus$times$units, ")", sep=""))
    }

    # Strings containing the top dose information:
    pres_human_max_dose_str = paste(
         var2string(HT[[TGT]]$dose, nsig_e=2, nsig_f=2),
         cfg$options$inputs$bolus$species[[human_bolus]]$units ,
         " every ",
         human_dose_interval,
         cfg$options$inputs$bolus$times$units)

    pres_tox_dose_str = paste(
         var2string(tox_dose, nsig_e=2, nsig_f=2),
         cfg$options$inputs$bolus$species[[tox_bolus]]$units ,
         " every ",
         tox_dose_interval,
         cfg$options$inputs$bolus$times$units)

    # Saving title/caption information for figures 
    SCEN$human_title            = paste("Human Projections:", pres_human_max_dose_str)
    SCEN$tox_title              = paste(tox_species, "Projections:", pres_tox_dose_str)

    # Now we summarize the statistics for the top dose:
    hdpsum = c(hdpsum, 1, paste("For a human dose of ", 
           pres_human_max_dose_str, 
           " the following are projected", sep=""))
    hdpsum = c(hdpsum, 2, paste("Cmin =", var2string(HT[[TGT]]$Cmin, nsig_e=2, nsig_f=2), units_Conc))
    hdpsum = c(hdpsum, 2, paste("Cmax =", var2string(HT[[TGT]]$Cmax, nsig_e=2, nsig_f=2), units_Conc))
    hdpsum = c(hdpsum, 2, paste("AUC  =", var2string(HT[[TGT]]$AUC , nsig_e=2, nsig_f=2), units_AUC))

    hdpsum = c(hdpsum, 1, paste("To satisfy margins of ", 
                          toString(tox_Cmax_multiple),
                          "times Cmax and",
                          toString(tox_AUC_multiple),
                          "times AUC a dose of",
                          pres_tox_dose_str,
                          "should have the following:"))
    hdpsum = c(hdpsum, 2, paste("Cmax =", var2string(TT$Cmax, nsig_e=2, nsig_f=2), units_Conc))
    hdpsum = c(hdpsum, 2, paste("AUC  =", var2string(TT$AUC , nsig_e=2, nsig_f=2), units_AUC))
    hdpsum = c(hdpsum, 1, paste("The following slides so the predicted concentrations and exposures"))


    # Saving the summary information. 
    SCEN$hdpsum = hdpsum

    #------------------------------------------------------------------ 
    # Running optional simulations
    # These variables will store the simulation results
    simall = NULL
    simsum = NULL

    # Summarizing the dosing information so we can run it for both species in
    # one loop below. sim_all_doses contains the different scenarios we watn
    # to run
    sim_all_doses = NULL
    if(!is.null(human_sim_doses)){
       tmp_all_doses = data.frame(dose_str = names(human_sim_doses),
                                  species  = "Human")
       if(is.null(sim_all_doses)){
         sim_all_doses = tmp_all_doses   
       } else {
         sim_all_doses = rbind( sim_all_doses, tmp_all_doses)  
       }
    }
    if(!is.null(tox_sim_doses)){
       tmp_all_doses = data.frame(dose_str = names(tox_sim_doses),
                                  species  = tox_species)
       if(is.null(sim_all_doses)){
         sim_all_doses = tmp_all_doses   
       } else {
         sim_all_doses = rbind( sim_all_doses, tmp_all_doses)  
       }
    }

    if(!is.null(sim_all_doses)){
      #------------------------------------------------------------------ 
      # Looping through the rows of sim_all_doses running one scenario after
      # another
      for(didx in 1:length(sim_all_doses[,1])){
        scenario_dose_str    = sim_all_doses[didx,]$dose_str
        scenario_species     = sim_all_doses[didx,]$species

        # Setting species-specific parameters here:
        if(scenario_species == "Human"){
          scenario_bolus               = human_bolus
          scenario_sim_times           = human_sim_times
          scenario_dose_times          = human_sim_doses[[as.character(scenario_dose_str)]]$TIME
          scenario_sim_dose            = human_sim_doses[[as.character(scenario_dose_str)]]$AMT
          scenario_parameters          = human_parameters
          scenario_sim_samples         = human_sim_samples
          scenario_dose_interval       = human_dose_interval
          scenario_dose_interval_TSsys = human_dose_interval_TSsys
        } else if(scenario_species == tox_species){
          scenario_bolus               = tox_bolus
          scenario_sim_times           = tox_sim_times
          scenario_dose_times          = tox_sim_doses[[as.character(scenario_dose_str)]]$TIME
          scenario_sim_dose            = tox_sim_doses[[as.character(scenario_dose_str)]]$AMT
          scenario_parameters          = tox_parameters
          scenario_sim_samples         = tox_sim_samples
          scenario_dose_interval       = tox_dose_interval
          scenario_dose_interval_TSsys = tox_dose_interval_TSsys
        }

        # Simulating the scenario
        cfg =system_set_option(cfg, group  = "simulation", 
                                    option = "output_times", 
                                    scenario_sim_times)
        cfg = system_zero_inputs(cfg) 
        cfg = system_set_bolus(cfg, state   = scenario_bolus, 
                                    times   = scenario_dose_times, 
                                    values  = scenario_sim_dose)
        som_tmp = run_simulation_ubiquity(scenario_parameters, cfg)
 
        # Storing the dose, species, etc
        som_tmp$simout$study_scenario        = study_scenario
        som_tmp$simout$glp_species           = scenario_species
        som_tmp$simout$glp_dose_interval     = scenario_dose_interval
        som_tmp$simout$glp_dose_interval_str = paste(scenario_dose_interval,  cfg$options$inputs$bolus$times$units)
        som_tmp$simout$glp_dose_str          = scenario_dose_str

        # Storing the simulated output in the summary table
        if(is.null(simall)){
          simall=som_tmp$simout
        } else {
          simall=rbind(simall, som_tmp$simout)
        }

        # If simulation samples have been requested then we do that
        # The sequence scenario_sim_samples is a list of sample times relative to
        # the nominal dosing time in the units specified by timescale
        if(!is.null(scenario_sim_samples)){
          for(dtidx in 1:length(scenario_dose_times)){
            # Finding the beginning of the start of dosing interval in the
            # system time units:
            DI_start = eval(parse(text=paste("scenario_dose_times[dtidx]*", cfg$options$inputs$bolus$times$scale, sep = "")))
            # Finding the time interval spanned by the dosing interval
            if(dtidx == length(scenario_dose_times)){
              DI_stop = max(som_tmp$simout$ts.time)
            } else {
              DI_stop = DI_start + scenario_dose_interval_TSsys
            }
        
            # Calculating the sample times 
            DI_sample_TSsys   = DI_start + scenario_sim_samples/cfg$options$time_scales[[timescale]]
        
            # Now I trim off any the user specified that extend beyond the
            # dosing interval:
            DI_sample_TSsys   = DI_sample_TSsys[DI_sample_TSsys < DI_stop]
        
            tmplist = list()
            for(sim_col in sim_cols){
              tmplist[[sim_col]] = stats::approx(x=som_tmp$simout$ts.time, y=som_tmp$simout[[sim_col]], xout=DI_sample_TSsys)$y
            }
            tmpdf = as.data.frame(tmplist)
            # Adding sorting columns
            tmpdf$study_scenario  = study_scenario
            tmpdf$dose_number     = dtidx
            tmpdf$glp_species     = scenario_species
            tmpdf$glp_dose_str    = som_tmp$simout$glp_dose_str[1]
            tmpdf$label_str       = var2string(tmpdf[[output_Conc]],  nsig_f=2, nsig_e=2)
        
            if(is.null(simsum)){
              simsum = tmpdf
            } else {
              simsum = rbind(simsum, tmpdf)
            }
          }
        }
      }

      # 
      #------------------------------------------------------------------ 
      # Adding simulation study report elements for each species
      for(species in c("Human", tox_species)){
        # Pulling out the simulation, summary information, etc
        # for the current species
        species_simall = simall[simall$glp_species == species, ]
        species_simsum = simsum[simsum$glp_species == species, ]
        species_xlabel = paste("Time (", timescale, ")", sep="")

        #Smallest value greate than zero:
        species_Conc_lb = min(species_simall[species_simall[[output_Conc]] > 0,][[output_Conc]])
        species_Conc_ub = max(species_simall[[output_Conc]])

        if(units_Conc == ""){
          species_ylabel = paste("Concentration (", output_Conc, ")", sep="")
        } else {
          species_ylabel = paste("Concentration (", units_Conc, ")", sep="")
        }
        if(species == "Human"){
          species_sample_interval = human_sample_interval
        }
        if(species == tox_species){
          species_sample_interval = tox_sample_interval
        }
         
        # This checks to make sure we have simulations for this species
        # if we don't then we just skip that species.
        if(nrow(species_simall) > 0){
          # For each species we add a figure with all of the PK profiles
          p = ggplot()
          # p = p + geom_line(data=species_simall, aes(x=ts.days,y=Cp_ng_ml, color=glp_dose_str))
          eval(parse(text=paste(" p = p + geom_line(data=species_simall, aes(x=", timescale_col, ",y=", output_Conc,", color=glp_dose_str))", sep="")))
          p = p + xlab(species_xlabel)
          p = p + ylab(species_ylabel)
          p = p + guides(color=guide_legend(title="Dose")) 
          p = prepare_figure(fo=p, purpose="present")
          p = gg_log10_yaxis(fo=p)
          p = p + geom_vline(xintercept=species_sample_interval, linetype='dashed', color='gray')
          if(!is.null(human_Cmin)){
            p = p + geom_hline(yintercept=human_Cmin, linetype='dashed', color='grey')
          }
        
          #
          # Saving the summary plot and descriptive elements
          # containing all of the dose levels for the species
          #
          SCEN$sims[[species]]$all_doses$figure                         = p
          SCEN$sims[[species]]$all_doses$elements$glp_dose_interval_str = species_simall$glp_dose_interval_str[1]

          # If annotate_plots is true and sample times have been specified for the
          # current species, we plot each dose level separately with labels
          if(annotate_plots & nrow(species_simsum) > 0){
            for(glp_dose_str in unique(species_simall$glp_dose_str)){

              # Creating temporary datasets for the simulation and summary:
              tmp_simall = species_simall[species_simall$glp_dose_str == glp_dose_str, ] 
              tmp_simsum = species_simsum[species_simsum$glp_dose_str == glp_dose_str, ]

             ## Stripping out zero values 
             #if(min(tmp_simall[[output_Conc]]) <= 0){
             #  tmp_simall = tmp_simall[tmp_simall$ts.time >=  min(tmp_simsum$ts.time),]
             #}
              # Plotting PK 
              p = ggplot()   
              eval(parse(text=paste(" p = p + geom_line(data=tmp_simall, aes(x=", timescale_col, ",y=", output_Conc,"), color='blue')", sep="")))

              eval(parse(text=paste(" p = p + ggrepel::geom_text_repel(data=tmp_simsum, aes(x=", timescale_col, ",y=", output_Conc,", label=label_str), color='orange')", sep="")))
              eval(parse(text=paste(" p = p +               geom_point(data=tmp_simsum, aes(x=", timescale_col, ",y=", output_Conc,"),                  color='orange')", sep="")))
              # Overlaying labels
              p = p + xlab(species_xlabel)
              p = p + ylab(species_ylabel)
              p = prepare_figure(fo=p, purpose="present")
              p = gg_log10_yaxis(fo=p, ylim_min=species_Conc_lb, ylim_max=species_Conc_ub)
              # Storing the plot
              SCEN$sims[[species]]$individual[[glp_dose_str]]$figure = p
            }
          }
        }

      }
      # End of plotting and reporting simulations
      #------------------------------------------------------------------ 
    }
    # End of study design
    #------------------------------------------------------------------ 

    # Saving the simulation and summary data frames 
    if(is.null(cfg$glp[[study_name]]$simall)){
      cfg$glp[[study_name]]$simall  = simall
    } else {
      cfg$glp[[study_name]]$simall  = rbind(cfg$glp[[study_name]]$simall, simall)
    }
    if(is.null(cfg$glp[[study_name]]$simsum)){
      cfg$glp[[study_name]]$simsum  = simsum
    } else {
      cfg$glp[[study_name]]$simsum  = rbind(cfg$glp[[study_name]]$simsum, simsum)
    }

    # preserving components of the scenario
    SCEN$elements$tox_species              =  tox_species
    SCEN$elements$pres_human_max_dose_str  =  pres_human_max_dose_str
    SCEN$elements$pres_tox_dose_str        =  pres_tox_dose_str
                                            
    # saving the ggplot objects 
    cfg$glp[[study_name]]$scenarios[[study_scenario]] = SCEN

  } else {
    vp(cfg, "ubiquity::system_glp_scenario()")
    vp(cfg, "Errors were found see messages above for more information")
  }


cfg }
#-------------------------------------------------------------------------

#'@export
#'@title Implementation of Matlab \code{tic()} command
#'@description Used in conjunction with \code{toc()} to find the elapsed time
#' when code is executed. 
#'
#'@param type can be either \code{"elapsed"} \code{"user.self"} or \code{"sys.self"} 
#'
#'@return time tic was called
#'
#'@examples
#' tic()
#' Sys.sleep(3)
#' toc()
#'@seealso \code{\link{toc}}
tic <- function(type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  tic <- proc.time()[type]         
  tic_file = file.path(tempdir(), "tic.RData")
  save(tic, type, file=tic_file)
  invisible(tic)
}

#-------------------------------------------------------------------------
#'@export
#'@title Implementation of Matlab \code{toc()} command
#'@description Used in conjunction with \code{tic()} to find the elapsed time
#' when code is executed. 
#'
#'@return time in seconds since tic() was called
#'
#'@examples
#' tic()
#' Sys.sleep(3)
#' toc()
#'@seealso \code{\link{tic}}
toc <- function()
{
  type = NULL
  tic_toc = NULL
  tic_file = file.path(tempdir(), "tic.RData")
  if(file.exists(tic_file)){
    load(tic_file)
    toc <- proc.time()[type]
    tic_toc = toc-tic
  } else {
    warning("toc()\nUnable to find tic() information. Run tic() before toc()")
  }

tic_toc}





