#clearing the workspace
rm(list=ls())
graphics.off()
options(show.error.locations = TRUE)

# If we cannot load the ubiquity package we try the stand alone distribution
if("ubiquity" %in% rownames(installed.packages())){require(ubiquity)} else 
{source(file.path("library", "r_general", "ubiquity.R")) }

# -------------------------------------------------------------------------
# Use system_new(system_file="empty") to create a minimal system file
# Build the system 
cfg = build_system(system_file="system.txt",
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))
# -------------------------------------------------------------------------
# Loading Datasets
#
# From Excel sheet
# cfg = system_load_data(cfg, dsname     = "DSNAME", 
#                             data_file  = "DS.xls", 
#                             data_sheet = "SHEET")
#
# From csv 
# cfg = system_load_data(cfg, dsname     = "DSNAME", 
#                             data_file  = "DS.csv")
#
# From tab 
# cfg = system_load_data(cfg, dsname     = "DSNAME", 
#                             data_file  = "DS.tab")
#    

# To see default PKNCA options use the following command:
# def_NCA_opts = PKNCA.options()
# Use this to overwrite specific values:
# my_NCA_opts = list(max.aucinf.pext  = 10, 
#                    min.hl.r.squared = .9)
# Replace the NULL value of NCA_options below with my_NCA_opts
# to overwrite the PKCNA defaults. 

# Performing NCA
# cfg = system_nca_run(cfg, 
#                      dsname        = "DSNAME", 
#                      dscale        = 1,
#                      NCA_options   = NULL,       
#                      NCA_min       = 4,
#                      analysis_name = "analysis",
#                      dsfilter      = NULL,
#                      dsmap         = list(TIME     = "TIME", 
#                                           NTIME    = "NTIME", 
#                                           CONC     = "DV",
#                                           DOSE     = "AMT", 
#                                           ROUTE    = "ROUTE", 
#                                           ID       = "ID", 
#                                           DOSENUM  = NULL),
#                      digits        = 3)

#-------------------------------------------------------
# Writing the results to a PowerPoint report
#  # Creating an empty report
#  cfg = system_rpt_read_template(cfg, template="PowerPoint")
#  # Giving the report a title slide
#  cfg = system_rpt_add_slide(cfg, 
#    template = "title_slide",
#    elements = list( title= list(content = "NCA Single Dose PK", type    = "text")))
#  # Appending the NCA results to the report
#  cfg = system_rpt_nca(cfg=cfg, analysis_name="analysis")
#  # Writing the results to a PowerPoint file
#  system_rpt_save_report(cfg=cfg, output_file=file.path("output","analysis-report.pptx"))
#-------------------------------------------------------
# Writing the results to a Word report  
#  # Creating an empty report
#  cfg = system_rpt_read_template(cfg, template="Word")
#  # Appending the NCA results to the report
#  cfg = system_rpt_nca(cfg=cfg, analysis_name="analysis")
#  # Writing the results to a Word file
#  system_rpt_save_report(cfg=cfg, output_file=file.path("output","analysis-report.docx"))
#-------------------------------------------------------
