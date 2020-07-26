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


# Performing NCA

# cfg = system_nca_run(cfg, 
#                      dsname        = "DSNAME", 
#                      dscale        = 1,
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

# -------------------------------------------------------------------------
# Writing output to PowerPoint
#  # Creating an empty report
#  cfg = system_report_init(cfg, rpttype="PowerPoint")
#  # Giving the report a title slide
#  cfg = system_report_slide_title(cfg, title = "NCA in ubiquity")
#  # Appending the NCA results to the report
#  cfg = system_report_nca(cfg, analysis_name = "analysis")
#  # Writing the results to a PowerPoint file
#  system_report_save(cfg=cfg, output_file=file.path("output", "analysis-report.pptx"))
# -------------------------------------------------------------------------
# Writing output to Word
#  # Creating an empty report
#  cfg = system_report_init(cfg, rpttype="Word")
#  # Appending the NCA results to the report
#  cfg = system_report_nca(cfg, analysis_name = "analysis")
#  # Writing the results to a Word file
#  system_report_save(cfg=cfg, output_file=file.path("output", "analysis-report.docx"))
# -------------------------------------------------------------------------

