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
cfg = build_system(system_file="system.txt")

# -------------------------------------------------------------------------
# Loading Dataset
cfg = system_load_data(cfg, dsname     = "PKDATA", 
                            data_file  = "pk_all_sd.csv")

# Performing NCA
cfg = system_nca_run(cfg, dsname        = "PKDATA", 
                          dscale        = 1e6, 
                          analysis_name = "pk_single_dose", 
                          extrap_C0     = FALSE, 
                          dsmap         = list(TIME    = "TIME_HR", 
                                               NTIME   = "TIME_HR", 
                                               CONC    = "C_ng_ml", 
                                               DOSE    = "DOSE",
                                               ROUTE   = "ROUTE", 
                                               ID      = "ID"),
                          digits        = 3)
            

# Creating an empty PowerPoint report
cfg = system_report_init(cfg)

# Giving the report a title slide
cfg = system_report_slide_title(cfg, title = "NCA of Single Dose PK")

# Appending the NCA results to the report
cfg = system_report_nca(cfg, analysis_name = "pk_single_dose")

# Writing the results to a PowerPoint report
system_report_save(cfg=cfg, output_file=file.path("output", "pk_single_dose-report.pptx"))
