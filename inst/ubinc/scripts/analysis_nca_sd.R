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
cfg = build_system(system_file="system-mab_pk.txt",
                   output_directory     = file.path(".", "output"),
                   temporary_directory  = file.path(".", "transient"))

# -------------------------------------------------------------------------
# Loading Dataset
cfg = system_load_data(cfg, dsname     = "PKDATA", 
                            data_file  = "pk_all_sd.csv")


# Options can be passed to PKNCA

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
                                               ID      = "ID"))
            

# You can access the results as a csv file in the output directory
# file.path("output", "pk_single_dose-nca_summary.csv")
# Or you can pull them out programmatically with the fetch function:
NCA_results = system_fetch_nca(cfg, analysis_name = "pk_single_dose")
NCA_sum =  NCA_results["NCA_summary"]


# This will provide you metadata about all of the available 
# columns in the NCA analysis
NCA_cols = system_fetch_nca_columns(cfg, analysis_name = "pk_single_dose")

# To print the available columns to the screen you can use the 
# system_view command:
# system_view(cfg, "nca", verbose=TRUE)

# To summarize the results in a table use the system_nca_summary function.
# This will summarize the results for the subjects in the 30 mg dose group:
NCA_summary = system_nca_summary(cfg, 
       analysis_name    = "pk_single_dose",
       params_include   = c( "ID", "cmax", "tmax", "half.life", "auclast"),
       params_header    = list(cmax = c( "<label>", "(ng/ml)")),
       ds_wrangle       = "NCA_sum = NCA_sum %>% dplyr::filter(Dose == 30)",
       summary_stats    = list("<MEAN> (<STD>)" = c("auclast", "half.life"),
                               "<MEDIAN>"       = c("tmax")), 
       summary_labels   = list(MEAN             = "Mean", 
                               STD              = "Std Dev", 
                               N                = "N obs",
                               MEDIAN           = "Median", 
                               SE               = "Std Err."),
       summary_location = "ID")

# -------------------------------------------------------------------------
# Writing output to PowerPoint
# Creating an empty report
cfg = system_report_init(cfg, rpttype="PowerPoint")
# Giving the report a title slide
cfg = system_report_slide_title(cfg, title = "NCA of Single Dose PK")
# Appending the NCA results to the report
cfg = system_report_nca(cfg, analysis_name = "pk_single_dose")
# Writing the results to a PowerPoint report
system_report_save(cfg=cfg, output_file=file.path("output", "pk_single_dose-report.pptx"))
# -------------------------------------------------------------------------
# Writing output to Word
# Creating an empty report
cfg = system_report_init(cfg, rpttype="Word")
# Appending the NCA results to the report
cfg = system_report_nca(cfg, analysis_name = "pk_single_dose")
# Writing the results to a Word report
system_report_save(cfg=cfg, output_file=file.path("output", "pk_single_dose-report.docx"))
# -------------------------------------------------------------------------
