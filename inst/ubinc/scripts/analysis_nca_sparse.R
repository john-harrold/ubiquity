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
                            data_file  = "pk_sparse_sd.csv")

# Performing NCA
cfg = system_nca_run(cfg, dsname        = "PKDATA", 
                          dscale        = 1e6, 
                          analysis_name = "pk_sparse",
                          sparse        = TRUE,
                          dsmap         = list(TIME        = "TIME_HR", 
                                               NTIME       = "TIME_HR", 
                                               CONC        = "C_ng_ml", 
                                               DOSE        = "DOSE",
                                               ROUTE       = "ROUTE", 
                                               ID          = "ID",
                                               SPARSEGROUP = "DOSE"),
                          dsinc         = c("ROUTE"))

# You can access the results as a csv file in the output directory
# file.path("output", "pk_sparse-nca_summary.csv")
# Or you can pull them out programmatically with the fetch function:
NCA_results = system_fetch_nca(cfg, analysis_name = "pk_sparse")
#-------------------------------------------------------
# Writing the results to a PowerPoint report
  cfg = system_rpt_read_template(cfg, template="PowerPoint")
  cfg = system_rpt_add_slide(cfg, 
    template = "title_slide",
    elements = list( title= list(content = "NCA of Sparsely Sampled PK", type    = "text")))
  cfg = system_rpt_nca(cfg=cfg, analysis_name="pk_sparse")         
  system_rpt_save_report(cfg=cfg, output_file=file.path("output","pk_sparse-report.pptx"))
#-------------------------------------------------------
# Writing the results to a Word report  
  cfg = system_rpt_read_template(cfg, template="Word")
  cfg = system_rpt_nca(cfg=cfg, analysis_name="pk_sparse")
  system_rpt_save_report(cfg=cfg, output_file=file.path("output","pk_sparse-report.docx"))
#-------------------------------------------------------
