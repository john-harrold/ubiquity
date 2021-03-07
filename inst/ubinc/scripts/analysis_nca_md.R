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
                            data_file  = "pk_all_md.csv")

# To see default PKNCA options use the following command:
# def_NCA_opts = PKNCA.options()
# Use this to overwrite specific values:
my_NCA_opts = list(max.aucinf.pext  = 10, 
                   min.hl.r.squared = .9)

# Performing NCA
cfg = system_nca_run(cfg, dsname        = "PKDATA", 
                          dscale        = 1e6, 
                          dsfilter      = list(ID = c(1:5, 25:30, 45:50)),
                          NCA_options   = my_NCA_opts,
                          analysis_name = "pk_multiple_dose", 
                          
                          dsmap         = list(TIME    = "TIME_HR", 
                                               NTIME   = "NTIME_HR", 
                                               CONC    = "C_ng_ml", 
                                               DOSE    = "DOSE",
                                               ROUTE   = "ROUTE", 
                                               ID      = "ID",
                                               DOSENUM = "DOSENUM",
                                               EXTRAP  = "EXTRAP"),
                          dsinc         = c("ROUTE"))

# You can access the results as a csv file in the output directory
# file.path("output", "pk_multiple_dose-nca_summary.csv")
# Or you can pull them out programmatically with the fetch function:
NCA_results = system_fetch_nca(cfg, analysis_name = "pk_multiple_dose")


library(tidyr)
NCA_sum =  NCA_results[["NCA_summary"]]

#params_include   = c( "ID", "tmax", "half.life", "auclast_1", "cmax_1", "auclast_6", "cmax_6", "AR_6_1", "ROUTE")
#NCA_sum = NCA_sum[, params_include]

# This will provide you metadata about all of the available 
# columns in the NCA analysis
NCA_cols = system_fetch_nca_columns(cfg, analysis_name = "pk_multiple_dose")

# To print the available columns to the screen you can use the 
# system_view command:
# system_view(cfg, "nca", verbose=TRUE)

# You can access the results as a csv file in the output directory
# file.path("output", "pk_multiple_dose-nca_summary.csv")
# Or you can pull them out programmatically with the fetch function:
NCA_results = system_fetch_nca(cfg, analysis_name = "pk_multiple_dose")


# -------------------------------------------------------------------------
# Writing output to PowerPoint
# Creating an empty report (this needs to be created before the table
# generation below)
cfg = system_report_init(cfg, rpttype="PowerPoint")

# Here we can take the summary results table and calculate things like
# Dose normalized Cmax and compare the first and last dose:
NCA_sum = NCA_sum             %>%
    dplyr::filter(Dose == 30) %>%
    dplyr::mutate(cmax_dose= cmax/Dose) %>%       
    tidyr::pivot_wider(
       id_cols     = c("ID", "Dose_Number"),
       names_from  = Dose_Number,
       values_from = c(auclast, cmax, cmax_dose, half.life)) %>%
    dplyr::mutate(AR_6_1   = auclast_6/auclast_1) 

# To summarize the results in a table use the system_nca_summary function.
# This will summarize the results for the subjects in the 30 mg dose group:
NCA_summary = system_nca_summary(cfg, 
       analysis_name    = "pk_multiple_dose",
       params_include   = c( "ID", "Dose_Number", "cmax", "tmax", "half.life", "auclast", "ROUTE"),
       params_header    = list(cmax = c( "<label>", "(ng/ml)"), ROUTE=c("route")),
       label_format     = "md", 
       ds_wrangle       = "NCA_sum = NCA_sum %>% dplyr::filter(Dose == 30)",
       summary_stats    = list("<MEAN> (<STD>)" = c("auclast", "half.life"),
                               "<MEDIAN>"       = c("tmax")), 
       summary_labels   = list(MEAN             = "Mean (<ff:symbol>m</ff>)", 
                               STD              = "Std Dev (<ff:symbol>s</ff>)", 
                               N                = "N~obs~",
                               MEDIAN           = "Median", 
                               SE               = "Std Err."),
       summary_location = "ID")

ds_wrangle_str = 'NCA_sum = NCA_sum             %>%
dplyr::filter(Dose == 30) %>%
dplyr::mutate(cmax_dose= cmax/Dose) %>%       
tidyr::pivot_wider(
   id_cols     = c("ID", "Dose_Number"),
   names_from  = Dose_Number,
   values_from = c(auclast, cmax, cmax_dose, half.life)) %>%
dplyr::mutate(AR_6_1   = auclast_6/auclast_1)'


NCA_summary_wide = system_nca_summary(cfg, 
       analysis_name    = "pk_multiple_dose",
       params_include   = c("ID", "auclast_1", "cmax_1", "cmax_dose_1", "half.life_1",
                                  "auclast_6", "cmax_6", "cmax_dose_6", "half.life_6", "AR_6_1"),
       params_header    = list("ID"           = c("ID", "ID"), 
                               "auclast_1"    = c("Day 1",        "AUC last", "hr-ng/ml"       ), 
                               "cmax_1"       = c("",             "Cmax",     "ng/ml"          ), 
                               "cmax_dose_1"  = c("",             "Cmax/Dose", "ng/ml/(mg/kg)" ),
                               "half.life_1"  = c("",             "Halflife",  "hr"            ),
                               "auclast_6"    = c("Day 6",        "AUC last", "hr-ng/ml"       ),    
                               "cmax_6"       = c("",             "Cmax",     "ng/ml"          ), 
                               "cmax_dose_6"  = c("",             "Cmax/Dose", "ng/ml/(mg/kg)" ), 
                               "half.life_6"  = c("",             "Halflife",  "hr"            ), 
                               "AR_6_1"       = c("Day 1/Day 6",  "AR")),
       ds_wrangle       = ds_wrangle_str,
       summary_stats    = list("<MEAN>"         = c("auclast_1", "auclast_6"),
                               "(<STD>)"        = c("auclast_1", "auclast_6")),
       summary_labels   = list(MEAN             = "Mean", 
                               STD              = "Std Dev", 
                               N                = "N obs",
                               MEDIAN           = "Median", 
                               SE               = "Std Err."),
       summary_location = "ID")
                          



cfg = system_report_slide_content(cfg,
       title        = "Day 1 to Day 6 comparison",
       sub_title    = "Accumulation Ratio",
       content_type = "flextable_object", 
       content      = NCA_summary_wide[["nca_summary_ft"]])


# Giving the report a title slide
cfg = system_report_slide_title(cfg, title = "NCA of Multiple Dose PK")
# Appending the NCA results to the report
cfg = system_report_nca(cfg, analysis_name = "pk_multiple_dose")
# Writing the results to a PowerPoint file
system_report_save(cfg=cfg, output_file=file.path("output", "pk_multiple_dose-report.pptx"))
# -------------------------------------------------------------------------
# Writing output to Word
# Creating an empty report
cfg = system_report_init(cfg, rpttype="Word")
# Adding the summary tables above
cfg = system_report_doc_add_content(cfg, 
  content_type  = "flextable_object",
  content       = list(caption = "Summary table of NCA outputs",
                       ft      = NCA_summary[["nca_summary_ft"]]))
cfg = system_report_doc_add_content(cfg, 
  content_type  = "flextable_object",
  content       = list(caption = "Transformed NCA output",
                       ft      = NCA_summary_wide[["nca_summary_ft"]]))

# Appending the NCA results to the report
cfg = system_report_nca(cfg, analysis_name = "pk_multiple_dose")
# Writing the results to a Word file
system_report_save(cfg=cfg, output_file=file.path("output", "pk_multiple_dose-report.docx"))
# -------------------------------------------------------------------------
