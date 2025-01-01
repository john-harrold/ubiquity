---
editor_options: 
  markdown: 
    wrap: 72
---
# ubiquity 2.1.0 (development version)

## Notes

- Removed the NONMEM and Monolix outputs
- Added translation of nlmixr2 output into NONMEM and Monolix outputs 
- Added simulation option `dynamic` set to `TRUE` by default and when set to `FALSE` it will fix ODEs to 0 and allow for in vitro analysis. 
- Applying `<OE:?>` to variance models in cohort definitions. 
- Fixed issue where initial conditions were not being passed to nlmixr2 output. 

# ubiquity 2.0.3 

## Notes

-  Fixed IIV comments breaking `nlmixr2` output
-  Fixed bug where spaces were breaking compilation of C output
-  Fixed bug in CRAN where flextable/officer update broke saved values in vignettes

# ubiquity 2.0.1

## Notes

-   Removed internal files that were no longer being used
-   Added documentation for creating user defined observation functions
    when running parameter estimation
-   Updated the reporting template to work with the development version
    of `onbrand`
-   Changed terminal messaging to use `cli`
-   Added termination criteria in estimation output
-   Fixed wrapping issue in Fortran output where unwrappable strings
    resulted in infinite loop
-   Added system-testing.txt example
-   Added nlmxir output target
-   Added template creation for NONMEM, Monolix, and nlmixr
-   Fixed R command in compilation to use `R.home()`
-   Fixed broken tests
-   Updated CITATION to use bibentry
-   Fixing error when building systems in windows

# ubiquity 2.0.0

## Notes

-   Lots of small updates to function documentation and the vignettes

-   Completely replaced the reporting functionality. Now using the
    `onbrand` package for templated reporting. **Note:** This will break
    all reporting in version 1 of the R package. Old Word and PowerPoint
    templates will work, but you will need to create a yaml mapping
    file. This is detailed in the Reporting vignette.

-   Reporting changes added the following functions

    -   `system_rpt_add_slide()`
    -   `system_rpt_add_doc_content()`
    -   `system_rpt_read_template()`
    -   `system_rpt_save_report()`
    -   `system_rpt_template_details()`
    -   `system_fetch_rpt_officer_object()`
    -   `system_fetch_rpt_onbrand_object()`
    -   `system_set_rpt_officer_object()`
    -   `system_set_rpt_onbrand_object()`

-   Reporting changes removed the following functions:

    -   `system_report_doc_add_content()`
    -   `system_report_doc_format_section()`
    -   `system_report_doc_set_ph()`
    -   `system_report_estimation()`
    -   `system_report_glp()`
    -   `system_report_init()`
    -   `system_report_nca()`
    -   `system_report_ph_content()`
    -   `system_report_save()`
    -   `system_report_set()`
    -   `system_report_slide_content()`
    -   `system_report_slide_section()`
    -   `system_report_slide_title()`
    -   `system_report_slide_two_col()`
    -   `system_report_view_layout()`
    -   `system_fetch_report()`
    -   `system_fetch_report_format()`
    -   `md_to_officer()`
    -   `md_to_oo()`

-   Reporting changes updated vignettes, example scripts and function
    templates

# ubiquity 1.0.4

## Notes

-   Renamed `system_report_fetch` to `system_fetch_report`
-   Added reporting scripts to the unit tests
-   Added import of officer functions starting with `body_end_`
-   Changed how table and figure captions were being numbered in Word
    reporting
-   Created testthat scripts to run through workshop functions

## New Features

-   Added shaded region for observed AUC in NCA reporting
-   Added cohort-specific output times option to the estimation workflow
-   Updated references in the documentation and templates to point to
    r.ubiquity.tools
-   Added checks in estimation routines to check for reasonable bounds
    for global optimizer and to notify users when estimates are near
    bounds
-   Added `sessionInfo()` to estimation Word reporting
-   Added `system_fetch_report_format`
-   Integrated the markdown (md) header format option with flextable
    outputs
-   Added default for `cfg$reporting$enabled` (FALSE)
-   Added the ability to use markdown for the NCA summary tables
    generated with `system_nca_summary`
-   Fixed the placeholder text, now the delimiters are `===`on either
    side of the text
-   Added `system_set_option` general group and `output_directory`
    option
-   For markdown in Word reporting, default font properties have been
    defined and the ability to specify them in the `org_functions.R`
    template has been added.
-   Moved annotated layout generation of PowerPoint files in
    `system_report_view_layout()` over to the `annotate_base()` command
    in officer
-   Added optional key fields for tables and figures in Word reporting
-   Added the ability to pass PKNCA.options through to `system_nca_run`
-   Added verbose option to the `system_view` command
-   Allowing pass through of dataset columns to summary NCA output
-   Updated `system_report_ph_content` and
    `system_report_doc_add_content` to allow for inclusion of flextable
    objects
-   Updated `system_view` to include nca results
-   Added the following functions:
    -   `system_fetch_nca` - function to fetch NCA results
    -   `system_fetch_nca_columns` - function to fetch column
        descriptors of a specific analysis
    -   `system_nca_parameters_meta` - list of standard NCA parameters
    -   `system_nca_summary` - creates summary tables for NCA results
-   Adding checks in `simulate_subjects` to ensure required columns are
    present
-   Added the ability to read xlsx data sets with `system_load_data`

## Bug Fixes

-   CRAN erroring out because of tic() function. Removed tic() and toc()
    functions
-   Fixed a bug where the dataset given to `system_run_nca` does not
    have enough valid data to actually run NCA
-   Fixed "Coordinate system already present..." warning in `gg_axis`
-   Fixed location of table generation in example script
    `analysis_nca_md.R` so that table styles would be picked up properly
-   Added scales package requirement
-   Fixed missing rptname inputs to `system_report_ph_content`
-   Fixed Bug in system view when there are cohort-specific parameters
    defined
-   Fixed figure generation errors in the estimation workflow
-   Fixed a bug in `system_report_save()`
-   Removed aberrant gdata require calls in templates
-   Fixed bug in `system_nca_run` when DOSE is a factor
-   Fixed bug in `system_nca_run` where back extrapolation was being
    done for doses that should have been skipped due to insufficient
    points. This was causing an error.
-   Removed coercion warnings: In
    tmpsum$halflife = NCA.res$result[NCA.res$result$PPTESTCD ==
    "half.life", : Coercing LHS to a list
-   Removed digits input from `system_nca_run` (this is now handled with
    `system_nca_summary` below)
-   Fixed coercion warnings for covariates when building the system
-   Using explicit declaration of officer functions and specifying those
    in importFrom this is to prevent namespace issues with readxl to
    allow the function readxl::read_xlsx

# ubiquity 1.0.3

## Notes

-   Removed the gdata dependency
-   Removed URL redirects from documentation to resolve CRAN submission
    warnings

## New Features

-   Added the option in simulate subjects to specify secondary
    parameters to be saved

## Bug Fixes

-   Converted `system_nca_run()` from using \$ to mostly using [[""]]
-   Fixed NCA template
-   Fixed bug in ShinyApp template where iiv tab was being displayed
    even when the system had no iiv elements

# ubiquity 1.0.2

## Bug Fixes

-   Updated the components to fix issues encountered with R 4.0
