## ubiquity

Provides an interface to the R workflow for [ubiquity](<https://ubiquity.tools/rworkflow>): a set of modeling tools created to accelerate the development and deployment of models of drug disposition and efficacy within an organization or institution. It is designed to make model development easier, reduce the pain when moving between modeling software, and provide a simple but customizable interface to allow other scientists to interrogate models. It is provide free under a BSD license with source code available in GitHub ([R-package](https://github.com/john-harrold/ubiquity), [ubiquity project](https://github.com/john-harrold/ubiquity-pkpd)). 

For detailed software requirements and installation instructions see this page:
(<https://ubiquity.tools/rworkflow>)

Briefly, install the following:

* [R](<https://cran.r-project.org>)
* [RStudio](<https://www.rstudio.com/products/rstudio/download/>)
* [Perl (Windows)](<http://strawberryperl.com/>)
* [Rtools (Windows)](<https://cran.r-project.org/bin/windows/Rtools/>)

To install the development version from GitHub:
```
install.packages("devtools") 
devtools::install_github("john-harrold/ubiquity",                                       
              force           = TRUE,
              build           = TRUE, 
              build_vignettes = TRUE,
              build_opts      = c("--no-resave-data", "--no-manual"))
```
For more information on specific tasks see the vignettes: 

* Constructing ODE based systems using the ubiquity language: ``vignette("Language", package = "ubiquity")``
* Running individual and population simulations: ``vignette("Simulation", package = "ubiquity")``
* Naive-pooled parameter estimation: ``vignette("Estimation", package = "ubiquity")``
* Rule-based/titration simulations: ``vignette("Titration", package = "ubiquity")``
* ShinyApp for running and deploying models: ``vignette("Deployment", package = "ubiquity")``
* Inline generation of PowerPoint and Word reports: ``vignette("Reporting", package = "ubiquity")``
* Automated non-compartmental analysis with PKNCA: ``vignette("NCA", package = "ubiquity")``
