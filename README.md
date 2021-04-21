## ubiquity R Workflow 

![R-CMD-check](https://github.com/john-harrold/ubiquity/workflows/R-CMD-check/badge.svg)
[![Build Status](https://travis-ci.com/john-harrold/ubiquity.svg?branch=master)](https://travis-ci.com/john-harrold/ubiquity)
[![version](https://www.r-pkg.org/badges/version/ubiquity)](https://CRAN.R-project.org/package=ubiquity)
![cranlogs](https://cranlogs.r-pkg.org/badges/ubiquity) 
![Active](https://www.repostatus.org/badges/latest/active.svg)
[![Lifecycle: Stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

### Overview  

Provides an interface to the R workflow for [ubiquity](<https://ubiquity.tools/rworkflow>): a set of modeling tools created to accelerate the development and deployment of models of drug disposition and efficacy within an organization or institution. It is designed to make model development easier, reduce the pain when moving between modeling software, and provide a simple but customizable interface to allow other scientists to interrogate models. It is provide free under a BSD license with source code available in GitHub ([R-package](https://github.com/john-harrold/ubiquity), [ubiquity project](https://github.com/john-harrold/ubiquity-pkpd)). 


### Installing ubiquity

For detailed software requirements and installation instructions see this page:
(<https://ubiquity.tools/rworkflow>)

Briefly, install the following:

* [R](<https://cran.r-project.org>)
* [RStudio](<https://www.rstudio.com/products/rstudio/>)
* [Perl (Windows)](<https://strawberryperl.com/>)
* [Rtools (Windows)](<https://cran.r-project.org/bin/windows/Rtools/>)

To install the development version from GitHub:

```r
install.packages("devtools") 
devtools::install_github("john-harrold/ubiquity",                                       
              force           = TRUE,
              build           = TRUE, 
              build_vignettes = TRUE,
              build_opts      = c("--no-resave-data", "--no-manual"))
```

### Getting started

For full documentation go to the [ubiquity vignettes and manuals](<https://john-harrold.github.io/ubiquity/>) are available.

For more information on specific tasks see the following vignettes: 

* [Language](https://john-harrold.github.io/ubiquity/articles/Language.html): Constructing ODE based systems using the ubiquity language
* [Simulation](https://john-harrold.github.io/ubiquity/articles/Simulation.html): Running individual and population simulations: ``vignette("Simulation", package = "ubiquity")``
* [Estimation](https://john-harrold.github.io/ubiquity/articles/Estimation.html):  Naive-pooled parameter estimation
* [Titration](https://john-harrold.github.io/ubiquity/articles/Titration.html):Rule-based/titration simulations
* [Deployment](https://john-harrold.github.io/ubiquity/articles/Deployment.html):  ShinyApp for running and deploying models
* [Reporting](https://john-harrold.github.io/ubiquity/articles/Reporting.html): Inline generation of PowerPoint and Word reports
* [NCA](https://john-harrold.github.io/ubiquity/articles/NCA.html):
Automated non-compartmental analysis with PKNCA

### Changes and Updates
See the 
[CHANGELOG.md](https://github.com/john-harrold/ubiquity-pkpd/blob/master/ubiquity_template/CHANGELOG.md) for details on differences between updates.

