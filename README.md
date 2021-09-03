## ubiquity R Workflow 

![R-CMD-check](https://github.com/john-harrold/ubiquity/workflows/R-CMD-check/badge.svg)
[![version](https://www.r-pkg.org/badges/version/ubiquity)](https://CRAN.R-project.org/package=ubiquity)
![cranlogs](https://cranlogs.r-pkg.org/badges/ubiquity) 
![Active](https://www.repostatus.org/badges/latest/active.svg)
[![Lifecycle: Stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

### Overview  

Provides an interface to the R workflow for [ubiquity](<https://r.ubiquity.tools>): a set of modeling tools created to accelerate the development and deployment of models of drug disposition and efficacy within an organization or institution. It is designed to make model development easier, reduce the pain when moving between modeling software, and provide a simple but customizable interface to allow other scientists to interrogate models. It is provide free under a BSD license with source code available in GitHub ([R-package](https://github.com/john-harrold/ubiquity), [ubiquity project](https://github.com/john-harrold/ubiquity-pkpd)). 


### Installing ubiquity

For detailed software requirements and installation instructions see this page:
(<https://ubiquity.tools/rworkflow>)

Briefly, install the following:

* [R](<https://cran.r-project.org>)
* [RStudio](<https://www.rstudio.com/products/rstudio/>)
* [Perl (Windows)](<https://strawberryperl.com/>)
* [Rtools (Windows)](<https://cran.r-project.org/bin/windows/Rtools/>)

Then you can install `ubiquity` from CRAN

```r
install.packages("ubiquity") 
```

Or install the development version from GitHub:

```r
#install.packages("devtools") 
devtools::install_github("john-harrold/ubiquity")
```

### Getting started

For full documentation the [ubiquity vignettes and manuals](<https://r.ubiquity.tools/>) are available.

For more information on specific tasks see the following vignettes: 

* [Language](https://r.ubiquity.tools/articles/Language.html): Constructing ODE-based systems using the ubiquity language
* [Simulation](https://r.ubiquity.tools/articles/Simulation.html): Running individual and population simulations
* [Estimation](https://r.ubiquity.tools/articles/Estimation.html):  Naive-pooled parameter estimation
* [Titration](https://r.ubiquity.tools/articles/Titration.html): Rule-based/titration simulations
* [Deployment](https://r.ubiquity.tools/articles/Deployment.html):  ShinyApp for running and deploying models
* [Reporting](https://r.ubiquity.tools/articles/Reporting.html): Inline generation of PowerPoint and Word reports
* [NCA](https://r.ubiquity.tools/articles/NCA.html):
Automated non-compartmental analysis with PKNCA

### Changes and Updates
See the [NEWS.md](https://github.com/john-harrold/ubiquity/blob/master/NEWS.md) for details on differences between updates.

