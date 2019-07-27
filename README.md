## ubiquity

Ubiquity (<https://ubiquity.tools/rworkflow>) is a set of modeling tools created to accelerate PKPD model development and deployment within an organization or institution. It is designed to make model development easier, reduce the pain when moving between modeling software, and provide a simple but customizable interface to allow other scientists to interrogate models. It is provide free under a BSD license with source code available in GitHub ([R-package](https://github.com/john-harrold/ubiquity), [ubiquity project](https://github.com/john-harrold/ubiquity-pkpd)). This package provides an interface to the ubiquity R Workflow. 

To install from GitHub:
```
install.packages("devtools") 
devtools::install_github("john-harrold/ubiquity",                                       
              force      = TRUE,
              build      = TRUE, 
              build_opts = c("--no-resave-data", "--no-manual"))
```
For more information on specific tasks see the vignettes: 

* Constructing ODE based systems using the ubiquity language: ``vignette("Language", package = "ubiquity")``
* Running individual and population simulations: ``vignette("Simulation", package = "ubiquity")``
* Naive-pooled parameter estimation: ``vignette("Estimation", package = "ubiquity")``
* Rule-based/titration simulations: ``vignette("Titration", package = "ubiquity")``
* ShinyApp for running and deploying models: ``vignette("Deployment", package = "ubiquity")``
* Inline generation of PowerPoint reports: ``vignette("Reporting", package = "ubiquity")``
* Autmoated non-compartmental analysis with PKNCA: ``vignette("NCA", package = "ubiquity")``

### Note
To build systems in R it is necessary that you have a Perl interpreter installed and that it is in your search path. Most Unix systems will have Perl installed and there are many free distributions for windows. For testing [Strawberry Perl](http://strawberryperl.com/) is used. 
