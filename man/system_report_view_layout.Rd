% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ubiquity.R
\name{system_report_view_layout}
\alias{system_report_view_layout}
\title{Generate Annotated Layout for Report Templates}
\usage{
system_report_view_layout(cfg, rptname = "default", output_file = NULL)
}
\arguments{
\item{cfg}{ubiquity system object}

\item{rptname}{report name initialized with \code{system_report_init}}

\item{output_file}{name of file to place the annotated layout information, set to \code{NULL} and it will generate a file named layout with the appropriate extension}
}
\value{
officer object with the layout of the template annotated,
}
\description{
Elements of slide masters are identified by placeholder labels.
As PowerPoint masters are created the labels
can be difficult to predict. Word documents are identified by style names. 
This function will create a layout file identifying all of the elements of 
each slide master for a PowerPoint template or each paragraph and table 
style for a Word template.
}
\seealso{
\code{\link{system_report_init}} and the reporting vignette (\code{vignette("Reporting", package = "ubiquity")})
}
