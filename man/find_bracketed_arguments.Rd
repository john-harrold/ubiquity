% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ubiquity.R
\name{find_bracketed_arguments}
\alias{find_bracketed_arguments}
\title{Parse Prototype Functions for Arguments}
\usage{
find_bracketed_arguments(str, pattern, replace = "", narg, op = "[", cp = "]")
}
\arguments{
\item{str}{string containing the prototype function call}

\item{pattern}{string indicating the start of the function eg. \code{"SI_TT_BOLUS["}}

\item{replace}{string to replace \code{pattern} with}

\item{narg}{number of arguments to prototype function}

\item{op}{string used to indicating open parenthesis}

\item{cp}{string used to indicating close parenthesis}
}
\value{
string containing the actual function call/code built from the prototype function
}
\description{
Parses strings to find abstract functions (of the format
SIFUNC[ARG1][ARG2][ARG3] and extract the arguments from that function and
replace it with actual functions and any additional arguments needed
}
\keyword{internal}
