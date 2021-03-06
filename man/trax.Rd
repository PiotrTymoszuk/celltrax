% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{trax}
\alias{trax}
\title{Create a trax object.}
\usage{
trax(x, ...)
}
\arguments{
\item{x}{a 'tracks' object generated i.e. with
\code{\link[celltrackR]{tracks}}.}

\item{...}{extra arguments, currently none.}
}
\value{
an object of 'trax' class.
}
\description{
Generates a trax object form a 'tracks' class object.
}
\details{
The 'trax' class inherits most of its methods from the 'tracks'
class but provides enhanced plotting and filtering features.
}
