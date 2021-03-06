% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_utils.R
\name{get_mean_vector}
\alias{get_mean_vector}
\title{Calculate mean displacement vector for cell population.}
\usage{
get_mean_vector(x, simplify = FALSE)
}
\arguments{
\item{x}{a trax object with a single track representing
the sum displacement vector.}
}
\value{
a trax object or a vector if simplify is TRUE.
}
\description{
Calculates the mean displacement vector for all tracks.
The tracks are normalized prior to calculation.
}
