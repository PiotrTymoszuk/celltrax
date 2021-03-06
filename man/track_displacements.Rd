% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_funs.R
\name{track_displacements}
\alias{track_displacements}
\title{Calculate scalar displacements.}
\usage{
track_displacements(x, aggregate = c("none", "total", "mean"))
}
\arguments{
\item{x}{a trax object.}

\item{aggregate}{'none' (default): displacements for each step are returned,
'total': total displacements are calculated, 'mean' or 'median': mean or
median displacements per track are returned.}
}
\value{
a tibble with the requested values.
}
\description{
Calculates displacements (lengths of displacement vectors),
step-wise, track-wise, mean or median.
}
