% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing_funs.R
\name{correct_drift}
\alias{correct_drift}
\title{Correct for non-specific drift.}
\usage{
correct_drift(x, drift_vector, return_both = FALSE)
}
\arguments{
\item{x}{a trax object.}

\item{drift_vector}{a drift vector. Its dimensions need to correspond to
the track dimension.}

\item{return_both}{logical: if TRUE, both the cells before and after the
transformation. FALSE by default.}
}
\value{
a trax object or, if return_both is TRUE, a list of trax object with
the 'outcome' (cells passing the procedure) and 'before' elements, each of
them is a trax object.
}
\description{
Corrects for unspecific drift in the trax object by subtracting
the user-provided mean speed vector.
}
