% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_simulation.R
\name{run_simulation}
\alias{run_simulation}
\title{Run simulation from a fitted object}
\usage{
run_simulation(
  obj,
  dose,
  interval,
  n_doses = NULL,
  n_days = 5,
  n_subjects = 500,
  aggregate = TRUE,
  bsv = TRUE,
  res_error = TRUE,
  group = NULL,
  path = NULL,
  ...
)
}
\arguments{
\item{obj}{fit object from nlmixr2}

\item{dose}{vector of dose amounts. Needs to match length of \code{interval}}

\item{interval}{vector of dosing intervals. Needs to match length of \code{dose}}

\item{n_doses}{number of doses to simulate}

\item{n_days}{alternative to \code{n_doses}, specify number of days. \code{n_days}
takes precedence if both are specified.}

\item{n_subjects}{number of subjects to simulate}

\item{aggregate}{summarize the data using mean, median, sd, etc.}

\item{bsv}{Simulate using between subject variability}

\item{res_error}{Add residual unexplained error to the simulated data?}

\item{group}{grouping to be added to aggregated data?}

\item{path}{path to file to store output object from fit.}

\item{...}{arguments passed on to rxode2::rxSolve() function}
}
\description{
Using fit object from fit with nlmixr2.
}
