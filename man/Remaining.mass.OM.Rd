% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.0.1.R
\name{Remaining.mass.OM}
\alias{Remaining.mass.OM}
\title{Simulating the OM decay of the stand.}
\usage{
Remaining.mass.OM(Litter, Harvest.residuals, Lat, parameters, timestep)
}
\arguments{
\item{Litter}{output from from \code{\link{Litter.production}}}

\item{Harvest.residuals}{output from from \code{\link{Harvest.residuals}}}

\item{parameters}{The parameter matrix from \code{\link{parameters}}}

\item{timestep}{time steps for the simulation}

\item{management.plan}{output from from \code{\link{management.plan}}}
}
\value{
a data frame with #' \itemize{
  \item FC needles (??? top be checked what it is exactly)
  \item FC branches (??? top be checked what it is exactly)
  \item GC needles (??? top be checked what it is exactly)
  \item GC branches (??? top be checked what it is exactly)
  \item GC stem V (??? top be checked what it is exactly)
  \item GC stem B (??? top be checked what it is exactly)
  \item GC stump (??? top be checked what it is exactly)
  \item GC roots >5 cm (??? top be checked what it is exactly)
  \item GC roots <5 cm (??? top be checked what it is exactly)
}
for each year of the simulation
}
\description{
The Remaining.mass.OM function applies the function \code{\link{Remaining.OM}}
on the results from \code{\link{Litter.production}} and from \code{\link{Harvest.residuals}}
to simulate the decay of OM in the stand
}
