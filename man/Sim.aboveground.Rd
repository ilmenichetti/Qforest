% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.0.1.R
\name{Sim.aboveground}
\alias{Sim.aboveground}
\title{Simulating the aboveground development of the stand.}
\usage{
Sim.aboveground(SI, Lat, management.plan)
}
\arguments{
\item{SI}{The site index.}

\item{Lat}{The latitude of the simulated site.}

\item{management.plan}{output from from \code{\link{management.plan}}}
}
\value{
a data frame with \itemize{
  \item development of basal area
  \item basal area under bark
  \item number of trees
  \item mean diameter of trees
 }
for each year of the simulation
}
\description{
The Sim.aboveground function simulats the development of the stand concerning the aboveground biomass.
It contains vectors with information from Swedish sites, and selects the correct value from these vectors
based on the site ID from \code{\link{management.plan}}
}
