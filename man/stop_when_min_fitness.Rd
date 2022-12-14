% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBGP.R
\name{stop_when_min_fitness}
\alias{stop_when_min_fitness}
\title{Stopping when reached min fitness value, in case we are minimizing
and we know what value is the optimum or good enough.}
\usage{
stop_when_min_fitness(min_fitness)
}
\arguments{
\item{min_fitness}{The fitness below which the evolution can stop.}
}
\value{
A stopping condition function that would return TRUE after
a fitness less than or equal \code{min_fitness} is found.
}
\description{
Stopping when reached min fitness value, in case we are minimizing
and we know what value is the optimum or good enough.
}
