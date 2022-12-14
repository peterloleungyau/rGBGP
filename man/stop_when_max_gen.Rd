% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBGP.R
\name{stop_when_max_gen}
\alias{stop_when_max_gen}
\title{Stopping up to maximum generations.}
\usage{
stop_when_max_gen(max_gen)
}
\arguments{
\item{max_gen}{The maximum generations after which the evolution
should end.}
}
\value{
A stopping condition function that would return TRUE if
\code{max_gen} generations has been done.
}
\description{
Stopping up to maximum generations.
}
