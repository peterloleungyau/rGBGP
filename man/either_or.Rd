% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBGP.R
\name{either_or}
\alias{either_or}
\title{Convenience function to combine stopping conditions}
\usage{
either_or(...)
}
\arguments{
\item{...}{The stopping condition functions, e.g. by
\code{stop_when_max_gen()} or
\code{stop_when_no_improvement_in_n_gen()} or any custom stopping
condition function.}
}
\value{
A stopping condition that is true if any of the stopping
conditions is true.
}
\description{
Convenience function to combine stopping conditions
}
