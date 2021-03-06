% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/most-variable.R
\name{most_variable}
\alias{most_variable}
\title{Most variably methylated loci.}
\usage{
most_variable(x, top_n = 1000)
}
\arguments{
\item{x}{A matrix of beta values.}

\item{top_n}{An integer of top N probes. Default to 1000.}
}
\value{
A matrix of beta values of most variably methylated loci.
}
\description{
Obtain the most variably methylated loci. The variability is measured
by standard deviation. The function can also meet the general need to
obtaint the most variable rows.
}
