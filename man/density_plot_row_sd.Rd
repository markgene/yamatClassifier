% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/most-variable.R
\name{density_plot_row_sd}
\alias{density_plot_row_sd}
\title{Plot variablity of methylation signals across loci.}
\usage{
density_plot_row_sd(x, top_n = 1000)
}
\arguments{
\item{x}{A matrix of beta values.}

\item{top_n}{An integer of top N probes. Default to 1000.}
}
\value{
A \code{\link[ggplot2]{ggplot}} object.
}
\description{
Plot the distribution of standard deviation of methylation across loci.
If \code{top_n} is specified, add vertical line to denote where the
minimum of SD of the top variably methylated locus is.
}
