% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca123}
\alias{pca123}
\title{PCA step 1, 2, 3.}
\usage{
pca123(x, k = 50)
}
\arguments{
\item{x}{A matrix which has columns as features and rows as samples.}

\item{k}{Number of eigenvalues requested.}
}
\value{
A list of four elements:
  \itemize{
    \item \code{scaled} A matrix of scaled input matrix.
    \item \code{cor_mat} Correlation/covariance matrix of the scaled
      matrix.
    \item \code{eigs} A list returned by \code{\link[RSpectra]{eigs}}.
    \item \code{plot} A \code{\link[ggplot2]{ggplot}} object of
      density plot of eigenvalues.
  }
}
\description{
PCA step 1, 2, 3.
}
\details{
PCA steps 1, 2, 3 are:
   \enumerate{
    \item Center and scale.
    \item Compute the correlation/covariance matrix.
    \item Calculate the eigenvectors and eigenvalues.
  }
  I use \code{\link[RSpectra]{eigs}} function in Rspectra package
  instead of \code{\link[base]{eigen}} function in base to deal
  with large matrix.
}
