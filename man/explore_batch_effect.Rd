% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch-effect-explore.R
\name{explore_batch_effect}
\alias{explore_batch_effect}
\title{Explore batch effect.}
\usage{
explore_batch_effect(
  trainer,
  batch_name = "Batch",
  classification_name = "Classification",
  top_n_rle = 20000,
  rle_downsample = 100,
  top_n_pca = 20000,
  k = 50,
  threshold = 0.9
)
}
\arguments{
\item{trainer}{A S3 object of \code{YamatClassifierTrainer} class.}

\item{batch_name}{column name of batch in the \code{targets} attribute of
\code{YamatClassifierTrainer} object.}

\item{classification_name}{column name of classification in the \code{targets}
attribute of \code{YamatClassifierTrainer} object.}

\item{top_n_rle}{an integer of the most variable N loci for relative log
expression (RLE) analysis.}

\item{rle_downsample}{if there are many samples, down-sample it to a number
while keep the proportion of each group.}

\item{top_n_pca}{an integer of the most variable N loci for PCA.}

\item{k}{Number of eigenvalues requested.}

\item{threshold}{A numeric scalar between 0 to 1 of the threshold of
the fraction of variance to choose PC number. Default to 0.9.}
}
\description{
Investigate if batch effect exists.
}
\details{
\enumerate{
    \item Center and scale.
    \item Compute the correlation/covariance matrix.
    \item Calculate the eigenvectors and eigenvalues.
    \item Choose the PC number. I use Capper's method and fraction of
      variance to calculate PC numbers and choose the bigger one
      from the two methods. See details at \code{\link{find_pc_number.capper}}
      and \code{\link{find_pc_number.var_frac}}.
    \item Project the scaled input matrix onto the new basis.
  }
}
