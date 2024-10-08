% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch-effect-explore-beta.R
\name{explore_batch_effect_beta_value}
\alias{explore_batch_effect_beta_value}
\title{Explore batch effect beta_value}
\usage{
explore_batch_effect_beta_value(
  trainer,
  batch_name = "Batch",
  classification_name = "Classification",
  top_n_rle = 20000,
  top_n_pca = 20000,
  rle_downsample = 100,
  k = 50,
  threshold = 0.9,
  beta_value_pca_pdf = "beta_value_pca.pdf"
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

\item{top_n_pca}{an integer of the most variable N loci for PCA.}

\item{rle_downsample}{if there are many samples, down-sample it to a number
while keep the proportion of each group.}

\item{k}{Number of eigenvalues requested.}

\item{threshold}{A numeric scalar between 0 to 1 of the threshold of
the fraction of variance to choose PC number. Default to 0.9.}

\item{beta_value_pca_pdf}{PDF file for beta_value PCA result.}
}
\description{
Explore batch effect beta_value
}
