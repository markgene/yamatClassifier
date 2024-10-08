% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature-selection.R
\name{select_features_boruta2}
\alias{select_features_boruta2}
\title{Perform Boruta feature selection with Boruta result.}
\usage{
select_features_boruta2(dat, response_name, with_tentative = FALSE, ...)
}
\arguments{
\item{dat}{a \code{data.frame} of input data.}

\item{response_name}{column name of the response.}

\item{with_tentative}{if set to \code{TRUE}, Tentative attributes will be
also returned. See \code{\link[Boruta]{getSelectedAttributes}}.}

\item{...}{parameters for \code{\link[Boruta]{Boruta}}.}
}
\value{
a list of two items: a vector of selected features and a \code{Boruta}
  object.
}
\description{
Perform Boruta feature selection with Boruta result.
}
