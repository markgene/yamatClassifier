% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_model3.R
\name{train_model3}
\alias{train_model3}
\title{Train model 3}
\usage{
train_model3(
  dat,
  response_name,
  outer_cv_folds = 5,
  inner_cv_folds = 5,
  random_state = 56,
  mtry = NULL,
  selected_features = NULL,
  importance = "permutation",
  save_level = 3,
  save_prefix = "train_model3_",
  overwrite = FALSE,
  output = NULL,
  verbose = TRUE
)
}
\arguments{
\item{dat}{a \code{data.frame} of input data.}

\item{response_name}{column name of the response.}

\item{outer_cv_folds}{outer cross-validation fold number.}

\item{inner_cv_folds}{inner cross-validation fold number.}

\item{random_state}{random seed.}

\item{mtry}{A vector of mtry for parameter tuning.}

\item{selected_features}{features selected.}

\item{importance}{\code{importance} of \code{\link[ranger]{ranger}}.}

\item{save_level}{if save_level > 0, save outer train index. If save_level > 1,
save calibrated probabilities and selected features in addition.}

\item{save_prefix}{output file prefix.}

\item{overwrite}{overwrite existing result files or not.}

\item{output}{output directory.}

\item{verbose}{A bool.}
}
\value{
a list of cross-validation result of given \code{mtry} values.
}
\description{
Train model 3
}
\details{
Key steps:
  \enumerate{
    \item Tuning loop tunes single parameter \code{mtry}.
    \item Outer cross-validation split the data set into training and testing
      set of M folds.
    \item Inner cross-validation split the training set of the outer CV into
      N folds. Each fold does random forest classification. When all folds
      are done. Train calibration model of Ridge multinomial logistic
      regression (MR) regression. The lambda is trained with
      \code{\link[glmnet]{cv.glmnet}}. The random forest and calibration
      models are used for the testing set of the outer CV.
  }
}
