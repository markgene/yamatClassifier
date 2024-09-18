# Explore batch effect.

#' Explore batch effect.
#'
#' Investigate if batch effect exists.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param classification_name column name of classification in the \code{targets}
#'   attribute of \code{YamatClassifierTrainer} object.
#' @param top_n_rle an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param top_n_pca an integer of the most variable N loci for PCA.
#' @param rle_downsample if there are many samples, down-sample it to a number
#'   while keep the proportion of each group.
#' @param k Number of eigenvalues requested.
#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance to choose PC number. Default to 0.9.
#' @param classification_name column name of classification in the \code{targets}
#'   attribute of \code{YamatClassifierTrainer} object.
#' @return NULL
#' @details
#'    \enumerate{
#'     \item Center and scale.
#'     \item Compute the correlation/covariance matrix.
#'     \item Calculate the eigenvectors and eigenvalues.
#'     \item Choose the PC number. I use Capper's method and fraction of
#'       variance to calculate PC numbers and choose the bigger one
#'       from the two methods. See details at \code{\link{find_pc_number.capper}}
#'       and \code{\link{find_pc_number.var_frac}}.
#'     \item Project the scaled input matrix onto the new basis.
#'   }
#' @export
explore_batch_effect <- function(trainer,
                                 batch_name = "Batch",
                                 classification_name = "Classification",
                                 top_n_rle = 20000,
                                 rle_downsample = 100,
                                 top_n_pca = 20000,
                                 k = 50,
                                 threshold = 0.9) {
  targets <- get_targets(trainer = trainer)
  if (!(batch_name %in% colnames(targets))) {
    stop(glue::glue("Batch name column {batch_name} not in the targets"))
  }
  explore_batch_effect_meth(
    trainer,
    batch_name = batch_name,
    top_n_rle = top_n_rle,
    rle_downsample = rle_downsample,
    top_n_pca = top_n_pca,
    k = k,
    threshold = threshold,
    classification_name = classification_name
  )
  explore_batch_effect_unmeth(
    trainer,
    batch_name = batch_name,
    top_n_rle = top_n_rle,
    rle_downsample = rle_downsample,
    top_n_pca = top_n_pca,
    k = k,
    threshold = threshold,
    classification_name = classification_name
  )
  explore_batch_effect_beta(
    trainer,
    batch_name = batch_name,
    top_n_rle = top_n_rle,
    rle_downsample = rle_downsample,
    top_n_pca = top_n_pca,
    k = k,
    threshold = threshold,
    classification_name = classification_name
  )
}

