# Pipeline - train classifier

#' Create a YamatClassifierTrainer object.
#'
#' @param x A matrix which has columns as features and rows as samples.
#' @param k Number of eigenvalues requested.
#' @param seed A numeric number of seed used for Capper's method of
#'   determining PC number. In brief, the method shuffles features
#'   across samples and determine the PC number by comparing the
#'   maximum of eigen values from the randomization.
#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance to choose PC number. Default to 0.9.
#' @return A list of four elements:
#'   \itemize{
#'     \item \code{projected} The result matrix of PCA analysis.
#'     \item \code{pca123} A list of result from step 1-3 returned by
#'       \code{\link{pca123}}.
#'     \item \code{capper} A list of the result of choosing PC number
#'       by Capper's method, returned by \code{\link{find_pc_number.capper}}.
#'     \item \code{vf} A list of the result of choosing PC number
#'       by fraction of variance, returned by \code{\link{find_pc_number.var_frac}}.
#'   }
#' @details The function wraps up the following five steps of PCA:
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
#'   I use \code{\link[RSpectra]{eigs}} function in Rspectra package
#'   instead of \code{\link[base]{eigen}} function in base to deal
#'   with large matrix.
#' @export
create_trainer <- function(idat_dir, targets, output, overwrite = FALSE) {
  if (is.null(targets$Sentrix_ID)) {
    stop("Sentrix_ID is required for targets")
  }
  trainer <- list(
    idat_dir = idat_dir,
    targets = targets,
    output = output,
    overwrite = overwrite
  )
  class(trainer) <- "YamatClassifierTrainer"
  return(trainer)
}


#' Get preprocessed file directory.
get_preprocessed_dir <- function(trainer, dir_name = "dkfz_preprocessed") {
  preprocessed_dir <- file.path(trainer$output, dir_name)
  if (!dir.exists(preprocessed_dir)) {
    dir.create(preprocessed_dir, recursive = TRUE)
  }
  return(preprocessed_dir)
}



#' Preprocess.
preprocess_dkfz <- function(trainer) {
  sentrix_ids <- unique(trainer$targets$Sentrix_ID)
  preprocessed_dir <- get_preprocessed_dir((trainer))
  i <- 0
  sample_total <- 0
  create_total <- 0
  for (sentrix_id in sentrix_ids) {
    i <- i + 1
    df <- targets[targets$Sentrix_ID == sentrix_id, ]
    sample_total <- sample_total + nrow(df)
    logger::log_info(
      glue::glue(
        "Processing {sentrix_id} ({i}/{length(sentrix_ids)}): {nrow(df)} samples"
      )
    )
    rda_file_name <- paste0(sentrix_id, ".Rda")
    mset_rda <- file.path(preprocessed_dir, rda_file_name)
    if (file.exists(mset_rda) && !trainer$overwrite) {
      logger::log_info("Rda file exists")
    } else {
      create_total <- create_total + 1
      logger::log_info("Read IDAT files into RGChannelSet")
      rgset <- yamat::read_metharray_exp(base = idat_dir,
                                         targets = targets,
                                         force = FALSE)
      logger::log_info("Create MethylSet by normalizing RGChannelSet")
      mset <- yamat::normalize(rgset,
                               norm_method = "dkfz",
                               map_to_genome = FALSE)
      logger::log_info("Saving MethylSet into Rda file")
      save(mset, file = mset_rda)
    }
  }
  logger::log_info(
    glue::glue(
      "Preprocessing is done: {i} chips and {sample_count} samples; {create_total} chips are new.."
    )
  )
  logger::log_info(glue::glue("Result files saved in directory {preprocessed_dir}."))
}



#' Train pipeline
train <- function(idat_dir, targets, output, overwrite = FALSE) {
  trainer <- create_trainer(
    idat_dir = idat_dir,
    targets = targets,
    output = output,
    overwrite = overwrite
  )
  preprocess_dkfz(trainer = trainer)
  return(trainer)
}
