#' Create a YamatClassifierTrainer object.
#'
#' @param idat_dir IDAT file directory.
#' @param targets \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
#' @param output output directory.
#' @param overwrite A bool if overwrite the result files.
#' @return A S3 object of \code{YamatClassifierTrainer} class.
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
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param dir_name output directory name.
#' @return path of preprocessed file directory.
get_preprocessed_dir <- function(trainer, dir_name = "dkfz_preprocessed") {
  preprocessed_dir <- file.path(trainer$output, dir_name)
  if (!dir.exists(preprocessed_dir)) {
    dir.create(preprocessed_dir, recursive = TRUE)
  }
  return(preprocessed_dir)
}


#' Get probe ID file directory.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param file_name output file name.
#' @return path of probe ID file.
get_probe_ids_rda <- function(trainer, file_name = "probe_ids.Rda") {
  file.path(trainer$output, file_name)
}


#' Get targets.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
get_targets <- function(trainer) {
  trainer$targets
}
