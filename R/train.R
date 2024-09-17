#' Train pipeline
#'
#' @param idat_dir IDAT file directory.
#' @param targets \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
#' @param output output directory.
#' @param overwrite A bool if overwrite the result files.
#' @return A S3 object of \code{YamatClassifierTrainer} class.
#' @export
train <- function(idat_dir, targets, output, overwrite = FALSE) {
  logger::log_debug("Creating YamatClassifierTrainer object")
  trainer <- create_trainer(
    idat_dir = idat_dir,
    targets = targets,
    output = output,
    overwrite = overwrite
  )
  logger::log_debug("Preprocessing IDAT files")
  preprocess_dkfz(trainer = trainer)
  logger::log_debug("Getting probe IDs (excluding SNPs, sex chromosomes, array type specific probes")
  trainer$probes <- get_probes(
    trainer = trainer,
    chip_type_name = NULL,
    present_by_epic_v2 = TRUE
  )
  return(trainer)
}
