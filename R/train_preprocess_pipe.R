#' Train preprocess pipeline
#'
#' Preprocess for the training/reference data set from IDAT file to beta values.
#' Notice batch effect correction is not included.
#'
#' @param idat_dir IDAT file directory.
#' @param targets \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
#' @param output output directory.
#' @param overwrite A bool if overwrite the result files.
#' @return A S3 object of \code{YamatClassifierTrainer} class.
#' @export
train_preprocess_pipe <- function(idat_dir, targets, output, overwrite = FALSE) {
  logger::log_info("Creating YamatClassifierTrainer object")
  trainer <- create_trainer(
    idat_dir = idat_dir,
    targets = targets,
    output = output,
    overwrite = overwrite
  )
  logger::log_info("Preprocessing IDAT files")
  preprocess_dkfz(trainer = trainer)
  logger::log_info("Getting probe IDs (excluding SNPs, sex chromosomes, array type specific probes")
  trainer$probes <- get_probes(
    trainer = trainer,
    chip_type_name = NULL,
    present_by_epic_v2 = TRUE
  )
  logger:log_info("Calculating meth, unmeth and beta")
  beta_value <- get_beta(trainer = trainer)
  logger:log_info("Saving YamatClassifierTrainer")
  trainer_rda <- get_trainer_rda(trainer = trainer)
  save(trainer, file = trainer_rda)
  logger::log_info("DONE")
  return(trainer)
}
