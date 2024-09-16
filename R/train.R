#' Train pipeline
#'
#' @param idat_dir IDAT file directory.
#' @param targets \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
#' @param output output directory.
#' @param overwrite A bool if overwrite the result files.
#' @return A S3 object of \code{YamatClassifierTrainer} class.
#' @export
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
