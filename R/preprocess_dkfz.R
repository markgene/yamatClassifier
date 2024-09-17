#' Preprocess DKFZ approach.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return output Rda files of preprocessed data of \code{\link{[minfi]MethySet}}
#'   class. One file per chip.
#' @export
preprocess_dkfz <- function(trainer) {
  sentrix_ids <- unique(trainer$targets$Sentrix_ID)
  preprocessed_dir <- get_preprocessed_dir(trainer)
  i <- 0
  sample_total <- 0
  create_total <- 0
  mset_rda_files <- character(length = length(sentrix_ids))
  targets <- trainer$targets
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
    mset_rda_files[i] <- mset_rda
    if (file.exists(mset_rda) && !trainer$overwrite) {
      logger::log_info("Rda file exists")
    } else {
      create_total <- create_total + 1
      logger::log_info("Read IDAT files into RGChannelSet")
      rgset <- yamat::read_metharray_exp(base = trainer$idat_dir,
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
      "Preprocessing is done: {i} chips and {sample_total} samples; {create_total} chips are new.."
    )
  )
  logger::log_info(glue::glue("Result files saved in directory {preprocessed_dir}."))
  return(mset_rda_files)
}
