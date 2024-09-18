#' Create a YamatClassifierTrainer object.
#'
#' @param idat_dir IDAT file directory.
#' @param targets \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
#' @param output output directory.
#' @param probes a character vector of probes.
#' @param beta_offset offset to calculate beta ratio.
#' @param probes_rda probes Rda file.
#' @param preprocessed_dir preprocessed file directory.
#' @param meth_rda meth Rda file.
#' @param overwrite A bool if overwrite the result files.
#' @return A S3 object of \code{YamatClassifierTrainer} class.
#' @export
create_trainer <- function(idat_dir,
                           targets,
                           output,
                           probes = NULL,
                           beta_offset = 100,
                           probes_rda = "probes.Rda",
                           preprocessed_dir = "dkfz_preprocessed",
                           meth_rda = "meth.Rda",
                           unmeth_rda = "unmeth.Rda",
                           beta_value_rda = "beta_value.Rda",
                           trainer_rda = "trainer.Rda",
                           overwrite = FALSE) {
  if (is.null(targets$Sentrix_ID)) {
    stop("Sentrix_ID is required for targets")
  }
  trainer <- list(
    idat_dir = idat_dir,
    targets = targets,
    output = output,
    probes = probes,
    beta_offset = beta_offset,
    probes_rda = probes_rda,
    preprocessed_dir = preprocessed_dir,
    meth_rda = meth_rda,
    unmeth_rda = unmeth_rda,
    beta_value_rda = beta_value_rda,
    trainer_rda = trainer_rda,
    overwrite = overwrite
  )
  class(trainer) <- "YamatClassifierTrainer"
  return(trainer)
}


#' Get preprocessed file directory.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return path of preprocessed file directory.
get_preprocessed_dir <- function(trainer) {
  preprocessed_dir <- file.path(trainer$output, trainer$preprocessed_dir)
  if (!dir.exists(preprocessed_dir)) {
    dir.create(preprocessed_dir, recursive = TRUE)
  }
  return(preprocessed_dir)
}


#' Get probe ID file directory.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return path of probe ID file.
get_probes_rda <- function(trainer) {
  file.path(trainer$output, trainer$probes_rda)
}


#' Get YamatClassifierTrainer Rda file.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return path of YamatClassifierTrainer Rda file.
#' @export
get_trainer_rda <- function(trainer) {
  file.path(trainer$output, trainer$trainer_rda)
}


#' Get targets.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
get_targets <- function(trainer) {
  trainer$targets
}


#' Get meth.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return a matrix of meth
#' @export
get_meth <- function(trainer) {
  meth_rda <- file.path(trainer$output, trainer$meth_rda)
  if (file.exists(meth_rda)) {
    logger::log_info("Reading existing meth Rda file")
    load(meth_rda)
  } else {
    logger::log_info("Getting meth from preprocessed files")
    meth <- get_meth_from_preprocessed_files(trainer = trainer)
    save(meth, file = meth_rda)
  }
  return(meth)
}


#' Get meth from preprocessed files.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return a matrix of meth
#' @export
get_meth_from_preprocessed_files <- function(trainer) {
  targets <- get_targets(trainer)
  sentrix_ids <- unique(targets$Sentrix_ID)
  preprocessed_dir <- get_preprocessed_dir(trainer)
  probes <- get_probes(trainer = trainer)
  meth_by_sentrix_id <- lapply(sentrix_ids, function(sentrix_id) {
    logger::log_info(paste("Processing", sentrix_id))
    df <- targets[targets$Sentrix_ID == sentrix_id, ]
    rda_file_name <- paste0(sentrix_id, ".Rda")
    mset_rda <- file.path(preprocessed_dir, rda_file_name)
    if (file.exists(mset_rda)) {
      logger::log_info("Rda file exists")
      load(mset_rda)
    } else {
      stop(paste(mset_rda, "file not exist"))
    }
    mset_flt <- mset[probes, ]
    minfi::getMeth(mset_flt)
  })
  meth <- do.call(cbind, meth_by_sentrix_id)
  # dim(meth)
  rm(meth_by_sentrix_id)
  gc()
  meth_rda <- file.path(trainer$output, trainer$meth_rda)
  logger::log_debug(glue::glue("meth has {ncol(meth)} samples and {nrow(meth)} loci"))
  save(meth, file = meth_rda)
  return(meth)
}


#' Get unmeth.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return a matrix of unmeth
#' @export
get_unmeth <- function(trainer) {
  unmeth_rda <- file.path(trainer$output, trainer$unmeth_rda)
  if (file.exists(unmeth_rda)) {
    logger::log_info("Reading existing unmeth Rda file")
    load(unmeth_rda)
  } else {
    logger::log_info("Getting unmeth from preprocessed files")
    unmeth <- get_unmeth_from_preprocessed_files(trainer = trainer)
    save(unmeth, file = unmeth_rda)
  }
  return(unmeth)
}


#' Get unmeth from preprocessed files.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return a matrix of unmeth
#' @export
get_unmeth_from_preprocessed_files <- function(trainer) {
  targets <- get_targets(trainer)
  sentrix_ids <- unique(targets$Sentrix_ID)
  preprocessed_dir <- get_preprocessed_dir(trainer)
  probes <- get_probes(trainer = trainer)
  unmeth_by_sentrix_id <- lapply(sentrix_ids, function(sentrix_id) {
    logger::log_info(paste("Processing", sentrix_id))
    df <- targets[targets$Sentrix_ID == sentrix_id, ]
    rda_file_name <- paste0(sentrix_id, ".Rda")
    mset_rda <- file.path(preprocessed_dir, rda_file_name)
    if (file.exists(mset_rda)) {
      logger::log_info("Rda file exists")
      load(mset_rda)
    } else {
      stop(paste(mset_rda, "file not exist"))
    }
    mset_flt <- mset[probes, ]
    minfi::getUnmeth(mset_flt)
  })
  unmeth <- do.call(cbind, unmeth_by_sentrix_id)
  # dim(unmeth)
  rm(unmeth_by_sentrix_id)
  gc()
  unmeth_rda <- file.path(trainer$output, trainer$unmeth_rda)
  logger::log_debug(glue::glue("unmeth has {ncol(unmeth)} samples and {nrow(unmeth)} loci"))
  save(unmeth, file = unmeth_rda)
  return(unmeth)
}


#' Get beta value.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return a matrix of beta value.
#' @export
get_beta <- function(trainer) {
  beta_value_rda <- file.path(trainer$output, trainer$beta_value_rda)
  if (file.exists(beta_value_rda)) {
    logger::log_info("Reading existing beta Rda file...")
    load(beta_value_rda)
  } else {
    logger::log_info("Calculating beta value...")
    meth <- get_meth(trainer = trainer)
    unmeth <- get_unmeth(trainer = trainer)
    beta_value <- meth / (meth + unmeth + trainer$beta_offset)
    save(beta_value, file = beta_value_rda)
  }
  return(beta_value)
}
