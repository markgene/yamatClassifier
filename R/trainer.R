#' Create a YamatClassifierTrainer object.
#'
#' @param idat_dir IDAT file directory.
#' @param targets \code{data.frame} of targets. See \code{\link[minfi]{read.metharray.exp}}.
#' @param output output directory.
#' @param probes a character vector of probes.
#' @param beta_offset offset to calculate beta ratio.
#' @param probes_rda probes Rda file.
#' @param preprocessed_dir preprocessed file directory.
#' @param batch_effect_explore_dir batch effect exploratory analysis directory.
#' @param meth_rda meth Rda file.
#' @param unmeth_rda unmeth Rda file.
#' @param beta_value_rda beta value Rda file.
#' @param beta_value_adjusted_rda adjusted beta value Rda file.
#' @param trainer_rda \code{YamatClassifierTrainer} object Rda file.
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
                           batch_effect_explore_dir = "batch_effect_explore",
                           meth_rda = "meth.Rda",
                           unmeth_rda = "unmeth.Rda",
                           beta_value_rda = "beta_value.Rda",
                           beta_value_adjusted_rda = "beta_value_adjusted.Rda",
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
    batch_effect_explore_dir = batch_effect_explore_dir,
    meth_rda = meth_rda,
    unmeth_rda = unmeth_rda,
    beta_value_rda = beta_value_rda,
    beta_value_adjusted_rda = beta_value_adjusted_rda,
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
#' @export
get_preprocessed_dir <- function(trainer) {
  preprocessed_dir <- file.path(trainer$output, trainer$preprocessed_dir)
  if (!dir.exists(preprocessed_dir)) {
    dir.create(preprocessed_dir, recursive = TRUE)
  }
  return(preprocessed_dir)
}


#' Get the directory of batch effect exploratory analysis.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return path of the directory of batch effect exploratory analysis.
#' @export
get_batch_effect_explore_dir <- function(trainer) {
  batch_effect_explore_dir <- file.path(trainer$output, trainer$batch_effect_explore_dir)
  if (!dir.exists(batch_effect_explore_dir)) {
    dir.create(batch_effect_explore_dir, recursive = TRUE)
  }
  return(batch_effect_explore_dir)
}


#' Get probe ID file directory.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return path of probe ID file.
#' @export
get_probes_rda <- function(trainer) {
  file.path(trainer$output, trainer$probes_rda)
}


#' Get adjusted beta value Rda file path.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return path of adjusted beta value Rda file.
#' @export
get_beta_value_adjusted_rda <- function(trainer) {
  file.path(trainer$output, trainer$beta_value_adjusted_rda)
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
#' @export
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
  meth_by_sentrix_id <- lapply(seq(length(sentrix_ids)), function(i) {
    sentrix_id <- sentrix_ids[i]
    logger::log_info(glue::glue("Processing {sentrix_id} ({i}/{length(sentrix_ids)})..."))
    df <- targets[targets$Sentrix_ID == sentrix_id, ]
    rda_file_name <- paste0(sentrix_id, ".Rda")
    mset_rda <- file.path(preprocessed_dir, rda_file_name)
    if (file.exists(mset_rda)) {
      logger::log_info("Rda file exists")
      load(mset_rda)
      logger::log_debug("mset has {ncol(mset)} samples and {nrow(mset)} loci")
      mset_flt <- mset[probes, ]
      return(minfi::getMeth(mset_flt))
    } else {
      stop(paste(mset_rda, "file not exist"))
      return(NULL)
    }
  })
  meth <- do.call(cbind, meth_by_sentrix_id)
  # Filter samples in the targets, - excluding samples in the same chip but not in the targets
  meth <- meth[, colnames(meth) %in% targets$Basename]
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
  unmeth_by_sentrix_id <- lapply(seq(length(sentrix_ids)), function(i) {
    sentrix_id <- sentrix_ids[i]
    logger::log_info(glue::glue("Processing {sentrix_id} ({i}/{length(sentrix_ids)})..."))
    df <- targets[targets$Sentrix_ID == sentrix_id, ]
    rda_file_name <- paste0(sentrix_id, ".Rda")
    mset_rda <- file.path(preprocessed_dir, rda_file_name)
    if (file.exists(mset_rda)) {
      logger::log_info("Rda file exists")
      load(mset_rda)
      logger::log_debug("mset has {ncol(mset)} samples and {nrow(mset)} loci")
      mset_flt <- mset[probes, ]
      return(minfi::getUnmeth(mset_flt))
    } else {
      stop(paste(mset_rda, "file not exist"))
      return(NULL)
    }
  })
  unmeth <- do.call(cbind, unmeth_by_sentrix_id)
  # Filter samples in the targets, - excluding samples in the same chip but not in the targets
  unmeth <- unmeth[, colnames(unmeth) %in% targets$Basename]
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
get_beta_value <- function(trainer) {
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
  logger::log_debug(
    glue::glue(
      "beta_value has {ncol(beta_value)} samples and {nrow(beta_value)} loci"
    )
  )
  return(beta_value)
}


#' Get adjusted beta value.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @return a matrix of beta value.
#' @export
get_beta_value_adjusted <- function(trainer) {
  beta_value_adjusted_rda <- get_beta_value_adjusted_rda(trainer = trainer)
  if (file.exists(beta_value_adjusted_rda)) {
    logger::log_info(glue::glue(
      "Reading existing beta Rda file {beta_value_adjusted_rda}..."
    ))
    load(beta_value_adjusted_rda)
    return(beta_value_adjusted)
  } else {
    stop("fail to find Rda file of adjusted beta value")
  }
}


#' Computing t-SNE with fit-SNE algorithm.
#'
#' Perform t-SNE with fit-SNE algorithm using \code{\link[snifter]{fitsne}} in
#' snifter package.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param top_n an integer of the most variable N loci for t-SNE
#' @param perplexity Numeric scalar controlling the neighborhood used when
#'   estimating the embedding. Default to 30.
#' @param n_iter Integer scalar specifying the number of iterations to complete.
#'   Default to 3000.
#' @param random_state Integer scalar specifying the seed used by the random
#'   number generator.
#' @param pca Logical scalar specifying whether PCA should be run on the data
#'   before creating the embedding.
#' @param save_result Logical scalar specifying whether result should be saved
#'   in Rda file.
#' @param ... other arguments of \code{\link[snifter]{fitsne}}.
#' @return A matrix of t-SNE embeddings.
#' @details
#'   Notice set \code{pca} argument as default (FALSE). Otherwise,
#'   \code{\link[snifter]{project}} will not work.
#' @export
run_fitsne <- function(trainer,
                       top_n = 10000,
                       perplexity = 30,
                       n_iter = 3000,
                       random_state = 123,
                       pca = FALSE,
                       save_result = TRUE,
                       ...) {
  beta_value_adjusted <- get_beta_value_adjusted(trainer = trainer)
  logger::log_info(glue::glue("Getting the {top_n} most variable loci..."))
  beta_vals_top_n <- most_variable(beta_value_adjusted, top_n = top_n)
  embedding_rda <- glue::glue("fitsne_embedding_{top_n}_{perplexity}_{n_iter}_{random_state}_{pca}.Rda")
  embedding_rda <- file.path(trainer$output, embedding_rda)
  logger::log_info(
    glue::glue(
      "Performing fit-SNE. perplexity={perplexity}, n_iter={n_iter}, random_state={random_state}, pca={pca} ..."
    )
  )
  embedding <- snifter::fitsne(
    t(beta_vals_top_n),
    random_state = random_state,
    perplexity = perplexity,
    n_iter = n_iter,
    pca = pca,
    ...
  )
  rownames(embedding) <- colnames(beta_value_adjusted)
  colnames(embedding) <- c("tSNE1", "tSNE2")
  if (save_result) {
    logger::log_info("Saving embedding into Rda file")
    save(embedding, file = embedding_rda)
  }
  return(embedding)
}


#' Computing t-SNE with \code{\link[Rtsne]{Rtsne}}.
#'
#' Perform t-SNE with fit-SNE algorithm using \code{\link[Rtsne]{Rtsne}}.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param top_n an integer of the most variable N loci for t-SNE
#' @param perplexity Numeric scalar controlling the neighborhood used when
#'   estimating the embedding. Default to 30.
#' @param n_iter Integer scalar specifying the number of iterations to complete.
#'   Default to 3000.
#' @param random_state Integer scalar specifying the seed used by the random
#'   number generator.
#' @param pca Logical scalar specifying whether PCA should be run on the data
#'   before creating the embedding.
#' @param save_result Logical scalar specifying whether result should be saved
#'   in Rda file.
#' @param ... other arguments of \code{\link[Rtsne]{Rtsne}}.
#' @return A matrix of t-SNE embeddings.
#' @details
#'   Notice set \code{pca} argument as default (TRUE).
#' @export
run_rtsne <- function(trainer,
                      top_n = 10000,
                      perplexity = 30,
                      n_iter = 3000,
                      random_state = 123,
                      pca = TRUE,
                      verbose = TRUE,
                      save_result = FALSE,
                      ...) {
  beta_value_adjusted <- get_beta_value_adjusted(trainer = trainer)
  logger::log_info(glue::glue("Getting the {top_n} most variable loci..."))
  beta_vals_top_n <- most_variable(beta_value_adjusted, top_n = top_n)
  embedding_rda <- glue::glue("rtsne_embedding_{top_n}_{perplexity}_{n_iter}_{random_state}_{pca}.Rda")
  embedding_rda <- file.path(trainer$output, embedding_rda)
  logger::log_info(
    glue::glue(
      "Performing t-SNE. perplexity={perplexity}, max_iter={n_iter}, seed={random_state}, pca={pca} ..."
    )
  )
  set.seed(random_state)
  rtsne_result <- Rtsne::Rtsne(
    t(beta_vals_top_n),
    perplexity = perplexity,
    max_iter = n_iter,
    pca = pca,
    verbose = verbose,
    ...
  )
  embedding <- rtsne_result$Y
  rownames(embedding) <- colnames(beta_value_adjusted)
  colnames(embedding) <- c("tSNE1", "tSNE2")
  if (save_result) {
    logger::log_info("Saving embedding into Rda file")
    save(embedding, file = embedding_rda)
  }
  return(embedding)
}
