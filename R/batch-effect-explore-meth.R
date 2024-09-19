#' Explore batch effect meth
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
#' @param meth_pca_pdf PDF file for meth PCA result.
#' @return NULL
explore_batch_effect_meth <- function(trainer,
                                      batch_name = "Batch",
                                      classification_name = "Classification",
                                      top_n_rle = 20000,
                                      top_n_pca = 20000,
                                      rle_downsample = 100,
                                      k = 50,
                                      threshold = 0.9,
                                      meth_pca_pdf = "meth_pca.pdf") {
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
  # RLE
  logger::log_debug("meth RLE")
  meth_rle_plot <- explore_batch_effect_meth_rle(
    trainer,
    batch_name = batch_name,
    top_n = top_n_rle,
    n_samples = rle_downsample
  )
  meth_rle_plot_file <- glue::glue("{batch_prefix}_meth_rle.pdf")
  ggplot2::ggsave(
    filename = file.path(batch_effect_explore_dir, meth_rle_plot_file),
    plot = meth_rle_plot,
    height = 7,
    width = 15
  )
  # PCA
  logger::log_debug("meth PCA")
  logger::log_debug("Computing PCA")
  pca_result <- explore_batch_effect_meth_pca_compute(
    trainer = trainer,
    batch_name = batch_name,
    top_n = top_n_pca,
    k = k,
    threshold = threshold
  )
  targets <- get_targets(trainer = trainer)
  plot_subtitle <- glue::glue("Most variable {top_n_pca} loci. Log2 transformed (offset by 1 to avoid zeros).")
  p <- plot_pca_result(
    pca_result,
    targets,
    batch_name = batch_name,
    classification_name = classification_name,
    plot_title = "PCA of meth",
    plot_subtitle = plot_subtitle
  )
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
  meth_pca_pdf <- glue::glue("{batch_prefix}_{meth_pca_pdf}")
  meth_pca_pdf <- file.path(batch_effect_explore_dir, meth_pca_pdf)
  ggplot2::ggsave(
    filename = meth_pca_pdf,
    plot = p,
    height = 8,
    width = 8
  )
}


#' Explore batch effect meth - RLE.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param top_n an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param n_samples if there are many samples, down-sample it to a number
#'   while keep the proportion of each group.
#' @return a \code{\link[ggplot2]{ggplot}} object.
explore_batch_effect_meth_rle <- function(trainer,
                                          batch_name = "Batch",
                                          top_n = 20000,
                                          n_samples = 100) {
  targets <- get_targets(trainer = trainer)
  if (!(batch_name %in% colnames(targets))) {
    stop(glue::glue("Batch name column {batch_name} not in the targets"))
  }
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  # RLE
  meth <- get_meth(trainer = trainer)
  meth_rle <- compute_relative_log_expression(meth, top_n = top_n)
  plot_subtitle <- glue::glue("Most variable {top_n} loci. Log2 transformed (offset by 1 to avoid zeros).")
  if (ncol(meth_rle) > n_samples) {
    plot_subtitle <- glue::glue("{plot_subtitle} {n_samples} samples are sampled.")
  }
  meth_rle_plot <- plot_relative_log_expression(
    meth_rle,
    pheno = targets,
    batch_name = batch_name,
    batch_label = batch_name,
    plot_title = "Relative Log Expression - Meth",
    plot_subtitle = plot_subtitle,
    n_samples = n_samples
  )
  return(meth_rle_plot)
}


#' Perform PCA on meth
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param top_n an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param k Number of eigenvalues requested.
#' @param seed A numeric number of seed used for Capper's method of
#'   determining PC number. In brief, the method shuffles features
#'   across samples and determine the PC number by comparing the
#'   maximum of eigen values from the randomization.
#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance to choose PC number. Default to 0.9.
#' @param meth_pca_rda Rda file for meth PCA result.
#' @return a \code{\link[ggplot2]{ggplot}} object.
explore_batch_effect_meth_pca_compute <- function(trainer,
                                                  batch_name,
                                                  top_n = 20000,
                                                  k = 50,
                                                  seed = 1,
                                                  threshold = 0.9,
                                                  meth_pca_rda = "meth_pca.Rda") {
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
  meth_pca_rda <- glue::glue("{batch_prefix}_{meth_pca_rda}")
  meth_pca_rda <- file.path(batch_effect_explore_dir, meth_pca_rda)
  if (file.exists(meth_pca_rda)) {
    logger::log_info("Reading existing meth_pca Rda file")
    load(meth_pca_rda)
  } else {
    x <- get_meth(trainer = trainer)
    logger::log_info(glue::glue("PCA computing..."))
    logger::log_info("Log2 transformed with offset by 1")
    x <- log2(x + 1)
    logger::log_info(glue::glue("Get {top_n} most variable"))
    x_mv <- most_variable(x, top_n = top_n)
    t(x_mv) %>%
      pca(
        x = .,
        k = k,
        seed = seed,
        threshold = threshold
      ) -> meth_pca
    meth_pca$pca123$scaled <- NULL
    meth_pca$pca123$cor_mat <- NULL
    save(meth_pca, file = meth_pca_rda)
  }
  return(meth_pca)
}
