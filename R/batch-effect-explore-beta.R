#' Explore batch effect beta_value
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
#' @param beta_value_pca_pdf PDF file for beta_value PCA result.
#' @return NULL
explore_batch_effect_beta_value <- function(trainer,
                                            batch_name = "Batch",
                                            classification_name = "Classification",
                                            top_n_rle = 20000,
                                            top_n_pca = 20000,
                                            rle_downsample = 100,
                                            k = 50,
                                            threshold = 0.9,
                                            beta_value_pca_pdf = "beta_value_pca.pdf") {
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
  # RLE
  logger::log_debug("beta_value RLE")
  beta_value_rle_plot <- explore_batch_effect_beta_value_rle(
    trainer,
    batch_name = batch_name,
    top_n = top_n_rle,
    n_samples = rle_downsample
  )
  beta_value_rle_plot_file <- glue::glue("{batch_prefix}_beta_value_rle.pdf")
  ggplot2::ggsave(
    filename = file.path(batch_effect_explore_dir, beta_value_rle_plot_file),
    plot = beta_value_rle_plot,
    height = 7,
    width = 15
  )
  # PCA
  logger::log_debug("beta_value PCA")
  logger::log_debug("Computing PCA")
  pca_result <- explore_batch_effect_beta_value_pca_compute(
    trainer = trainer,
    batch_name = batch_name,
    top_n = top_n_pca,
    k = k,
    threshold = threshold
  )
  targets <- get_targets(trainer = trainer)
  plot_subtitle <- glue::glue("Most variable {top_n_pca} loci.")
  p <- plot_pca_result(
    pca_result,
    targets,
    batch_name = batch_name,
    classification_name = classification_name,
    plot_title = "PCA of beta",
    plot_subtitle = plot_subtitle
  )
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
  beta_value_pca_pdf <- glue::glue("{batch_prefix}_{beta_value_pca_pdf}")
  beta_value_pca_pdf <- file.path(batch_effect_explore_dir, beta_value_pca_pdf)
  ggplot2::ggsave(
    filename = beta_value_pca_pdf,
    plot = p,
    height = 8,
    width = 8
  )
}


#' Explore batch effect beta_value - RLE.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param top_n an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param n_samples if there are many samples, down-sample it to a number
#'   while keep the proportion of each group.
#' @return a \code{\link[ggplot2]{ggplot}} object.
explore_batch_effect_beta_value_rle <- function(trainer,
                                                batch_name = "Batch",
                                                top_n = 20000,
                                                n_samples = 100) {
  targets <- get_targets(trainer = trainer)
  if (!(batch_name %in% colnames(targets))) {
    stop(glue::glue("Batch name column {batch_name} not in the targets"))
  }
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  # RLE
  beta_value <- get_beta_value(trainer = trainer)
  beta_value_rle <- compute_relative_log_expression(
    beta_value,
    top_n = top_n,
    log2_transform = FALSE,
    log2_offset = 0
  )
  plot_subtitle <- glue::glue("Most variable {top_n} loci. Log2 transformed (offset by 1 to avoid zeros).")
  if (ncol(beta_value_rle) > n_samples) {
    plot_subtitle <- glue::glue("{plot_subtitle} {n_samples} samples are sampled.")
  }
  beta_value_rle_plot <- plot_relative_log_expression(
    beta_value_rle,
    pheno = targets,
    batch_name = batch_name,
    batch_label = batch_name,
    plot_title = "Relative Log Expression - beta_value",
    plot_subtitle = plot_subtitle,
    n_samples = n_samples
  )
  return(beta_value_rle_plot)
}


#' Perform PCA on beta_value
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param top_n an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param k Number of eigenvalues requested.
#' @param seed A numeric number of seed used for Capper's beta_valueod of
#'   determining PC number. In brief, the beta_valueod shuffles features
#'   across samples and determine the PC number by comparing the
#'   maximum of eigen values from the randomization.
#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance to choose PC number. Default to 0.9.
#' @param beta_value_pca_rda Rda file for beta_value PCA result.
#' @return a \code{\link[ggplot2]{ggplot}} object.
explore_batch_effect_beta_value_pca_compute <- function(trainer,
                                                        batch_name,
                                                        top_n = 20000,
                                                        k = 50,
                                                        seed = 1,
                                                        threshold = 0.9,
                                                        beta_value_pca_rda = "beta_value_pca.Rda") {
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
  beta_value_pca_rda <- glue::glue("{batch_prefix}_{beta_value_pca_rda}")
  beta_value_pca_rda <- file.path(batch_effect_explore_dir, beta_value_pca_rda)
  if (file.exists(beta_value_pca_rda)) {
    logger::log_info("Reading existing beta_value_pca Rda file")
    load(beta_value_pca_rda)
  } else {
    x <- get_beta_value(trainer = trainer)
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
      ) -> beta_value_pca
    beta_value_pca$pca123$scaled <- NULL
    beta_value_pca$pca123$cor_mat <- NULL
    beta_value_pca$pca123$eigs$vectors <- NULL
    save(beta_value_pca, file = beta_value_pca_rda)
  }
  return(beta_value_pca)
}
