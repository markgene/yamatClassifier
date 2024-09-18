# Explore batch effect.

#' Explore batch effect.
#'
#' Investigate if batch effect exists.
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param top_n_rle an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param rle_downsample if there are many samples, down-sample it to a number
#'   while keep the proportion of each group.
#' @return NULL
#' @details
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
#' @export
explore_batch_effect <- function(trainer,
                                 batch_name = "Batch",
                                 top_n_rle = 20000,
                                 rle_downsample = 100) {
  targets <- get_targets(trainer = trainer)
  if (!(batch_name %in% colnames(targets))) {
    stop(glue::glue("Batch name column {batch_name} not in the targets"))
  }
  explore_batch_effect_meth(
    trainer,
    batch_name = batch_name,
    top_n_rle = top_n_rle,
    rle_downsample = rle_downsample
  )
}

#' Explore batch effect meth
#'
#' @param trainer A S3 object of \code{YamatClassifierTrainer} class.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param top_n_rle an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param rle_downsample if there are many samples, down-sample it to a number
#'   while keep the proportion of each group.
#' @return NULL
explore_batch_effect_meth <- function(trainer,
                                      batch_name = "Batch",
                                      top_n_rle = 20000,
                                      rle_downsample = 100) {
  batch_effect_explore_dir <- get_batch_effect_explore_dir(trainer = trainer)
  batch_prefix <- make.names(batch_name)
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


#' Computing relative log expression
#'
#' @param x A matrix.
#' @param top_n an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param log2_transform perform log2 transform or not.
#' @param log2_offset offset by certain value to avoid zeros. Default to 1.
#' @return a matrix.
compute_relative_log_expression <- function(x,
                                            top_n,
                                            log2_transform = TRUE,
                                            log2_offset = 1) {
  if (log2_transform) {
    logger::log_info("Log2 transformed with offset by 1")
    x <- log2(x + 1)
  }
  logger::log_info(glue::glue("Get {top_n} most variable"))
  x_mv <- most_variable(x, top_n = top_n)
  # RLE - Relative Log Expression
  logger::log_info("Computing RLE")
  probe_medians <- apply(x, 1, median)
  x_mv_rle <- sweep(x_mv, 1, probe_medians[rownames(x_mv)], "-")
  rm(probe_medians)
  gc()
  return(x_mv_rle)
}


#' Plot relative log expression.
#'
#' @param x A matrix of RLE (RLE).
#' @param top_n an integer of the most variable N loci for relative log
#'   expression (RLE) analysis.
#' @param batch_name column name of batch in the \code{targets} attribute of
#'   \code{YamatClassifierTrainer} object.
#' @param batch_label batch label in the ggplot (color).
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle
#' @param seed seed for sampling.
#' @param n_samples if there are many samples, down-sample it to a number
#'   while keep the proportion of each group.
#' @return a \code{\link[ggplot2]{ggplot}} object.
plot_relative_log_expression <- function(x,
                                         pheno,
                                         batch_name,
                                         batch_label,
                                         plot_title,
                                         plot_subtitle,
                                         seed = 123,
                                         n_samples = 100) {
  # RLE - sample prep type
  set.seed(seed)
  pheno$Batch <- pheno[, batch_name]
  if (nrow(pheno) <= n_samples) {
    sampled <- pheno
  } else {
    frac <- n_samples / nrow(pheno)
    sampled <- reference_pheno_df %>%
      dplyr::group_by(Batch) %>%
      dplyr::sample_frac(frac) %>%
      dplyr::arrange(Batch) %>%
      dplyr::ungroup()
  }
  x_sampled <- x[, sampled$Basename]
  x <- as.data.frame(t(x_sampled)) %>%
    tibble::rownames_to_column(var = "Basename") %>%
    dplyr::left_join(sampled[, c("Basename", "Batch")], by = "Basename") %>%
    tidyr::pivot_longer(
      cols = -c(Basename, Batch),
      names_to = "cg",
      values_to = "Value"
    ) %>%
    dplyr::mutate(Basename = factor(Basename, levels = sampled$Basename))
  ggplot2::ggplot(data = x, ggplot2::aes(x = Basename, y = Value, fill =
                                           Batch)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::labs(fill = batch_label) +
    viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      # legend.key.height = unit(0.5, units = "in"),
      # legend.key.width = unit(0.3, units = "in"),
      axis.text.x = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, size = 10),
      strip.text.x = ggplot2::element_text(size = 10)
    ) +
    patchwork::plot_annotation(
      title = plot_title,
      subtitle = plot_subtitle,
      theme = ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = 9))
    )
}
