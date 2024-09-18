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
  ggplot2::ggplot(data = x, ggplot2::aes(x = Basename, y = Value, fill = Batch)) +
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
