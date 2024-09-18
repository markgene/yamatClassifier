#' Plot PCA.
#'
#' @param pca_result An object of pca_result class.
#' @param pheno the \code{data.frame} of phenotype.
#' @param batch_name the column name of batch in the phenotype \code{data.frame}.
#' @param classification_name the column name of classification in the phenotype
#'   \code{data.frame}.
#' @param pc_x an integer of PC on x axis. Default to 1.
#' @param pc_y an integer of PC on y axis. Default to 2.
#' @param batch_label a character describe the ggplot2 label for batch. Default
#'   to "Batch".
#' @param classification_label a character describe the ggplot2 label for
#'   classification. Default to "Classification".
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @details Inspired by \code{PLSDAbatch} package.
#' @export
plot_pca_result <- function(pca_result,
                            pheno,
                            batch_name,
                            classification_name,
                            pc_x = 1,
                            pc_y = 2,
                            batch_label = "Batch",
                            classification_label = "Classification",
                            density_lwd = 0.6,
                            plot_title = "PCA",
                            plot_subtitle = "PCA subtitle") {
  projection <- pca_result$projected
  eigen_values <- pca_result$pca123$eigs$values
  frac_var <- eigen_values / sum(eigen_values)
  axis_labels <- sapply(1:length(frac_var), function(i) {
    glue::glue("PC{i} {round(frac_var[i] * 100)}% explained variance")
  })
  names(axis_labels) <- sapply(1:length(frac_var), function(i) {
    glue::glue("PC{i}")
  })
  pc_x_name <- glue::glue("PC{pc_x}")
  pc_y_name <- glue::glue("PC{pc_y}")
  plot_data <- prepare_plot_data(
    projection = projection,
    pheno = pheno,
    batch_name = batch_name,
    classification_name = classification_name,
    pc_x_name = pc_x_name,
    pc_y_name = pc_y_name
  )
  classifications <- unique(pheno[, classification_name])
  pal <- viridis::viridis_pal(option = "H")(length(classifications))
  names(pal) <- classifications
  main_plot <- get_main_pca_plot(
    plot_data,
    pal = pal,
    batch_label = batch_label,
    classification_label = classification_label,
    x_label = axis_labels[pc_x_name],
    y_label = axis_labels[pc_y_name]
  )
  x_limits <- ggplot2::layer_scales(main_plot)$x$get_limits()
  y_limits <- ggplot2::layer_scales(main_plot)$y$get_limits()
  x_density_plot <- get_x_density_plot(
    plot_data,
    density_lwd = density_lwd,
    pal = pal,
    x_limits = x_limits
  )
  y_density_plot <- get_y_density_plot(
    plot_data,
    density_lwd = density_lwd,
    pal = pal,
    y_limits = y_limits
  )

  legend <- ggpubr::get_legend(main_plot)
  main_plot_without_legend <- main_plot + ggplot2::theme(legend.position = 'none')
  p <- x_density_plot + legend + main_plot_without_legend + y_density_plot +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 2,
      widths = c(3, 1),
      heights = c(1, 3)
    ) +
    patchwork::plot_annotation(title = plot_title, subtitle = plot_subtitle)
  return(p)
  # table_grob <- gridExtra::arrangeGrob(
  #   x_density_plot,
  #   legend,
  #   main_plot + ggplot2::theme(legend.position = 'none'),
  #   y_density_plot,
  #   ncol = 2,
  #   nrow = 2,
  #   widths = c(3, 1),
  #   heights = c(1, 3)
  # )
  # return(table_grob)
}


#' Prepare PCA plot data
#'
#' @param projection A matrix pf PCA projections stored in \code{projected}
#'   attribute of \code{pca_result} object.
#' @param pheno the \code{data.frame} of phenotype.
#' @param batch_name the column name of batch in the phenotype \code{data.frame}.
#' @param classification_name the column name of classification in the phenotype
#'   \code{data.frame}.
#' @param pc_x_name column name for PC on X axis. Default to "PC1".
#' @param pc_xyname column name for PC on Y axis. Default to "PC2".
#' @return A \code{data.frame} has four columns at least: \code{pc_x}, \code{pc_y},
#'   \code{batch}, \code{classification}.
prepare_plot_data <- function(projection,
                              pheno,
                              batch_name,
                              classification_name,
                              pc_x_name = "PC1",
                              pc_y_name = "PC2") {
  data.frame(
    pc_x = projection[, pc_x_name],
    pc_y = projection[, pc_y_name],
    batch = pheno[, batch_name],
    classification = pheno[, classification_name]
  )
}


#' Main PCA plot
#'
#' @param data A \code{data.frame} for PCA plot. It can be created
#' @param pal point color palette.
#' @param batch_label a character describe the ggplot2 label for batch. Default
#'   to "Batch".
#' @param classification_label a character describe the ggplot2 label for
#'   classification. Default to "Classification".
#' @param x_label a character of X axis label. Default to "PC1".
#' @param y_label a character of Y axis label. Default to "PC2".
#' @param legend_cex a float for legend text size. Default to 0.7.
#' @return A \code{\line[ggplot2]{ggplot}} object.
get_main_pca_plot <- function(data,
                              pal,
                              batch_label = "Batch",
                              classification_label = "Classification",
                              x_label = "PC1",
                              y_label = "PC2",
                              legend_cex = 0.7) {
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = pc_x,
      y = pc_y,
      color = classification,
      shape = batch
    )
  ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      shape = batch_label,
      color = classification_label
    ) +
    ggplot2::scale_color_manual(values = pal, drop = FALSE) +
    # ggplot2::scale_x_continuous(limits = ggplot2::xlim) +
    # ggplot2::scale_y_continuous(limits = ggplot2::ylim) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = 'right',
      legend.box = 'horizontal',
      legend.direction = 'vertical',
      legend.key.height = ggplot2::unit(0.2, 'cm'),
      legend.key.width = ggplot2::unit(0.1, 'cm'),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.75)),
      legend.spacing.x = ggplot2::unit(0.1, 'cm'),
      legend.spacing.y = ggplot2::unit(0.1, 'cm'),
      legend.text = ggplot2::element_text(size = ggplot2::rel(legend_cex))
    )
}


#' PC x density plot
#'
#' @param data A \code{data.frame} for PCA plot. It can be created
#' @param pal point color palette.
#' @param density_lwd density line width. Default to 0.2.
#' @param x_limits X axis limits.
#' @param title_cex a float for title text size. Default to 0.7.
#' @return A \code{\line[ggplot2]{ggplot}} object.
get_x_density_plot <- function(data,
                               density_lwd = 0.2,
                               pal,
                               x_limits,
                               title_cex = 1.5) {
  ggplot2::ggplot(data = data,
                  ggplot2::aes(pc_x, fill = classification, linetype = batch)) +
    ggplot2::geom_density(size = density_lwd, alpha = 0.5) +
    ggplot2::ylab('Density') +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(0.8)),
      plot.title = ggplot2::element_text(hjust = 0.5, size = ggplot2::rel(title_cex)),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = 'none'
    ) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_x_continuous(limits = x_limits)
}


#' PC y density plot
#'
#' @param data A \code{data.frame} for PCA plot. It can be created
#' @param pal point color palette.
#' @param density_lwd density line width. Default to 0.2.
#' @param x_limits X axis limits.
#' @param title_cex a float for title text size. Default to 0.7.
#' @return A \code{\line[ggplot2]{ggplot}} object.
get_y_density_plot <- function(data,
                               density_lwd = 0.2,
                               pal,
                               y_limits,
                               title_cex = 1.5) {
  ggplot2::ggplot(data = data,
                  ggplot2::aes(
                    x = pc_y,
                    fill = classification,
                    linetype = batch
                  )) +
    ggplot2::geom_density(size = density_lwd, alpha = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::ylab('Density') +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = ggplot2::rel(0.8)),
      axis.title.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = 'none'
    ) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_x_continuous(limits = y_limits)
}
