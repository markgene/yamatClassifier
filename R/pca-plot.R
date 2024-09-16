#' Plot PCA.
#'
#' @param pca_result An object of pca_result class.
#' @param pheno the \code{data.frame} of phenotype.
#' @param batch_name the column name of batch in the phenotype \code{data.frame}.
#' @param classification_name the column name of classification in the phenotype
#'   \code{data.frame}.

#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance to choose PC number. Default to 0.9.
#' @return A list of four elements:
#'   \itemize{
#'     \item \code{projected} The result matrix of PCA analysis.
#'     \item \code{pca123} A list of result from step 1-3 returned by
#'       \code{\link{pca123}}.
#'     \item \code{capper} A list of the result of choosing PC number
#'       by Capper's method, returned by \code{\link{find_pc_number.capper}}.
#'     \item \code{vf} A list of the result of choosing PC number
#'       by fraction of variance, returned by \code{\link{find_pc_number.var_frac}}.
#'   }
#' @details The function wraps up the following five steps of PCA:
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
#'   I use \code{\link[RSpectra]{eigs}} function in Rspectra package
#'   instead of \code{\link[base]{eigen}} function in base to deal
#'   with large matrix.
#' @export
plot_pca_result <- function(pca_result,
                            pheno,
                            batch_name,
                            classification_name,
                            pc_x = 1,
                            pc_y = 2,
                            batch_label = "Batch",
                            classification_label = "Classification",
                            density_lwd = 0.2) {
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

  gridExtra::grid.arrange(
    x_density_plot,
    legend,
    main_plot + ggplot2::theme(legend.position = 'none'),
    y_density_plot,
    ncol = 2,
    nrow = 2,
    widths = c(3, 1),
    heights = c(1, 3)
  )
}


#' Prepare PCA plot data
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
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = ylim) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = 'right',
      legend.box = 'horizontal',
      legend.direction = 'vertical',
      legend.key.height = unit(0.2, 'cm'),
      legend.key.width = unit(0.1, 'cm'),
      legend.title = ggplot2::element_text(size = rel(0.75)),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.text = ggplot2::element_text(size = rel(legend_cex))
    )
}


#' PC x density plot
get_x_density_plot <- function(data,
                               density_lwd = 0.2,
                               pal,
                               x_limits,
                               title_cex = 1.5) {
  ggplot2::ggplot(data = data,
                  ggplot2::aes(
                    x = pc_x,
                    fill = classification,
                    linetype = batch
                  )) +
    ggplot2::geom_density(size = density_lwd, alpha = 0.5) +
    ggplot2::ylab('Density') +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = rel(0.8)),
      plot.title = ggplot2::element_text(hjust = 0.5, size = rel(title_cex)),
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
      axis.title.x = ggplot2::element_text(size = rel(0.8)),
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


plot_pca <- function(object,

                     batch = NULL,
                     trt = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     color.set = NULL,
                     batch.legend.title = 'Batch',
                     trt.legend.title = 'Treatment',
                     density.lwd = 0.2,
                     title = NULL,
                     title.cex = 1.5,
                     legend.cex = 0.7,
                     legend.title.cex = 0.75) {
  data <- as.data.frame(object[['variates']][['X']])
  expl.var <- object[['prop_expl_var']][['X']]

  if (is.null(batch)) {
    batch <- batch
  } else{
    batch <- as.factor(batch)
  }
  if (is.null(trt)) {
    trt <- trt
  } else{
    trt <- as.factor(trt)
  }

  # color set
  if (is.null(color.set)) {
    color.set <- viridis::viridis_pal(option = "H")(length(unique(pheno_df[, trt])))

  } else{
    color.set <- color.set
  }

  # main plot
  pMain <- ggplot(data = data, aes(
    x = data[, 1],
    y = data[, 2],
    colour = batch,
    shape = trt
  )) +
    geom_point() + xlab(paste0('PC1: ', round(as.numeric(expl.var[1]) *
                                                100), '% expl.var')) +
    ylab(paste0('PC2: ', round(as.numeric(expl.var[2]) * 100), '% expl.var')) +
    scale_shape_manual(values = c(1, 19, 2, 17, 4)) +
    scale_color_manual(values = color.set) + theme_bw() +
    labs(colour = batch.legend.title, shape = trt.legend.title) +
    scale_x_continuous(limits = xlim) + scale_y_continuous(limits = ylim) +
    theme(
      legend.position = 'right',
      legend.box = 'horizontal',
      legend.direction = 'vertical',
      legend.key.height = unit(0.2, 'cm'),
      legend.key.width = unit(0.1, 'cm'),
      legend.title = element_text(size = rel(legend.title.cex)),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.text = element_text(size = rel(legend.cex))
    )

  xlim.update <- layer_scales(pMain)$x$get_limits()
  ylim.update <- layer_scales(pMain)$y$get_limits()

  # top density plot
  pTop <- ggplot(data = data, aes(
    x = data[, 1],
    fill = batch,
    linetype = trt
  )) +
    geom_density(size = density.lwd, alpha = 0.5) + ylab('Density') +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = rel(0.8)),
      plot.title = element_text(hjust = 0.5, size = rel(title.cex)),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_manual(values = color.set) +
    scale_x_continuous(limits = xlim.update) + labs(title = title)

  # right density plot
  pRight <- ggplot(data = data, aes(
    x = data[, 2],
    fill = batch,
    linetype = trt
  )) +
    geom_density(size = density.lwd, alpha = 0.5) +  coord_flip() +
    ylab('Density') +
    theme(
      axis.title.x = element_text(size = rel(0.8)),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      legend.position = 'none'
    ) +
    scale_fill_manual(values = color.set) +
    scale_x_continuous(limits = ylim.update)

  if (is.null(batch) && is.null(trt)) {
    legend <-
      grid.rect(gp = gpar(col = "white"))
  } else{
    legend <- get_legend(pMain)
  }

  grid.arrange(
    pTop,
    legend,
    pMain + theme(legend.position = 'none'),
    pRight,
    ncol = 2,
    nrow = 2,
    widths = c(3, 1),
    heights = c(1, 3)
  )

}
