# # Calculate adjusted beta value with adjusted meth and unmeth.
#
# library(dplyr)
# library(ggplot2)
# library(logger)
#
# logger::log_threshold(DEBUG)
#
# prepare_plot_data <- function(embedding,
#                               pheno,
#                               batch_name,
#                               classification_name,
#                               pc_x_name = "PC1",
#                               pc_y_name = "PC2") {
#   df <- data.frame(Basename = pheno[, "Basename", drop = TRUE],
#                    batch = pheno[, batch_name, drop = TRUE],
#                    classification = pheno[, classification_name, drop = TRUE])
#   output <- data.frame(pc_x = projection[, pc_x_name],
#                        pc_y = projection[, pc_y_name],
#                        Basename = rownames(projection)) %>%
#     dplyr::left_join(df, by = "Basename") %>%
#     dplyr::select(-Basename)
#   if (!all(!is.na(output$batch))) {
#     stop("pheno does not have all the items of projection")
#   }
#   return(output)
# }
#
# data_dir <- "/home/chenm8/beegfs/projects/MC123_SarcomaClassifier/data/GSE140686"
# output <- "/home/chenm8/beegfs/projects/MC123_SarcomaClassifier/output/GSE140686"
#
# trainer_rda <- file.path(output, "trainer.Rda")
# if (file.exists(trainer_rda)) {
#   logger::log_info("Reading existing trainer Rda file")
#   load(trainer_rda)
# } else {
#   logger::log_error("fail to find the trainer Rda file")
# }
#
# logger::log_info("Reading existing embedding Rda file")
# embedding_rda <- "fitsne_embedding_10000_5_3000_123.Rda"
# embedding_rda <- file.path(trainer$output, embedding_rda)
# load(embedding_rda)
#
# logger::log_info("Get phenotype data")
# load(file.path(output, "methylation_class.Rda"))
#
# meth_classification <- methylation_class %>%
#   dplyr::rename(MethylationClassName = `Methylation Class Name (in alphabetical order)`, Abbreviation = `Methylation Class Name Abbreviated`) %>%
#   dplyr::select(MethylationClassName, Abbreviation)
#
# pheno <- trainer$targets %>%
#   dplyr::rename(
#     MethylationClassName = `Methylation Class Name`,
#     TumorCellContent = `Tumour cell content [absolute]`,
#     Color = Colour,
#     Platform_ID = platform_id
#   ) %>%
#   dplyr::select(
#     MethylationClassName,
#     TumorCellContent,
#     Sample_Prep,
#     Supplier,
#     Platform_ID,
#     Diagnosis,
#     Color,
#     Basename
#   ) %>%
#   dplyr::mutate(Color = glue::glue("#{Color}")) %>%
#   dplyr::left_join(meth_classification, by = "MethylationClassName")
#
# plot_data <- embedding %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = "Basename") %>%
#   dplyr::left_join(pheno, by = "Basename")
#
# if (!all(!is.na(plot_data$Abbreviation))) {
#   stop("missing methylation classification abbreviation")
# }
#
# pal_df <- plot_data %>%
#   dplyr::select(Abbreviation, Color) %>%
#   dplyr::distinct() %>%
#   tibble::column_to_rownames(var = "Abbreviation")
# pal <- pal_df$Color
# names(pal) <- pal_df$Abbreviation
#
# ggplot2::ggplot(data = plot_data, mapping = aes(x = tSNE1, y = tSNE2)) +
#   ggplot2::geom_point(mapping = aes(color = Abbreviation), ) +
#   ggplot2::scale_color_manual(values = pal, drop = FALSE) +
#   ggplot2::labs(x = "t-SNE 1", y = "t-SNE 2", color = "Tumor Class Abbreviation") +
#   ggplot2::guides(color=ggplot2::guide_legend(ncol = 2)) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     panel.grid = ggplot2::element_blank(),
#     legend.position = "right",
#     # legend.key.height = unit(0.5, units = "in"),
#     # legend.key.width = unit(0.3, units = "in"),
#     axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
#     strip.text.y.left = ggplot2::element_text(angle = 0, size = 10),
#     strip.text.x = ggplot2::element_text(size = 10)
#   ) +
#   patchwork::plot_annotation(
#     title = glue::glue("t-SNE"),
#     subtitle = glue::glue(
#       "Each dot is a sample. Color by methylation class. Beta value of most variable 10,000 loci are used."
#     ),
#     theme = ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = 9))
#   ) -> tsne_p
# tsne_p_file <- file.path(output,
#                          glue::glue("fitsne_embedding_10000_5_3000_123_tsne_methylation_class.pdf"))
# ggplot2::ggsave(
#   filename = tsne_p_file,
#   plot = tsne_p,
#   height = 11,
#   width = 13
# )
