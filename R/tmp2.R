# # Feature selection with Boruta algorithm
#
# library(dplyr)
# library(ggplot2)
# library(logger)
# library(yamatClassifier)
#
# logger::log_threshold(DEBUG)
#
# # Real data
#
# data_dir <- "/home/chenm8/beegfs/projects/MC123_SarcomaClassifier/data/GSE140686"
# output <- "/home/chenm8/beegfs/projects/MC123_SarcomaClassifier/output/GSE140686"
# random_state <- 39
# batch_size <- 600 # sqrt(360456 probes)
#
# trainer_rda <- file.path(output, "trainer.Rda")
# if (file.exists(trainer_rda)) {
#   logger::log_info("Reading existing trainer Rda file")
#   load(trainer_rda)
# } else {
#   logger::log_error("fail to find the trainer Rda file")
# }
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
#   dplyr::left_join(meth_classification, by = "MethylationClassName") %>%
#   dplyr::mutate(MethylationClass = glue::glue("{MethylationClassName} ({Abbreviation})")) %>%
#   dplyr::select(Basename, MethylationClass)
#
# beta_value_adjusted <- yamatClassifier::get_beta_value_adjusted(trainer = trainer)
# pheno_sorted <- pheno[order(match(pheno$Basename, colnames(beta_value_adjusted))), ]
# dat <- beta_value_adjusted %>%
#   t() %>%
#   as.data.frame() %>%
#   cbind(pheno_sorted[, "MethylationClass", drop = FALSE])
#
#
# tune_result <- yamatClassifier::train_model1(
#   dat = dat,
#   response_name = "MethylationClass",
#   feature_selection = "Boruta",
#   outer_cv_folds = 5,
#   inner_cv_folds = 5,
#   random_state = 42,
#   mtry = 13,
#   verbose = TRUE
# )
#
# tune_result_rda <- file.path(output, "tune_result.Rda")
# save(tune_result, file = tune_result_rda)


