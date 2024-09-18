# idat_dir <- file.path(system.file("extdata", package = "minfiData"), "5723646052")
# targets <- data.frame(
#   Basename = c(
#     "5723646052_R02C02",
#     "5723646052_R04C01",
#     "5723646052_R05C02"
#   ),
#   Sentrix_ID = "5723646052",
#   Batch = c("B1", "B1", "B1"),
#   Diagnosis = "D1"
# )
# trainer <- yamatClassifier::create_trainer(idat_dir = idat_dir,
#                                            targets = targets,
#                                            output = "output/5723646052")
# load("tests/testthat/output/5723646052/batch_effect_explore/Batch_meth_pca.Rda")
#
#
# library(yamatClassifier)
# p <- yamatClassifier::plot_pca_result(meth_pca,
#                      targets,
#                      batch_name = "Batch",
#                      classification_name = "Diagnosis")
