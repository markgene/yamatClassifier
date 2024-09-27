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
#
# tune_result_rda <- file.path(output, "tune_result_v2.Rda")
# load(tune_result)
