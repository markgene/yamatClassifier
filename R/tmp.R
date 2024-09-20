# # Batch effect correction for trainer set on meth
#
# library(logger)
# library(yamatClassifier)
#
# logger::log_threshold(DEBUG)
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
# meth <- yamatClassifier::get_meth(trainer = trainer)
# targets <- yamatClassifier:::get_targets(trainer = trainer)
#
# yamatClassifier::train_batch_effect_model(
#   x = dat$x,
#   batch = dat$batch,
#   batch2 = dat$batch2,
#   adjusted_rda = "output/batch_effect_adjusted.Rda",
#   fit_rda = "output/batch_effect_fit.Rda"
# )
