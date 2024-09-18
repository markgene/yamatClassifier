context("Test explore_batch_effect()")
library(logger)
library(yamatClassifier)

logger::log_threshold(DEBUG)

test_that("explore_batch_effect() runs without error", {
  idat_dir <- file.path(system.file("extdata", package = "minfiData"), "5723646052")
  targets <- data.frame(
    Basename = c(
      "5723646052_R02C02",
      "5723646052_R04C01",
      "5723646052_R05C02"
    ),
    Sentrix_ID = "5723646052",
    Batch = c("B1", "B1", "B1"),
    Diagnosis = "D1"
  )
  trainer <- yamatClassifier::create_trainer(idat_dir = idat_dir,
                                             targets = targets,
                                             output = "output/5723646052")
  yamatClassifier::explore_batch_effect(
    trainer = trainer,
    batch_name = "Batch",
    classification_name = "Diagnosis",
    top_n_rle = 200,
    rle_downsample = 3,
    top_n_pca = 500,
    k = 2,
    threshold = 0.7
  )
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_meth_rle.pdf"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_meth_pca.Rda"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_meth_pca.pdf"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_unmeth_rle.pdf"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_unmeth_pca.Rda"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_unmeth_pca.pdf"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_beta_value_rle.pdf"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_beta_value_pca.Rda"
  ))
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_beta_value_pca.pdf"
  ))
})
