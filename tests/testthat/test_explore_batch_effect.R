context("Test explore_batch_effect()")
library(yamatClassifier)


test_that("explore_batch_effect() runs without error", {
  idat_dir <- file.path(system.file("extdata", package = "minfiData"), "5723646052")
  targets <- data.frame(
    Basename = c(
      "5723646052_R02C02",
      "5723646052_R04C01",
      "5723646052_R05C02"
    ),
    Sentrix_ID = "5723646052",
    Batch = c("B1", "B2", "B2")
  )
  trainer <- yamatClassifier::create_trainer(idat_dir = idat_dir,
                                             targets = targets,
                                             output = "output/5723646052")
  yamatClassifier::explore_batch_effect(
    trainer = trainer,
    batch_name = "Batch",
    top_n = 500,
    rle_downsample = 3
  )
  expect_true(file.exists(
    "output/5723646052/batch_effect_explore/Batch_meth_rle.pdf"
  ))
})
