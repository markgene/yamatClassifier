context("Test preprocess_dkfz()")
library(yamatClassifier)


test_that("preprocess_dkfz()", {
  idat_dir <- file.path(system.file("extdata", package = "minfiData"), "5723646052")
  targets <- data.frame(
    Basename = c(
      "5723646052_R02C02",
      "5723646052_R04C01",
      "5723646052_R05C02"
    ),
    Sentrix_ID = "5723646052"
  )
  trainer <- yamatClassifier::create_trainer(idat_dir = idat_dir,
                                             targets = targets,
                                             output = "output/5723646052")
  preprocessed_dir <- yamatClassifier::preprocess_dkfz(trainer = trainer)
  expect_true(file.exists("output/5723646052/dkfz_preprocessed"))
  expect_true(file.exists("output/5723646052/dkfz_preprocessed/5723646052.Rda"))
})
