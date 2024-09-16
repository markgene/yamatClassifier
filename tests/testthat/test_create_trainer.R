context("Test create_trainer()")
library(yamatClassifier)


test_that("create_trainer()", {
  idat_dir <- system.file("extdata", package = "minfiData")
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
  expect_true("YamatClassifierTrainer" %in% class(trainer))
})
