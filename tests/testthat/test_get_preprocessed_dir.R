context("Test get_preprocessed_dir()")
library(yamatClassifier)


test_that("get_preprocessed_dir()", {
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
  preprocessed_dir <- yamatClassifier:::get_preprocessed_dir(trainer = trainer)
  expect_true(file.exists("output/5723646052/dkfz_preprocessed"))
})
