context("Test get_probes()")
library(yamatClassifier)


test_that("get_probes()", {
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
  probe_ids <- yamatClassifier::get_probes(trainer = trainer)
  expect_true(file.exists("output/5723646052/probe_ids.Rda"))
  expect_equal(length(probe_ids), 360456)
})
