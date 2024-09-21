context("Test get_beta()")
library(yamatClassifier)


test_that("get_beta()", {
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
  beta_value_adjusted <- yamatClassifier::get_beta_value(trainer = trainer)
  beta_value_adjusted_rda <- yamatClassifier::get_beta_value_adjusted_rda((trainer = trainer))
  save(beta_value_adjusted, file = beta_value_adjusted_rda)
  embeddings <- yamatClassifier::run_fitsne(
    trainer = trainer,
    top_n = 100,
    perplexity = 30,
    n_iter = 5,
    random_state = 123
  )
  expect_true(ncol(embeddings) == 2)
  expect_true(nrow(embeddings) == nrow(targets))
})
