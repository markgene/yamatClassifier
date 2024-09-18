context("Test plot_pca_result()")
library(ggplot2)
library(yamatClassifier)

get_test_trainer <- function() {
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
  trainer
}


test_that("plot_pca_result()", {
  trainer <- get_test_trainer()
  meth <- yamatClassifier::get_meth(trainer = trainer)
  meth_log2 <- log2(meth + 1)
  meth_log2 <- meth_log2[1:250, ]
  element_count <- length(meth_log2)
  set.seed(123)
  meth_log2 <- cbind(meth_log2,
                     meth_log2 + rnorm(element_count) + 5,
                     meth_log2 + rnorm(element_count) + 10)
  basenames <- c(
    "5723646052_R02C02",
    "5723646052_R04C01",
    "5723646052_R05C02",
    "5723646052_R01C02",
    "5723646052_R02C01",
    "5723646052_R03C02",
    "5723646052_R04C02",
    "5723646052_R05C01",
    "5723646052_R06C02"
  )
  colnames(meth_log2) <- basenames
  pca_result <- yamatClassifier::pca(t(meth_log2),
                                     k = 4,
                                     seed = 123,
                                     threshold = 0.9)
  targets <- data.frame(
    Basename = basenames,
    Sentrix_ID = "5723646052",
    Batch = c(rep("B1", 4), rep("B2", 5)),
    Classification = c("C1", "C1", "C2", "C2", "C1", "C1", "C2", "C2", "C2")
  )
  p <- yamatClassifier::plot_pca_result(pca_result,
                                   targets,
                                   batch_name = "Batch",
                                   classification_name = "Classification")
  pca_plot_file <- file.path(trainer$output, "pca_meth.pdf")
  ggplot2::ggsave(filename = pca_plot_file, plot = p, height = 8, width = 8)
  expect_true(file.exists("output/5723646052/pca_meth.pdf"))
})
