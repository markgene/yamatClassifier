library(testthat)
library(yamatClassifier)
library(logger)

logger::log_threshold(DEBUG)

test_check("yamatClassifier")
