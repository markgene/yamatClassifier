
# set.seed(123)
# x <- matrix(rnorm(10 * 18), 10, 18)
# x[, 1:3] <- x[, 1:3] + 5
# batch <- rep(c("A", "B", "C"), each = 6)
# batch2 <- rep(c("D", "E"), 9)
# batch <- as.factor(batch)
# batch2 <- as.factor(batch2)
# batch2 <- as.factor(batch2)
# covariates <- NULL
# X.batch <- yamatClassifier:::get_batch_matrix(batch = batch,
#                                               batch2 = batch2,
#                                               covariates =  covariates)
# X.batch
# design = matrix(1, ncol(x), 1)
# fit <- limma::lmFit(x, cbind(design, X.batch))
# bs <- fit$coefficients[, -(1:ncol(design)), drop = FALSE]
# bs[is.na(beta)] <- 0
# fit$coefficients_batch_effect <- bs
