# Multinomial
# library(glmnet)
# n = 100
# p = 30
# nzc = trunc(p/10)
# x = matrix(rnorm(n * p), n, p)
# beta3 = matrix(rnorm(30), 10, 3)
# beta3 = rbind(beta3, matrix(0, p - 10, 3))
# f3 = x %*% beta3
# p3 = exp(f3)
# p3 = p3/apply(p3, 1, sum)
# g3 = glmnet:::rmult(p3)
# g3 <- c(rep(1, 87), rep(2, 10), rep(3, 3))
# set.seed(10101)
# cvfit = cv.glmnet(x, g3, family = "multinomial", nfolds = 10)
# plot(cvfit)
# title("Multinomial Family", line = 2.5)
