# library(glmnet)
#
# library(mlbench)
# data(Sonar)
# X <- as.matrix(iris[, 1:4])
# y <- factor(iris$Species)
#
# # Assuming you have a binary response variable y and predictor matrix X
# # X is your predictor matrix and y is your binary outcome (0/1)
#
# # Fit ridge logistic regression model using glmnet
# ridge_model <- glmnet(X, y, alpha = 0, lambda.min.ratio = 1e-6, family = "multinomial")
#
# # Get a sequence of lambda values that glmnet used to fit the model
# lambdas <- ridge_model$lambda
#
# # Predict probabilities for each lambda
# probs <- predict(ridge_model,
#                  newx = X,
#                  s = lambdas,
#                  type = "response")
#
# # Function to calculate Youden Index for each class in one-vs-rest (OvR) manner
# youden_index_multinomial <- function(probs, true_labels, class, threshold = 0.9) {
#   # Convert probabilities to predicted class (1 for positive class, 0 for rest)
#   predicted_class <- ifelse(probs[, class] > threshold, 1, 0)
#
#   # Convert true labels to binary (1 for class of interest, 0 for others)
#   true_binary <- ifelse(true_labels == class, 1, 0)
#
#   # Calculate confusion matrix components
#   TP <- sum(predicted_class == 1 & true_binary == 1)
#   TN <- sum(predicted_class == 0 & true_binary == 0)
#   FP <- sum(predicted_class == 1 & true_binary == 0)
#   FN <- sum(predicted_class == 0 & true_binary == 1)
#
#   # Calculate sensitivity and specificity
#   sensitivity <- TP / (TP + FN)
#   specificity <- TN / (TN + FP)
#
#   # Calculate Youden Index for the class
#   J <- sensitivity + specificity - 1
#   return(J)
# }
#
# # Calculate the Youden Index for each lambda and for each class
# youden_values <- array(0, dim = c(length(lambdas), length(unique(y))))
# colnames(youden_values) <- levels(y)
# for (k in 1:length(lambdas)) {
#   for (class in levels(y)) {
#     j <- which(class == levels(y))
#     youden_values[k, j] <- youden_index_multinomial(probs[, , k], y, class)
#   }
# }
# youden_values
#
# # Average Youden Index across all classes for each lambda
# avg_youden_values <- rowMeans(youden_values)
#
# # Find the lambda that maximizes the average Youden Index
# optimal_lambda <- lambdas[which.max(avg_youden_values)]
#
# # Alternatively, for a specific class (e.g., class 1)
# class_1_optimal_lambda <- lambdas[which.max(youden_values[, 1])]
#
# # Fit the final model using the optimal lambda
# final_model <- glmnet(X,
#                       y,
#                       alpha = 0,
#                       family = "multinomial",
#                       lambda = optimal_lambda)
#
# # You can now use this model for predictions, model interpretation, etc.
# round(predict(final_model, newx = X, type = "response"), digits = 3)
# predict(final_model, newx = X, type = "class")
#
