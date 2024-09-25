# # Install and load necessary libraries
# install.packages("Boruta")        # Boruta for feature selection
# install.packages("randomForest")  # Random Forest for classification
# install.packages("caret")         # Caret for cross-validation and hyperparameter tuning
#
# library(Boruta)
# library(randomForest)
# library(caret)
# library(mlbench)
# data(Sonar)
#
#
# # Perform Boruta feature selection
# set.seed(123)  # For reproducibility
# boruta_result <- Boruta(Class ~ ., data = Sonar, doTrace = 2)
#
# # Print the feature selection result
# print(boruta_result)
#
# # Get the selected features (only Confirmed attributes)
# selected_features <- getSelectedAttributes(boruta_result, withTentative = FALSE)
# print(selected_features)
#
# # Subset the dataset with selected features
# sonar_boruta <- Sonar[, c(selected_features, "Class")]  # 'Class' is the target variable
#
# # Create outer CV
# outer_fold <- 5
# set.seed(67)
# outer_test_indexes <- caret::createDataPartition(sonar_boruta$Class, times = outer_fold, p = (1/outer_fold))
#
# for (test_indexes in outer_test_indexes) {
#   outer_test <- sonar_boruta[test_indexes, ]
#   outer_train <- sonar_boruta[-test_indexes, ]
#   # Set up control for 5x5 cross-validation
#   control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, search = "grid")
#
#   # Define the grid of hyperparameters to tune
#   # Here, we are tuning 'mtry' (number of variables randomly sampled at each split)
#   tune_grid <- expand.grid(mtry = seq(2, length(selected_features), by = 10))
#
#   # Train Random Forest model with 5x5 cross-validation and parameter tuning
#   set.seed(123)  # For reproducibility
#   rf_model <- train(
#     Class ~ .,
#     data = outer_train,
#     method = "rf",
#     tuneGrid = tune_grid,
#     trControl = control,
#     importance = TRUE
#   )
# }
#
# control <- trainControl(method = "cv",
#                         number = 5,
#                         classProbs = TRUE,  # Enable probability estimation
#                         summaryFunction = multiClassSummary)  # For multi-class
#
# # Train the Random Forest model
# set.seed(123)
# rf_model <- train(Class ~ .,
#                   data = outer_train,
#                   method = "rf",
#                   trControl = control,
#                   tuneLength = 5)
#
# # Get predicted probabilities on the training data
# predicted_probs <- predict(rf_model, newdata = outer_train, type = "prob") %>%
#   as.matrix()
# ridge_multiclass_model <- glmnet::glmnet(predicted_probs, outer_train$Class, family = "multinomial", alpha = 0)
# cv_ridge_multiclass <- cv.glmnet(predicted_probs, outer_train$Class, family = "multinomial", alpha = 0)
# calibrated_probs <- predict(cv_ridge_multiclass, newx = predicted_probs, s = "lambda.min", type = "response")
#
#
# # Print the final model and the best hyperparameters
# print(rf_model)
#
# # View the best value of mtry
# print(rf_model$bestTune)
#
# # Plot the model performance for different mtry values
# plot(rf_model)
#
# # Get the feature importance from the final Random Forest model
# importance <- varImp(rf_model, scale = FALSE)
# print(importance)
#
# # Plot variable importance
# plot(importance)
