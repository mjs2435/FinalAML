## Stacked Models (Weighted Ensemble Model)

# Base Learners

final_stacked_mod <- function(train, test){
  set.seed(1)
  fitControl_final <- trainControl(method = "none", classProbs = TRUE)
  # Random Forest (RF)
  RF <- train(TARGET ~ .,
              data = train,
              method = "ranger",
              trControl = fitControl_final,
              metric = "ROC",
              verbose = FALSE,
              tuneGrid = data.frame(mtry = 13,
                                    min.node.size = 7,
                                    splitrule = "extratrees"),
              num.trees = 300,
              importance = "impurity")
  RF_pred_train <- predict(RF, newdata = train)
  RF_train_results <- confusionMatrix(train$TARGET, RF_pred_train)
  RF_Kappa <- RF_train_results$overall["Kappa"]
  # ANN
  ANN <- train(TARGET ~ .,
               data = train, 
               method = "svmLinear",
               trControl = fitControl_final,
               verbose = FALSE,
               tuneGrid = data.frame(C = 0.1),
               metric = "ROC",
               importance = TRUE)
  ANN_pred_train <- predict.train(ANN, newdata = train %>% select(-TARGET))
  ANN_train_results <- confusionMatrix(train$TARGET, ANN_pred_train)
  ANN_Kappa <- ANN_train_results$overall["Kappa"]
  # Extreme Gradient Boosting Machine (XGBoost)
  XGB <- train(TARGET ~ .,
               data = train, 
               method = "xgbTree",
               verbose = FALSE,
               trControl = fitControl_final, 
               tuneGrid = data.frame(nrounds = 150,   
                                     max_depth = 4, 
                                     eta = 0.2,    
                                     min_child_weight = 6, 
                                     subsample = 0.7, 
                                     gamma = 0,
                                     colsample_bytree = 1),
               metric = "ROC",
               importance = TRUE)
  XGB_pred_train <- predict(XGB, newdata = train)
  XGB_train_results <- confusionMatrix(train$TARGET, XGB_pred_train)
  XGB_Kappa <- XGB_train_results$overall["Kappa"]
  Weights <- c((RF_Kappa)^2/sum((RF_Kappa)^2 + (ANN_Kappa)^2 + (XGB_Kappa)^2),
               (ANN_Kappa)^2/sum((RF_Kappa)^2 + (ANN_Kappa)^2 + (XGB_Kappa)^2),
               (XGB_Kappa)^2/sum((RF_Kappa)^2 + (ANN_Kappa)^2 + (XGB_Kappa)^2))
  # Making predictions
  RF_prediction <- predict(RF, newdata = test, type = "prob")[, "p"]
  ANN_prediction <- predict.train(ANN, newdata = test %>% select(-TARGET), type = "prob")[, "p"]
  XGB_prediction <- predict(XGB, newdata = test, type = "prob")[, "p"]
  SP_prediction <- super_learner(RF_prediction, ANN_prediction, XGB_prediction, Weights)
  SP_results <- confusionMatrix(test$TARGET, SP_prediction)
  return(SP_results)
  
}

b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]
mod = final_stacked_mod(tr, ts)









