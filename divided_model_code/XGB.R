#### RUN GET_MAIN_DATA.R FIRST ####

# XGBoost Training and Evaluation

train_XGB <- function(train, grid){
  resample <- trainControl(method = "cv",
                           number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
  
  xg_fit <- train(TARGET ~ .,
                  data = train, 
                  method = "xgbTree",
                  trControl = resample,
                  tuneGrid = grid,
                  metric = "ROC")
  ggplot(xg_fit)
  
  plot(xg_fit, metric = "ROC", plotType = "level")
  plot(xg_fit, metric = "Sens", plotType = "level")
  plot(xg_fit, metric = "Spec", plotType = "level")
  plot(xg_fit, metric = "ROCSD", plotType = "level")
  
  return(xg_fit)
  
}


# Final Model
final_XGB_mod <- function(train, test, best_grid){
  fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data
  
  
  XG_final <- train(TARGET ~., 
                    
                    data = train,
                    
                    method = "xgbTree",
                    
                    trControl = fitControl_final,
                    
                    metric = "ROC",
                    
                    tuneGrid = best_grid
  )
  
  xg_pred_train <- predict(XG_final, newdata = train)
  
  xg_pred_test <- predict(XG_final, newdata = test)
  
  confusionMatrix(data=xg_pred_train, reference=train$TARGET)
  
  confusionMatrix(data=xg_pred_test, reference=test$TARGET)
  
  return(XG_final)

}

grid <- expand.grid(nrounds = c(300, 400),   
                          max_depth = c(1, 2, 4), 
                          eta = c(0.05, 0.01),    
                          min_child_weight = c(15), 
                          subsample = 0.6, 
                          gamma = 0,
                          colsample_bytree = 1)

best_grid = data.frame(nrounds = 400,
           max_depth = 4,
           eta = 0.05,
           gamma = 0,
           colsample_bytree = 1,
           min_child_weight = 15,
           subsample = .6)

train_XGB(transformed_train, grid)

final_XGB_mod(transformed_train, transformed_test, best_grid)