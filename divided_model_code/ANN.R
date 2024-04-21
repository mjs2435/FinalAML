# Artificial Neural Network Model Training and Evaluation
train_ANN <- function(train, grid){
  ## Resampling strategy
  
  resample <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
  
  
  ## Grid of hyperparameter values
  
  # rang = c(0.1, 0.9),  
  # algorithm = "rprop+"
  
  ## Tuning Hyperparameters
  
  ann_fit <- train(TARGET ~ .,
                   
                   data = train, 
                   
                   method = "nnet", 
                   
                   trControl = resample, 
                   
                   tuneGrid = grid,
                   
                   metric = "ROC")
  
  
  ggplot(ann_fit)
  
  plot(ann_fit, metric = "ROC", plotType = "level")
  plot(ann_fit, metric = "Sens", plotType = "level")
  plot(ann_fit, metric = "Spec", plotType = "level")
  plot(ann_fit, metric = "ROCSD", plotType = "level")
  return(ann_fit)
}
final_ANN_mod <- function(train, test, grid_best){
  

  fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data
  
  
  ANN_final <- train(TARGET ~., 
                     
                     data = train,
                     
                     method = "nnet",
                     
                     trControl = fitControl_final,
                     
                     metric = "ROC",
                     
                     tuneGrid = grid_best
  )
  
  ANN_pred_train <- predict(ANN_final, newdata = train)
  
  ANN_pred_test <- predict(ANN_final, newdata = test)
  
  confusionMatrix(data=ANN_pred_train, reference=train$TARGET)
  
  confusionMatrix(data=ANN_pred_test, reference=test$TARGET)
  return(ANN_final)
}


grid <- expand.grid(size = c(2, 5, 10),
                    decay = c(0, 0.1, 0.05, 0.01))  

b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]



ANN_tr = train_ANN(tr, grid)


ggplot(ANN_tr)

plot(ANN_tr, metric = "ROC", plotType = "level")
plot(ANN_tr, metric = "Sens", plotType = "level")
plot(ANN_tr, metric = "Spec", plotType = "level")
plot(ANN_tr, metric = "ROCSD", plotType = "level")
grid_best = data.frame(size = 1, decay = .01)

ANN_fn = final_ANN_mod(tr, ts, grid_best)

ANN_pred_train <- predict(ANN_fn, newdata = tr)

ANN_pred_test <- predict(ANN_fn, newdata = ts)

confusionMatrix(data=ANN_pred_train, reference=tr$TARGET)

confusionMatrix(data=ANN_pred_test, reference=ts$TARGET)

do_vip(ANN_fn, tr)

vip(ANN_fn, method = "permute", train = tr, target = "TARGET",
    
    metric = "logloss", reference_class = "p", pred_wrapper = prob_yes, event_level = "first")
