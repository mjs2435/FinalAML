#### RUN GET_MAIN_DATA.R AND DEP.R FIRST ####



# Random Forest Training and Evaluation



# a / (a + b)



# initial_rf <- randomForest(TARGET ~., data = transformed_train)

# plot(initial_rf)




## Resampling strategy
train_rf <- function(train, grid){
  resample <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
  
  

  
  
  ## Tuning Hyperparameters
  
  rf_fit <- train(TARGET ~ .,
                  
                  data = train, 
                  
                  method = "ranger", 
                  
                  trControl = resample, 
                  
                  tuneGrid = hyper_grid,
                  
                  metric = "ROC",
                  
                  num.trees = 50)
  
  
  ggplot(rf_fit)
  
  plot(rf_fit, metric = "ROC", plotType = "level")
  plot(rf_fit, metric = "Sens", plotType = "level")
  plot(rf_fit, metric = "Spec", plotType = "level")
  plot(rf_fit, metric = "ROCSD", plotType = "level")
  
  return(rf_fit)
}

final_RF_mod <- function(train, test, best_grid){
  # Best ROC for this set = with mtry = 17, min.node.size = 9, splitrule = "hellinger"
  
  # Part 2 - Final Model
  
  fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data
  
  
  RF_final <- train(TARGET ~., 
                    
                    data = train,
                    
                    method = "ranger",
                    
                    trControl = fitControl_final,
                    
                    metric = "ROC",
                    
                    tuneGrid = best_grid,
                    
                    num.trees = 250)

  return(RF_final)
  
}

## Grid of hyperparameter values

hyper_grid <- expand.grid(mtry = c(13, 15, 17, 20, 22, 25), 
                          
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          
                          min.node.size = c(5, 7, 9)) # 



b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]
mod = train_rf(tr, hyper_grid)

ggplot(mod)

plot(mod, metric = "ROC", plotType = "level")
plot(mod, metric = "Sens", plotType = "level")
plot(mod, metric = "Spec", plotType = "level")
plot(mod, metric = "ROCSD", plotType = "level")

best_grid = data.frame(mtry = 17,
                       
                       min.node.size = 9,
                       
                       splitrule = "hellinger")

mod_fin = final_RF_mod(tr, ts, best_grid)

rf_pred_train <- predict(mod_fin, newdata = tr)

rf_pred_test <- predict(mod_fin, newdata = ts)

confusionMatrix(data=rf_pred_train, reference=tr$TARGET)

confusionMatrix(data=rf_pred_test, reference=ts$TARGET)
do_vip(mod_fin, tr)

vip(mod_fin, method = "permute", train = tr, target = "TARGET",
    
    metric = "logloss", reference_class = "p", pred_wrapper = prob_yes, event_level = "first")