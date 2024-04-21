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
  

  
  return(XG_final)

}

grid <- expand.grid(nrounds = c(300, 400, 500, 600),   
                          max_depth = c(1, 2, 4, 8, 16), 
                          eta = c(0.05, 0.01, 0.005, 0.001),    
                          min_child_weight = c(14, 15, 16, 17), 
                          subsample = 0.6, 
                          gamma = 0,
                          colsample_bytree = 1)



b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]


XGB_all = train_XGB(tr, grid)

ggplot(XGB_all)

plot(XGB_all, metric = "ROC", plotType = "level")
plot(XGB_all, metric = "Sens", plotType = "level")
plot(XGB_all, metric = "Spec", plotType = "level")
plot(XGB_all, metric = "ROCSD", plotType = "level")

best_grid = data.frame(nrounds = 500,
                       max_depth = 16,
                       eta = 0.010,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 16,
                       subsample = .6)

XGB_mod = final_XGB_mod(tr, ts, best_grid)

xg_pred_train <- predict(XGB_mod, newdata = tr)

xg_pred_test <- predict(XGB_mod, newdata = ts)

confusionMatrix(data=xg_pred_train, reference=tr$TARGET)

confusionMatrix(data=xg_pred_test, reference=ts$TARGET)
