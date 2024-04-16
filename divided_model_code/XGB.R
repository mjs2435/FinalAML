#### RUN GET_MAIN_DATA.R FIRST ####

# XGBoost Training and Evaluation

resample <- trainControl(method = "cv",
                         
                         number = 5,
                         
                         classProbs = TRUE,
                         
                         summaryFunction = twoClassSummary) 



hyper_grid <- expand.grid(nrounds = c(300, 400),   
                          
                          max_depth = c(1, 2, 4), 
                          
                          eta = c(0.05, 0.01),    
                          
                          min_child_weight = c(15), 
                          
                          subsample = 0.6, 
                          
                          gamma = 0,
                          
                          colsample_bytree = 1)



xg_fit <- train(TARGET ~ .,
                
                data = transformed_train, 
                
                method = "xgbTree",
                
                trControl = resample,
                
                tuneGrid = hyper_grid,
                
                metric = "ROC")
ggplot(xg_fit)

plot(xg_fit, metric = "ROC", plotType = "level")
plot(xg_fit, metric = "Sens", plotType = "level")
plot(xg_fit, metric = "Spec", plotType = "level")
plot(xg_fit, metric = "ROCSD", plotType = "level")

# Final Model

fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data


XG_final <- train(TARGET ~., 
                  
                  data = transformed_train,
                  
                  method = "xgbTree",
                  
                  trControl = fitControl_final,
                  
                  metric = "ROC",
                  
                  tuneGrid = data.frame(nrounds = 400,
                                        
                                        max_depth = 4,
                                        
                                        eta = 0.05,
                                        
                                        gamma = 0,
                                        
                                        colsample_bytree = 1,
                                        
                                        min_child_weight = 15,
                                        
                                        subsample = .6)
)

xg_pred_train <- predict(XG_final, newdata = transformed_train)

xg_pred_test <- predict(XG_final, newdata = transformed_test)

confusionMatrix(data=xg_pred_train, reference=transformed_train$TARGET)

confusionMatrix(data=xg_pred_test, reference=transformed_test$TARGET)