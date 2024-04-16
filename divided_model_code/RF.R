#### RUN GET_MAIN_DATA.R FIRST ####



# Random Forest Training and Evaluation



# a / (a + b)



initial_rf <- randomForest(TARGET ~., data = transformed_train)

plot(initial_rf)




## Resampling strategy

resample <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)


## Grid of hyperparameter values

hyper_grid <- expand.grid(mtry = c(13, 15, 17, 20, 22, 25), 
                          
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          
                          min.node.size = c(5, 7, 9)) # 


## Tuning Hyperparameters

rf_fit <- train(TARGET ~ .,
                
                data = transformed_train, 
                
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

# Best ROC for this set = with mtry = 17, min.node.size = 9, splitrule = "hellinger"

# Part 2 - Final Model

fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data


RF_final <- train(TARGET ~., 
                  
                  data = transformed_train,
                  
                  method = "ranger",
                  
                  trControl = fitControl_final,
                  
                  metric = "ROC",
                  
                  tuneGrid = data.frame(mtry = 17,
                                        
                                        min.node.size = 9,
                                        
                                        splitrule = "hellinger"),
                  
                  num.trees = 250)

rf_pred_train <- predict(RF_final, newdata = transformed_train)

rf_pred_test <- predict(RF_final, newdata = transformed_test)

confusionMatrix(data=rf_pred_train, reference=transformed_train$TARGET)

confusionMatrix(data=rf_pred_test, reference=transformed_test$TARGET)

#### Again, but this time with a balanced dataset (through downsampling) to see if it makes a difference ####

tt=transformed_train
all_neg = tt[tt$TARGET == 'n',]
all_pos = tt[tt$TARGET == 'p',]
subs_neg = all_neg[sample(tot_neg, tot_pos),]
bal_train = rbind(subs_neg, all_pos)

# run training/testing again, but with bal_train

initial_rf_bal <- randomForest(TARGET ~., data = bal_train)

plot(initial_rf_bal)

# about 200 is the best

## Tuning Hyperparameters

rf_fit_bal <- train(TARGET ~ .,
                    
                    data = bal_train, 
                    
                    method = "ranger", 
                    
                    trControl = resample, 
                    
                    tuneGrid = hyper_grid,
                    
                    metric = "ROC",
                    
                    num.trees = 200)


ggplot(rf_fit_bal)

plot(rf_fit_bal, metric = "ROC", plotType = "level")
plot(rf_fit_bal, metric = "Sens", plotType = "level")
plot(rf_fit_bal, metric = "Spec", plotType = "level")
plot(rf_fit_bal, metric = "ROCSD", plotType = "level")



fitControl_final_bal <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data


RF_final_bal <- train(TARGET ~., 
                      
                      data = bal_train,
                      
                      method = "ranger",
                      
                      trControl = fitControl_final_bal,
                      
                      metric = "ROC",
                      
                      tuneGrid = data.frame(mtry = 13,
                                            
                                            min.node.size = 9,
                                            
                                            splitrule = "hellinger"),
                      
                      num.trees = 200)

rf_pred_train_bal_actual <- predict(RF_final_bal, newdata = bal_train)

rf_pred_train_bal <- predict(RF_final_bal, newdata = transformed_train)

rf_pred_test_bal <- predict(RF_final_bal, newdata = transformed_test)

confusionMatrix(data=rf_pred_train_bal_actual, reference=bal_train$TARGET)

confusionMatrix(data=rf_pred_train_bal, reference=transformed_train$TARGET)

confusionMatrix(data=rf_pred_test_bal, reference=transformed_test$TARGET)
