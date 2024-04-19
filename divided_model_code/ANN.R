# Artificial Neural Network Model Training and Evaluation


## Resampling strategy

resample <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)


## Grid of hyperparameter values

grid <- expand.grid(size = c(2, 5, 10, 15),
                    decay = c(0, 0.1, 0.05, 0.01))  
# rang = c(0.1, 0.9),  
# algorithm = "rprop+"

## Tuning Hyperparameters

ann_fit <- train(TARGET ~ .,
                 
                 data = transformed_train, 
                 
                 method = "nnet", 
                 
                 trControl = resample, 
                 
                 tuneGrid = grid,
                 
                 metric = "ROC")


ggplot(ann_fit)

plot(ann_fit, metric = "ROC", plotType = "level")
plot(ann_fit, metric = "Sens", plotType = "level")
plot(ann_fit, metric = "Spec", plotType = "level")
plot(ann_fit, metric = "ROCSD", plotType = "level")
# use Hidden = 10, Weight Decay = 0.1

fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data


ANN_final <- train(TARGET ~., 
                   
                   data = transformed_train,
                   
                   method = "nnet",
                   
                   trControl = fitControl_final,
                   
                   metric = "ROC",
                   
                   tuneGrid = data.frame(size = 15,
                                         
                                         decay = 0
                   )
)

ANN_pred_train <- predict(ANN_final, newdata = transformed_train)

ANN_pred_test <- predict(ANN_final, newdata = transformed_test)

confusionMatrix(data=ANN_pred_train, reference=transformed_train$TARGET)

confusionMatrix(data=ANN_pred_test, reference=transformed_test$TARGET)