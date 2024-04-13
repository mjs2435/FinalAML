library(caret)

library(tidymodels)

library(tidyverse)

library(randomForest)

# Random Forest Training and Evaluation

transformed_train$TARGET[transformed_train$TARGET == 0] = 'n'
transformed_train$TARGET[transformed_train$TARGET == 1] = 'p'
transformed_train$TARGET = as.factor(transformed_train$TARGET)

transformed_test$TARGET[transformed_test$TARGET == 0] = 'n'
transformed_test$TARGET[transformed_test$TARGET == 1] = 'p'
transformed_test$TARGET = as.factor(transformed_test$TARGET)

tot_neg = sum(transformed_train$TARGET == 'n')
tot_pos = sum(transformed_train$TARGET == 'p')

a / (a + b)



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






# XGBoost Training and Evaluation

resample <- trainControl(method = "cv",
                         
                         number = 5,
                         
                         classProbs = TRUE,
                         
                         summaryFunction = twoClassSummary) 



hyper_grid <- expand.grid(nrounds = c(100, 200, 300),   
                          
                          max_depth = c(4, 6, 8, 10), 
                          
                          eta = c(0.05, 0.1, 0.2, 0.3),    
                          
                          min_child_weight = c(5, 10, 15), 
                          
                          subsample = 0.6, 
                          
                          gamma = 0,
                          
                          colsample_bytree = 1)



xg_fit <- train(X16 ~ .,
                
                data = crx_train, 
                
                method = "xgbTree",
                
                trControl = resample, 
                
                tuneGrid = hyper_grid,
                
                metric = "ROC")
ggplot(xg_fit)

plot(xg_fit, metric = "ROC", plotType = "level")

# Final Model

fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data


XG_final <- train(X16 ~., 
                  
                  data = crx_train,
                  
                  method = "xgbTree",
                  
                  trControl = fitControl_final,
                  
                  metric = "ROC",
                  
                  tuneGrid = data.frame(nrounds = 100,
                                        
                                        max_depth = 8,
                                        
                                        eta = 0.05,
                                        
                                        gamma = 0,
                                        
                                        colsample_bytree = 1,
                                        
                                        min_child_weight = 5,
                                        
                                        subsample = .6)
)

xg_pred_train <- predict(XG_final, newdata = crx_train)

xg_pred_test <- predict(XG_final, newdata = crx_test)

confusionMatrix(data=xg_pred_train, reference=crx_train$X16)

confusionMatrix(data=xg_pred_test, reference=crx_test$X16)


# SVM Training and Evaluation

## Support Vector Machine (SVM): Tuning hyperparameters

resample <- trainControl(method = "cv",
                         
                         number = 5,
                         
                         classProbs = TRUE,
                         
                         summaryFunction = twoClassSummary)




SVMGrid_Lin <- expand.grid(C = c(0.01, 0.1, 1, 10))  # Linear SVM


SVMGrid_Poly <- expand.grid(C = c(0.01, 0.1, 1, 10),   # Polynomial SVM
                            
                            degree = c(2, 3),
                            
                            scale = 1)


SVMGrid_RBF <- expand.grid (sigma = c(0.01, 0.1, 1, 10),  # RBF (Radial Basis Function) SVM
                            
                            C = c(0.01, 0.1, 1, 10))




# Linear SVM

set.seed(1)

SVM_Linear <- train(X16 ~ ., data = crx_train,
                    
                    method = "svmLinear",
                    
                    trControl = resample,
                    
                    verbose = FALSE,
                    
                    tuneGrid = SVMGrid_Lin,
                    
                    metric = "ROC")



# Polynomial SVM

set.seed(1)

SVM_Poly <- train(X16 ~ ., data = crx_train,
                  
                  method = "svmPoly",
                  
                  trControl = resample,
                  
                  verbose = FALSE,
                  
                  tuneGrid = SVMGrid_Poly,
                  
                  metric = "ROC")



# RBF (Radial Basis Function) SVM

set.seed(1)

SVM_RBF <- train(X16 ~ ., data = crx_train,
                 
                 method = "svmRadial",
                 
                 trControl = resample,
                 
                 verbose = FALSE,
                 
                 tuneGrid = SVMGrid_RBF,
                 
                 metric = "ROC")




### Comparing tuning results

resamps <- resamples(list(SVM_Linear = SVM_Linear,
                          
                          SVM_Poly = SVM_Poly,
                          
                          SVM_RBF = SVM_RBF))

summary(resamps)


# visualization

ggplot(SVM_Linear)


ggplot(SVM_Poly)

plot(SVM_Poly, metric = "ROC", plotType = "level")


ggplot(SVM_RBF)

plot(SVM_RBF, metric = "ROC", plotType = "level")

ggplot(resamps)

# Redo of linear SVM, smaller cost values

SVMGrid_Lin_2 <- expand.grid(C = c(0.01, 0.05, 0.1, 0.2))

SVM_Linear_2 <- train(X16 ~ ., data = crx_train,
                      
                      method = "svmLinear",
                      
                      trControl = resample,
                      
                      verbose = FALSE,
                      
                      tuneGrid = SVMGrid_Lin_2,
                      
                      metric = "ROC")

ggplot(SVM_Linear_2)


# Final With Evaluation

SVM_fitControl_final <- trainControl(method = "none", classProbs = TRUE, summaryFunction = twoClassSummary)   # no resampling applies to the data

SVM_final <- train(X16 ~ ., data = crx_train,
                   
                   method = "svmLinear",
                   
                   trControl = SVM_fitControl_final,
                   
                   verbose = FALSE,
                   
                   tuneGrid = data.frame(C=0.01),
                   
                   metric = "ROC")

SVM_pred_train <- predict(SVM_final, newdata = crx_train)

SVM_pred_test <- predict(SVM_final, newdata = crx_test)

confusionMatrix(data=SVM_pred_train, reference=crx_train$X16)

confusionMatrix(data=SVM_pred_test, reference=crx_test$X16)

#### LAB 3 ####

## Classification Problems


attrition$Attrition <- factor(attrition$Attrition, levels = c("Yes", "No"))


resample_1 <- trainControl(method = "cv",
                           
                           number = 5,
                           
                           classProbs = TRUE,
                           
                           summaryFunction = twoClassSummary)    # adds more metrics to the model



hyper_grid <- expand.grid(mtry = c(13, 15, 17),
                          
                          splitrule = c("gini", "extratrees"),
                          
                          min.node.size = c(5, 7, 9))  # example: random forest method



rf_fit <- train(Attrition ~ .,
                
                data = attrition_train, 
                
                method = "ranger",
                
                verbose = FALSE,
                
                trControl = resample_1, 
                
                tuneGrid = hyper_grid,
                
                metric = "ROC")


ggplot(rf_fit, metric = "Sens")   # Sensitivity

ggplot(rf_fit, metric = "ROC")    # AUC ROC

ggplot(rf_fit, metric = "Spec")   # Specificity


fitControl_final <- trainControl(method = "none",
                                 
                                 classProbs = TRUE)


RF_final <- train(Attrition ~., 
                  
                  data = attrition_train,
                  
                  method = "ranger",
                  
                  trControl = fitControl_final,
                  
                  metric = "ROC",
                  
                  tuneGrid = data.frame(.mtry = 13,
                                        
                                        .min.node.size = 7,
                                        
                                        .splitrule = "gini"))



# Training set results

RF_pred_train <- predict(RF_final, newdata = attrition_train)

RF_train_results <- confusionMatrix(attrition_train$Attrition, RF_pred_train)

print(RF_train_results)

# Test set results

RF_pred_test <- predict(RF_final, newdata = attrition_test)

RF_test_results <- confusionMatrix(attrition_test$Attrition, RF_pred_test)

print(RF_test_results)


#### LAB 9, ANN METHODS ####

## Stacked Models (Weighted Ensemble Model)

# Base Learners

set.seed(1)

fitControl_final <- trainControl(method = "none", classProbs = TRUE)

# Random Forest (RF)

RF <- train(Attrition ~ .,
            
            data = attrition_train,
            
            method = "ranger",
            
            trControl = fitControl_final,
            
            metric = "ROC",
            
            verbose = FALSE,
            
            tuneGrid = data.frame(mtry = 13,
                                  
                                  min.node.size = 7,
                                  
                                  splitrule = "extratrees"),
            
            num.trees = 300,
            
            importance = "impurity")


RF_pred_train <- predict(RF, newdata = attrition_train)

RF_train_results <- confusionMatrix(attrition_train$Attrition, RF_pred_train)

RF_Kappa <- RF_train_results$overall["Kappa"]


# Support Vector Machine (SVM)

SVM <- train(Attrition ~ .,
             
             data = attrition_train, 
             
             method = "svmLinear",
             
             trControl = fitControl_final,
             
             verbose = FALSE,
             
             tuneGrid = data.frame(C = 0.1),
             
             metric = "ROC",
             
             importance = TRUE)



SVM_pred_train <- predict.train(SVM, newdata = attrition_train %>% select(-Attrition))

SVM_train_results <- confusionMatrix(attrition_train$Attrition, SVM_pred_train)

SVM_Kappa <- SVM_train_results$overall["Kappa"]


# Extreme Gradient Boosting Machine (XGBoost)

XGB <- train(Attrition ~ .,
             
             data = attrition_train, 
             
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


XGB_pred_train <- predict(XGB, newdata = attrition_train)

XGB_train_results <- confusionMatrix(attrition_train$Attrition, XGB_pred_train)

XGB_Kappa <- XGB_train_results$overall["Kappa"]


# Super Learner (SP)

# Weights 

Weights <- c((RF_Kappa)^2/sum((RF_Kappa)^2 + (SVM_Kappa)^2 + (XGB_Kappa)^2),
             
             (SVM_Kappa)^2/sum((RF_Kappa)^2 + (SVM_Kappa)^2 + (XGB_Kappa)^2),
             
             (XGB_Kappa)^2/sum((RF_Kappa)^2 + (SVM_Kappa)^2 + (XGB_Kappa)^2))


super_learner <- function(M1, M2, M3, W) {
  
  weighted_average <- t(W*t(cbind(M1, M2, M3))) %>% apply(1, sum)
  
  final_prediction <- ifelse(weighted_average > 0.5, "Yes", "No") %>% factor(levels = c("Yes", "No"))
  
  return(final_prediction)
  
}


# Making predictions

RF_prediction <- predict(RF, newdata = attrition_test, type = "prob")[, "Yes"]

SVM_prediction <- predict.train(SVM, newdata = attrition_test %>% select(-Attrition), type = "prob")[, "Yes"]

XGB_prediction <- predict(XGB, newdata = attrition_test, type = "prob")[, "Yes"]

SP_prediction <- super_learner(RF_prediction, SVM_prediction, XGB_prediction, Weights)

SP_results <- confusionMatrix(attrition_test$Attrition, SP_prediction)


## Interpretable ML

# Permutation-based Feature Importance 

# RF

vip(RF)

vip(RF, num_features = 20)

vip(RF, num_features = 20, geom = "point")


# XGB

vip(XGB)


# SVM

prob_yes <- function(object, newdata) {                        # wrapper function
  
  predict(object, newdata = newdata, type = "prob")[, "Yes"]
  
}


vip(SVM, method = "permute", train = attrition_train, target = "Attrition",
    
    metric = "roc_auc", reference_class = "Yes", pred_wrapper = prob_yes)


