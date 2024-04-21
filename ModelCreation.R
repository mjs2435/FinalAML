library(caret)

library(tidymodels)

library(tidyverse)

library(randomForest)

# Random Forest Training and Evaluation


trans_target <- function(df, old_names = c(1,2), new_names = c("n", "p"), targ_name = "TARGET"){
  df[[targ_name]][df[[targ_name]] == old_names[1]] = new_names[1]
  df[[targ_name]][df[[targ_name]] == old_names[2]] = new_names[2]
  transformed_train$TARGET = as.factor(transformed_train$TARGET)
  return(df)
  
}
#corr_df = trans_target(transformed_train)
#print(typeof(2) == typeof(1))
balance_df <- function(df_new, final_row = -1){
  if (typeof(df_new$TARGET[1]) == typeof(1)){
    df = trans_target(df_new)
  }
  else{
    df = df_new
  }
  
  all_neg = df[df$TARGET == 'n',]
  all_pos = df[df$TARGET == 'p',]
  print(nrow(all_pos))
  tot_neg = nrow(all_neg)
  tot_pos = nrow(all_pos)
  if (tot_neg > tot_pos){ # likely here
    min_cut = tot_pos
    max_cut = tot_neg
    df_to_subset = all_neg
    df_to_bind = all_pos
  } else {
    min_cut = tot_neg
    max_cut = tot_pos
    df_to_subset = all_pos
    df_to_bind = all_neg
  }
  subs_large = df_to_subset[sample(max_cut, min_cut),]
  #print(sample(max_cut, min_cut))
  df_bal = rbind(subs_large, df_to_bind)
  #print(final_row)
  df_bal$TARGET = as.factor(df_bal$TARGET)
  if (final_row != -1 & nrow(df_bal) > final_row){
    df_red = df_bal[sample(nrow(df_bal), final_row),]
    print(nrow(df_red))
    return(df_red)
  }
  return(df_bal)
  
}
split_df <- function(df, prop = .7){
  return(sample(nrow(df), nrow(df) * prop))
}

do_pca <- function(df, targ_name = "TARGET", num_comp = 10){
  
  targ = df[[targ_name]]
  
  df[[targ_name]] <- NULL
  pca_raw = prcomp(df)
  pca_vals = pca_raw$x[,1:num_comp]
  return(data.frame(cbind(TARGET = targ, pca_vals)))
}



all_dat = rbind(transformed_train, transformed_test)
pc_df = do_pca(all_dat, "TARGET")
bpc = balance_df(pc_df)
s_plit = split_df(bpc)

pc_train=bpc[s_plit,]
pc_test=bpc[-s_plit,]

sum(bpc$TARGET=="p")
sum(pc_train$TARGET == "n")

prob_yes <- function(object, newdata) {                        # wrapper function
  
  predict(object, newdata = newdata, type = "prob")[, "p"]
  
}

do_vip <- function(model, train_dat){
  train_dat$TARGET = as.factor(train_dat$TARGET)
  vip(model, method = "permute", train = train_dat, target = "TARGET",
      
      metric = "roc_auc", reference_class = "p", pred_wrapper = prob_yes)
}


#transformed_train[s,]
#b = balance_df(transformed_train, final_row = 1000)

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


tts = transformed_test
tot_neg_tst = sum(transformed_test$TARGET == 'n')
tot_pos_tst = sum(transformed_test$TARGET == 'p')

test_neg = tts[tts$TARGET == 'n',]
test_pos = tts[tts$TARGET == 'p',]
subs_neg_tst = test_neg[sample(tot_neg_tst, tot_pos_tst),]
bal_test = rbind(subs_neg_tst, test_pos)

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

rf_pred_test_bal_set = predict(RF_final_bal, newdata = bal_test)

confusionMatrix(data=rf_pred_train_bal_actual, reference=bal_train$TARGET)

confusionMatrix(data=rf_pred_train_bal, reference=transformed_train$TARGET)

confusionMatrix(data=rf_pred_test_bal, reference=transformed_test$TARGET)

confusionMatrix(data=rf_pred_test_bal_set, reference=bal_test$TARGET)

#### RF with Principal Components, Balanced ####

hyper_grid <- expand.grid(mtry = c(5,7,9), 
                          
                          splitrule = c("gini", "extratrees", "hellinger"), 
                          
                          min.node.size = c(5, 7, 9)) 

rf_fit_bal <- train(TARGET ~ .,
                    
                    data = pc_train, 
                    
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
                      
                      data = pc_train,
                      
                      method = "ranger",
                      
                      trControl = fitControl_final_bal,
                      
                      metric = "ROC",
                      
                      tuneGrid = data.frame(mtry = 5,
                                            
                                            min.node.size = 5,
                                            
                                            splitrule = "extratrees"),
                      
                      num.trees = 200)

rf_pred_train_bal <- predict(RF_final_bal, newdata = pc_train)

rf_pred_test_bal <- predict(RF_final_bal, newdata = pc_test)

confusionMatrix(data=rf_pred_train_bal, reference=as.factor(pc_train$TARGET))

confusionMatrix(data=rf_pred_test_bal, reference=as.factor(pc_test$TARGET))

pc_train$TARGET = as.factor(pc_train$TARGET)

do_vip(RF_final_bal, pc_train)




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

SVM_Linear <- train(TARGET ~ ., data = transformed_train,
                    
                    method = "svmLinear",
                    
                    trControl = resample,
                    
                    verbose = FALSE,
                    
                    tuneGrid = SVMGrid_Lin,
                    
                    metric = "ROC")



# Polynomial SVM

set.seed(1)

SVM_Poly <- train(TARGET ~ ., data = transformed_train,
                  
                  method = "svmPoly",
                  
                  trControl = resample,
                  
                  verbose = FALSE,
                  
                  tuneGrid = SVMGrid_Poly,
                  
                  metric = "ROC")



# RBF (Radial Basis Function) SVM

set.seed(1)

SVM_RBF <- train(TARGET ~ ., data = transformed_train,
                 
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

RF <- train(TARGET ~ .,
            
            data = transformed_train,
            
            method = "ranger",
            
            trControl = fitControl_final,
            
            metric = "ROC",
            
            verbose = FALSE,
            
            tuneGrid = data.frame(mtry = 13,
                                  
                                  min.node.size = 7,
                                  
                                  splitrule = "extratrees"),
            
            num.trees = 300,
            
            importance = "impurity")


RF_pred_train <- predict(RF, newdata = transformed_train)

RF_train_results <- confusionMatrix(transformed_train$TARGET, RF_pred_train)

RF_Kappa <- RF_train_results$overall["Kappa"]


# Support Vector Machine (SVM)

ANN_final <- train(TARGET ~., 
                   
                   data = transformed_train,
                   
                   method = "nnet",
                   
                   trControl = fitControl_final,
                   
                   verbose = FALSE,
                   
                   metric = "ROC",
                   
                   tuneGrid = data.frame(size = 15,
                                         
                                         decay = 0
                   ),
                   
                   importance = "impurity"
)


SVM <- train(TARGET ~ .,
             
             data = attrition_train, 
             
             method = "svmLinear",
             
             trControl = fitControl_final,
             
             verbose = FALSE,
             
             tuneGrid = data.frame(C = 0.1),
             
             metric = "ROC",
             
             importance = TRUE)



ANN_pred_train <- predict.train(ANN, newdata = transformed_train %>% select(-TARGET))

ANN_train_results <- confusionMatrix(transformed_train$TARGET, ANN_pred_train)

ANN_Kappa <- ANN_train_results$overall["Kappa"]


# Extreme Gradient Boosting Machine (XGBoost)

XGB <- train(TARGET ~ .,
             
             data = transformed_train, 
             
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


XGB_pred_train <- predict(XGB, newdata = transformed_train)

XGB_train_results <- confusionMatrix(transformed_train$TARGET, XGB_pred_train)

XGB_Kappa <- XGB_train_results$overall["Kappa"]


# Super Learner (SP)

# Weights 

Weights <- c((RF_Kappa)^2/sum((RF_Kappa)^2 + (ANN_Kappa)^2 + (XGB_Kappa)^2),
             
             (ANN_Kappa)^2/sum((RF_Kappa)^2 + (ANN_Kappa)^2 + (XGB_Kappa)^2),
             
             (XGB_Kappa)^2/sum((RF_Kappa)^2 + (ANN_Kappa)^2 + (XGB_Kappa)^2))


super_learner <- function(M1, M2, M3, W) {
  
  weighted_average <- t(W*t(cbind(M1, M2, M3))) %>% apply(1, sum)
  
  final_prediction <- ifelse(weighted_average > 0.5, "p", "n") %>% factor(levels = c("p", "n"))
  
  return(final_prediction)
  
}


# Making predictions

RF_prediction <- predict(RF, newdata = transformed_test, type = "prob")[, "p"]

ANN_prediction <- predict.train(ANN, newdata = transformed_test %>% select(-TARGET), type = "prob")[, "p"]

XGB_prediction <- predict(XGB, newdata = transformed_test, type = "prob")[, "p"]

SP_prediction <- super_learner(RF_prediction, ANN_prediction, XGB_prediction, Weights)

SP_results <- confusionMatrix(transformed_test$TARGET, SP_prediction)


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


