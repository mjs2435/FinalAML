library(caret)

library(tidymodels)

library(tidyverse)

library(randomForest)

library(readr)
library(caret)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(yardstick)
library(randomForest)

library(vip)   # first install the "vip" package you haven't done it before
library(imbalance)
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
    return(df_red)
  }
  return(df_bal)
  
}
split_df <- function(df, prop = .7){
  return(sample(nrow(df), nrow(df) * prop))
}
oversample_train<- function(transformed_train){
  tot_pos = sum(transformed_train$TARGET == 'p')
  tot_neg = sum(transformed_train$TARGET == 'n')
  return(rbind(rwo(data.frame(transformed_train), tot_neg - tot_pos, "TARGET"), transformed_train))
}


# d = oversample_train(transformed_train)


# preds = predict(RF, newdata=ts, type='prob')
# act_pos_preds = preds[ts$TARGET == 'p']
# act_neg_preds = preds[ts$TARGET == 'n']

# hist(act_pos_preds)
# hist(act_neg_preds)


# oversample_tr <- function(df, pos = 'p', neg = 'n'){
#   tot_pos = sum(df$TARGET == 'p')
#   tot_neg = sum(df$TARGET == 'n')
#   
#   
#   
#   max_class = c(pos, neg)[ifelse(tot_pos < tot_neg, 1, 2)] # probably neg
#   min_class = c(pos, neg)[ifelse(tot_pos > tot_neg, 1, 2)] # probably pos
#   
#   max_df = df[df$TARGET == max_class,]
#   min_df = df[df$TARGET == min_class,]
#   
#   sampled_df = min_df[sample(1:nrow(min_df), max(tot_pos, tot_neg), replace = TRUE),]
#   
#   
#   # for(x in s){
#   #   sampled_df = rbind(min_df[x,], sampled_df)
#   #   # print(x)
#   # }
#   
#   
#   return(rbind(max_df, sampled_df))
# }

# t1 = oversample_tr(transformed_train)

do_pca <- function(df, targ_name = "TARGET", num_comp = 10){
  
  targ = df[[targ_name]]
  df[[targ_name]] <- NULL
  
  pca_raw = prcomp(df)
  pca_vals = pca_raw$x[,1:num_comp]
  df_tot = data.frame(cbind(TARGET = targ, pca_vals))
  df_tot$TARGET = factor(df_tot$TARGET, labels = c('n', 'p'))
  return(df_tot)
}

prob_yes <- function(object, newdata) {                        # wrapper function
  
  predict(object, newdata = newdata, type = "prob")[, "p"]
  
}

do_vip <- function(model, train_dat){
  vip(model, method = "permute", train = train_dat, target = "TARGET",
      
      metric = "roc_auc", reference_class = "p", pred_wrapper = prob_yes)
}

super_learner <- function(M1, M2, M3, W) {
  
  weighted_average <- t(W*t(cbind(M1, M2, M3))) %>% apply(1, sum)
  
  final_prediction <- ifelse(weighted_average > 0.5, "p", "n") %>% factor(levels = c("n", "p"))
  
  return(final_prediction)
  
}

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


final_stacked_mod <- function(train, test, wt_metric = "Kappa"){
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
  RF_Kappa <- RF_train_results$overall[wt_metric]

  # SVM
  SVM <- train(TARGET ~ .,
               data = train, 
               method = "svmLinear",
               trControl = fitControl_final,
               verbose = FALSE,
               tuneGrid = data.frame(C = 0.1),
               metric = "ROC")
  SVM_pred_train <- predict.train(SVM, newdata = train %>% select(-TARGET))
  SVM_train_results <- confusionMatrix(train$TARGET, SVM_pred_train)
  SVM_Kappa <- SVM_train_results$overall[wt_metric]
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
               metric = "ROC")
  XGB_pred_train <- predict(XGB, newdata = train)
  XGB_train_results <- confusionMatrix(train$TARGET, XGB_pred_train)
  XGB_Kappa <- XGB_train_results$overall[wt_metric]
  Weights <- c((RF_Kappa)^2/sum((RF_Kappa)^2 + (SVM_Kappa)^2 + (XGB_Kappa)^2),
               (SVM_Kappa)^2/sum((RF_Kappa)^2 + (SVM_Kappa)^2 + (XGB_Kappa)^2),
               (XGB_Kappa)^2/sum((RF_Kappa)^2 + (SVM_Kappa)^2 + (XGB_Kappa)^2))
  # Making predictions
  RF_prediction <- predict(RF, newdata = test, type = "prob")[, "p"]
  SVM_prediction <- predict.train(SVM, newdata = test %>% select(-TARGET), type = "prob")[, "p"]
  XGB_prediction <- predict(XGB, newdata = test, type = "prob")[, "p"]
  SP_prediction <- super_learner(RF_prediction, SVM_prediction, XGB_prediction, Weights)
  # print(test$TARGET)
  # print(SP_prediction)
  SP_results <- confusionMatrix(test$TARGET, SP_prediction)
  return(SP_results)
  
}

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

class_prob_pred <- function(mod_fin, df, new_prob = 0.15) {
  preds <- predict(mod_fin, newdata=df, type = 'prob')
  pp = as.factor(ifelse(preds$p > new_prob, 'p', 'n'))
  return(pp)
}