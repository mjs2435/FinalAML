#### RUN NO_SCALE_PREPROC.R AND DEP.R FIRST ####



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

hyper_grid <- expand.grid(mtry = c(2,5,7), 
                          
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          
                          min.node.size = c(5, 7, 9)) # 



#b = balance_df(rbind(transformed_train, transformed_test), final_row = 10000)
#bpca = do_pca(b)
#bpca_t = trans_target(bpca)
#sp = split_df(bpca_t)
tr = oversample_train(transformed_train)
ts = transformed_test
mod = train_rf(tr, hyper_grid)

ggplot(mod)

plot(mod, metric = "ROC", plotType = "level")
plot(mod, metric = "Sens", plotType = "level")
plot(mod, metric = "Spec", plotType = "level")
plot(mod, metric = "ROCSD", plotType = "level")

best_grid = data.frame(mtry = 7,
                       
                       min.node.size = 2,
                       
                       splitrule = "extratrees")

mod_fin = final_RF_mod(tr, ts, best_grid)

rf_pred_train <- predict(mod_fin, newdata=tr, type = 'prob')

rf_pred_test <- predict(mod_fin, newdata = ts, type='prob')


# confusionMatrix(data=rf_pred_train, reference=as.factor(tr$TARGET))

# confusionMatrix(data=rf_pred_test, reference=as.factor(ts$TARGET))

hist(rf_pred_test$p[ts$TARGET == 'p'], main = "Histogram of Probability for Test Set on Presences", xlab = "Probability")
hist(rf_pred_test$n[ts$TARGET == 'n'], main = "Histogram of Probability for Test Set on Absences", xlab = "Probability")



pp = ifelse(rf_pred_test$p > .138, 'p', 'n')

pn = ifelse(rf_pred_train$p >.138, 'p', 'n')

confusionMatrix(data = as.factor(pp), reference = as.factor(ts$TARGET))
confusionMatrix(data = as.factor(pn), reference = as.factor(tr$TARGET))



tr$TARGET = as.factor(tr$TARGET)

do_vip(mod_fin, tr)

vip(mod_fin, method = "permute", train = tr, target = "TARGET",
    
    metric = "logloss", reference_class = "p", pred_wrapper = prob_yes, event_level = "first")