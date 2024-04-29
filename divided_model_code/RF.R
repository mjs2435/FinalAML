#### RUN NO_SCALE_PREPROC.R AND DEP.R FIRST ####



# Random Forest Training and Evaluation



# a / (a + b)



# initial_rf <- randomForest(TARGET ~., data = transformed_train)

# plot(initial_rf)






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



pred_tr = class_prob_pred(mod_fin, tr)
pred_ts = class_prob_pred(mod_fin, ts)

# confusionMatrix(data=rf_pred_train, reference=as.factor(tr$TARGET))

# confusionMatrix(data=rf_pred_test, reference=as.factor(ts$TARGET))

# hist(rf_pred_test$p[ts$TARGET == 'p'], main = "Histogram of Probability for Test Set on Presences", xlab = "Probability")
# hist(rf_pred_test$n[ts$TARGET == 'n'], main = "Histogram of Probability for Test Set on Absences", xlab = "Probability")

confusionMatrix(data = pred_tr, reference = as.factor(tr$TARGET))
confusionMatrix(data = pred_ts, reference = as.factor(ts$TARGET))




tr$TARGET = as.factor(tr$TARGET)

do_vip(mod_fin, tr)

vip(mod_fin, method = "permute", train = tr, target = "TARGET",
    
    metric = "logloss", reference_class = "p", pred_wrapper = prob_yes, event_level = "first")