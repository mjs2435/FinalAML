###############################################################
############################# R F #############################
###############################################################

############################# [1] #############################
tr = transformed_train
ts = transformed_test

hyper_grid <- expand.grid(mtry = c(5, 7, 9), 
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          min.node.size = c(5, 7, 9)) # 
mod = train_rf(tr, hyper_grid)

best_grid = data.frame(mtry = 9,
                       min.node.size = 13,
                       splitrule = "hellinger")
mod_fin = final_RF_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [2] #############################
b = balance_df(rbind(transformed_train, transformed_test), final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]

hyper_grid <- expand.grid(mtry = c(13, 15, 17, 20, 22, 25), 
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          min.node.size = c(5, 7, 9)) # 

mod = train_rf(tr, hyper_grid)
best_grid = data.frame(mtry = 13,
                       min.node.size = 9,
                       splitrule = "hellinger")
mod_fin = final_RF_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [3] #############################
b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]

hyper_grid <- expand.grid(mtry = c(13, 15, 17, 20, 22, 25), 
                          splitrule = c("gini", "extratrees", "hellinger"), # 
                          min.node.size = c(5, 7, 9)) # 
mod = train_rf(tr, hyper_grid)

best_grid = data.frame(mtry = 25,
                       min.node.size = 9,
                       splitrule = "hellinger")
mod_fin = final_RF_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [4] #############################
b = balance_df(rbind(transformed_train, transformed_test), final_row = 10000)
pc = do_pca(b)
sp = split_df(pc)
tr = pc[sp,]
ts = pc[-sp,]

hyper_grid <- expand.grid(mtry = c(5,6,7,8,9), 
                          splitrule = c("gini", "extratrees", "hellinger"), 
                          min.node.size = c(5, 7, 9))
mod = train_rf(tr, hyper_grid)

best_grid = data.frame(mtry = 5,
                       min.node.size = 9,
                       splitrule = "extratrees")
mod_fin = final_RF_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [5] #############################
b = balance_df(rbind(transformed_train, transformed_test), final_row = 10000)
pc = do_pca(b)
sp = split_df(pc)
tr = pc[sp,]
ts = pc[-sp,]

hyper_grid <- expand.grid(mtry = c(2,3,4,5,6,7), 
                          splitrule = c("gini", "extratrees", "hellinger"), 
                          min.node.size = c(5, 7, 9))
mod = train_rf(tr, hyper_grid)

best_grid = data.frame(mtry = 7,
                       min.node.size = 9,
                       splitrule = "extratrees")
mod_fin = final_RF_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [6] #############################
tr = oversample_train(transformed_train)
ts = transformed_test

hyper_grid <- expand.grid(mtry = c(2,3,4,5,6,7), 
                          splitrule = c("gini", "extratrees", "hellinger"), 
                          min.node.size = c(5, 7, 9))
mod = train_rf(tr, hyper_grid)

best_grid = data.frame(mtry = 2,
                       min.node.size = 5,
                       splitrule = "extratrees")
mod_fin = final_RF_mod(tr, ts, best_grid)

pred_train <- class_prob_pred(mod_fin, tr, new_prob = 0.15) # CHANGED
pred_test <- class_prob_pred(mod_fin, ts)  # CHANGED

###############################################################
############################# ANN #############################
###############################################################

############################# [7] #############################
tr = transformed_train
ts = transformed_test

hyper_grid <- expand.grid(size = c(2, 5, 10, 15),
                    decay = c(0, 0.01, 0.05, 0.1))  
mod = train_ANN(tr, hyper_grid)

best_grid = data.frame(size = 5, decay = 0.1)
mod_fin = final_ANN_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [8] #############################
sp = split_df(df8)
tr = df8[sp,]
ts = df8[-sp,]

hyper_grid <- expand.grid(size = c(2, 5, 10, 15),
                          decay = c(0, 0.01, 0.05, 0.1))  
mod = train_ANN(tr, hyper_grid)

best_grid = data.frame(size = 5, decay = 0.1)
mod_fin = final_ANN_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

###############################################################
############################# XGB #############################
###############################################################

############################# [9] #############################
tr = transformed_train
ts = transformed_test

hyper_grid <- expand.grid(nrounds = c(100, 150, 200, 250, 300),   
                    max_depth = c(1, 2, 4, 8, 16), 
                    eta = c(0.05, 0.3),    
                    min_child_weight = c(5, 15), 
                    subsample = c(0.4, 0.6), 
                    gamma = 0,
                    colsample_bytree = 1)
mod = train_XGB(tr, hyper_grid)

best_grid = data.frame(nrounds = 300,
                       max_depth = 4,
                       eta = 0.05,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 15,
                       subsample = 0.4)
mod_fin = final_XGB_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

############################# [10] #############################
b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]

hyper_grid <- expand.grid(nrounds = c(300, 400, 500, 600),   
                          max_depth = c(4,8,12,16), 
                          eta = c(0.001, 0.005, 0.01, 0.05),    
                          min_child_weight = c(14,15,16,17), 
                          subsample = 0.4, 
                          gamma = 0,
                          colsample_bytree = 1)
mod = train_XGB(tr, hyper_grid)

best_grid = data.frame(nrounds = 400,
                       max_depth = 4,
                       eta = 0.05,
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 0.05,
                       subsample = 0.4)
mod_fin = final_XGB_mod(tr, ts, best_grid)

pred_train <- predict(mod_fin, newdata = tr)
pred_test <- predict(mod_fin, newdata = ts)

################################################################
############################# S P ##############################
################################################################

############################# [11] #############################
b = balance_df(rbind(transformed_train, transformed_test), final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]

zp = nearZeroVar(tr, saveMetrics = TRUE)

tr = tr[,!zp$zeroVar]
ts = ts[,!zp$zeroVar]

final_stacked_mod(tr, ts, wt_metric = "Kappa")