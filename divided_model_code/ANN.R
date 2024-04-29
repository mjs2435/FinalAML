


grid <- expand.grid(size = c(2, 5, 10),
                    decay = c(0, 0.1, 0.05, 0.01))  

b = balance_df(rbind(transformed_train, transformed_test), final_row = 10000)
b$TARGET = as.factor(b$TARGET)
bpca = do_pca(b)
sp = split_df(bpca)
tr = bpca[sp,]
ts = bpca[-sp,]


tr = trans_target(tr)
ts = trans_target(ts)

ANN_tr = train_ANN(tr, grid)


ggplot(ANN_tr)

plot(ANN_tr, metric = "ROC", plotType = "level")
plot(ANN_tr, metric = "Sens", plotType = "level")
plot(ANN_tr, metric = "Spec", plotType = "level")
plot(ANN_tr, metric = "ROCSD", plotType = "level")
grid_best = data.frame(size = 1, decay = .01)

ANN_fn = final_ANN_mod(tr, ts, grid_best)

ANN_pred_train <- predict(ANN_fn, newdata = tr)

ANN_pred_test <- predict(ANN_fn, newdata = ts)

confusionMatrix(data=ANN_pred_train, reference=tr$TARGET)

confusionMatrix(data=ANN_pred_test, reference=ts$TARGET)

do_vip(ANN_fn, tr)

vip(ANN_fn, method = "permute", train = tr, target = "TARGET",
    
    metric = "logloss", reference_class = "p", pred_wrapper = prob_yes, event_level = "first")
