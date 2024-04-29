## Stacked Models (Weighted Ensemble Model)

# Base Learners


# d_all = d_bind[sample(nrow(d_bind), 1000),]
# d_all = balance_df(d_bind, final_row = 1000)
# sp = split_df(d_all)
# b = balance_df(df8, final_row = 10000)
tr = oversample_train(transformed_train)
ts = transformed_test

zp = nearZeroVar(tr, saveMetrics = TRUE)

tr= tr[,!zp$zeroVar]
ts = ts[,!zp$zeroVar]

# ts$TARGET
final_stacked_mod(tr, ts, wt_metric = "AccuracyLower")





