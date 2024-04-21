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

prob_yes <- function(object, newdata) {                        # wrapper function
  
  predict(object, newdata = newdata, type = "prob")[, "p"]
  
}

do_vip <- function(model, train_dat){
  vip(model, method = "permute", train = train_dat, target = "TARGET",
      
      metric = "roc_auc", reference_class = "p", pred_wrapper = prob_yes)
}

super_learner <- function(M1, M2, M3, W) {
  
  weighted_average <- t(W*t(cbind(M1, M2, M3))) %>% apply(1, sum)
  
  final_prediction <- ifelse(weighted_average > 0.5, "p", "n") %>% factor(levels = c("p", "n"))
  
  return(final_prediction)
  
}