# SVM Training and Evaluation

## Support Vector Machine (SVM): Tuning hyperparameters



train_SVM <- function(train, test, grid_lin, grid_poly, grid_rbf){
  resample <- trainControl(method = "cv",
                           number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
  # Linear SVM
   set.seed(1)
   # print(grid_lin)
   SVM_Linear <- train(TARGET ~ ., data = train,
                       method = "svmLinear",
                       trControl = resample,
                       verbose = FALSE,
                       tuneGrid = grid_lin,
                       minstep = 0.00001,
                       metric = "ROC")


   # Polynomial SVM
  
   # set.seed(1)
   # 
   # SVM_Poly <- train(TARGET ~ ., data = train,
   #                   method = "svmPoly",
   #                   trControl = resample,
   #                   verbose = FALSE,
   #                   tuneGrid = grid_poly,
   #                   metric = "ROC")
  
  
  
   # RBF (Radial Basis Function) SVM
  
   # set.seed(1)
   # 
   # SVM_RBF <- train(TARGET ~ ., data = train,
   # 
   #                  method = "svmRadial",
   # 
   #                  trControl = resample,
   # 
   #                  verbose = FALSE,
   # 
   #                  tuneGrid = grid_rbf,
   # 
   #                  metric = "ROC")
   # 
  
  
  
   ### Comparing tuning results
  
  #  resamps <- resamples(list(SVM_Linear = SVM_Linear,
  # 
  #                            SVM_Poly = SVM_Poly,
  # 
  #                            SVM_RBF = SVM_RBF))
  # 
  # summary(resamps)
  
  
  #Visualization
  
  ggplot(SVM_Linear)
  
  
  # ggplot(SVM_Poly)
  # plot(SVM_Poly, metric = "ROC", plotType = "level")
  
  
  # ggplot(SVM_RBF)
  # 
  # plot(SVM_RBF, metric = "ROC", plotType = "level")
  # 
  # ggplot(resamps)
  # return(c(SVM_Linear, SVM_Poly, SVM_RBF))
  return(SVM_Linear)
}

SVMGrid_Lin <- expand.grid(C = c(0.01, 0.1, 1, 10))  # Linear SVM


SVMGrid_Poly <- expand.grid(C = c(0.01, 0.1, 1, 10),   # Polynomial SVM
                            degree = c(2, 3),
                            scale = 1)


SVMGrid_RBF <- expand.grid (sigma = c(0.01, 0.1, 1, 10),  # RBF (Radial Basis Function) SVM
                            C = c(0.01, 0.1, 1, 10))

b = balance_df(df8, final_row = 10000)
sp = split_df(b)
tr = b[sp,]
ts = b[-sp,]
print(SVMGrid_Lin)
ms = train_SVM(tr,ts, SVMGrid_Lin, SVMGrid_Poly, SVMGrid_RBF)
