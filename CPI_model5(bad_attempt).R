#Model 5.
#This model use predicted CPI to predict CPI. The out of sample R square is really bad.

#Perequisite
library(tidyverse)
library(glmnet)
set.seed(10)


load(url("https://github.com/zhentaoshi/Econ5821/raw/main/data_example/dataset_inf.Rdata"))


#-----------------Start-----------------#
st=Sys.time()

#Prepare datasets:
data_5=X
data_5=data_5 |> mutate(yp=NA,.before=1)
data_5[data_5$month %in% 1:13,"yp"] <- cpi[1:13,"CPI"]

#Train the PCA model
pca_data = X |> select(!c("month"))
PCA=princomp(pca_data)

##------------------------------#Functions:---------------------------##

pca_lasso_test2 <- function(i,data) {
  j=i+11
  k=i+12
  y_train=data[(i+1):(j+1),"yp"]
  y_train=y_train$yp

  x_train=data[i:j,] |> select(!c("month","yp"))
  x_test=data[k,] |> select(!c("month","yp"))
  x_cpi=data[k,"yp"]
  x_cpi=x_cpi[[1,1]]
  
  #collect 8 Principal Components and train the lasso regression.
  PC=predict(PCA,newdata = x_train)[,1:dim]
  
  #Calculate scores for testing
  PC_test=predict(PCA,newdata = x_test)[,1:dim]
  
  #find lambda
  cv_model <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1,nfolds = 3)
  best_lambda <- cv_model$lambda.min
  
  #display optimal lambda value
  best_lambda
  
  #view plot of test MSE's vs. lambda values
  
  #view coefficients of best model
  PCs=cbind(PC,y_train)
  pca_lasso2 <- glmnet(PCs, y_train, alpha = 1, lambda =best_lambda)
  coef(pca_lasso2)
  summary(pca_lasso2)
  
  #make a prediction for the response value of a new observation
  PC_test2=append(PC_test,x_cpi)
  
  yp=predict(pca_lasso2, s = best_lambda, newx = PC_test2)[1,1]
  return(yp)
}


rsquare_test <- function(yt,y_predicted) {
  sst <- sum((yt - mean(yt))^2)
  sse <- sum((y_predicted - yt)^2)
  
  rsq <- 1 - sse/sst
  
  cat("The R-square is:",rsq,"\n") 
  return(rsq)
}



dim=8

for (i in 1:30) {
  
  data_5[i+13,"yp"]=pca_lasso_test2(1,data_5)
  
}

rsquare_test <- function(y_true,y_predicted) {
  sst <- sum((y_true - mean(y_true))^2)
  sse <- sum((y_predicted - y_true)^2)
  
  rsq <- 1 - sse/sst
  
  cat("The R-square is:",rsq,"\n") 
  return(rsq)
}

yt=cpi[1:13,"CPI"]$CPI
yp=data_5[1:13,"yp"]$yp

rsquare_test(y_true = yt,y_predicted = yp)



yt=cpi[14:43,"CPI"]$CPI
yp=data_5[14:43,"yp"]$yp

rsquare_test(y_true = yt,y_predicted = yp)
