#Final model for CPI:
#The best model for CPI:"Lag=1, with CPI as independent variable, dim = 8, window size = 60"

#Perequisite
library(tidyverse)
library(glmnet)
library(doParallel)
registerDoParallel(detectCores()-1)
set.seed(10)

dim=8

load("data/dataset_inf.Rdata")
load("data/data_oos.Rdata")




#Lag=1 data:
data_1=rbind(X,real.X)
data_1=merge(rbind(cpi,real.cpi),data_1,by="month",all = T)
data_1=data_1 |> mutate(inflation=log(CPI)-log(lag(CPI,n=12)))
data_1=data_1 |> mutate(y=lead(inflation))|> select(!c("CPI"))

#Train the PCA model
pca_data=X |> select(!c("month"))
PCA=princomp(pca_data)

##------------------------------#Functions:---------------------------##

pca_lasso_test2 <- function(i,data) {
  j=i+59.  #training data end month
  k=i+60. #test month
  y_train=data[i:j,"y"]
  y_test=data[k,"y"]
  x_train=data[i:j,] |> select(!c("month","y","inflation"))
  x_train_inflation=data[i:j,"inflation"] 
  x_test=data[k,] |> select(!c("month","y","inflation"))
  x_test_inflation=data[k,"inflation"]
  
  
  #collect 8 Principal Components and train the lasso regression.
  PC=predict(PCA,newdata = x_train)[,1:dim]
  PCs=cbind(PC,x_train_inflation)
  
  #Calculate scores for testing
  PC_test=predict(PCA,newdata = x_test)[,1:dim]
  PC_test2=append(PC_test,x_test_inflation)
  
  #find lambda
  cv_model <- cv.glmnet(PCs, y_train, alpha = 1,nfolds = 3)
  best_lambda <- cv_model$lambda.min
  
  #display optimal lambda value
  best_lambda
  
  #view coefficients of best model
  
  pca_lasso2 <- glmnet(PCs, y_train, alpha = 1, lambda =best_lambda)
  coef(pca_lasso2)
  summary(pca_lasso2)
  
  #make a prediction for the response value of a new observation
  
  yp=predict(pca_lasso2, s = best_lambda, newx = PC_test2)[1,1]
  return(c((k+1),yp,y_test))
  
}


rsquare_test <- function(yt,y_predicted) {
  sst <- sum((yt - mean(yt))^2)
  sse <- sum((y_predicted - yt)^2)
  
  rsq <- 1 - sse/sst
  
  cat("The R-square is:",rsq,"\n") 
  return(rsq)
}

rep_test2 <- function(data){
  
  result <-foreach (x = 108:137,.combine = "rbind") %dopar% {
    pca_lasso_test2(i = x,data)
  }
  result=data.frame(result)
  colnames(result) <- c("month","y_predicted","y_true")
  return(result)
  
}

result <- rep_test2(data_1)
yp=result$y_predicted
yt=result$y_true

rsquare_test(yt,yp)



