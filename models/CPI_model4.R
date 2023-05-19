#Model 4.
#This model use x_t-2 and y_t-1 to predict yt.

#Perequisite
library(tidyverse)
library(glmnet)
set.seed(10)
n=1
dim=8

load(url("https://github.com/zhentaoshi/Econ5821/raw/main/data_example/dataset_inf.Rdata"))


#-----------------Start-----------------#
st=Sys.time()

#Prepare datasets:
#Lag=2:
data_4=merge(cpi,X,by="month",all = T)
data_4=data_4 |> mutate(y=lead(CPI,n=2)) 
data_4=data_4 |> mutate(CPI=lead(CPI))|> filter(is.na(y)==F)

#Train the PCA model
pca_data=X |> select(!c("month"))
PCA=princomp(pca_data)

##------------------------------#Functions:---------------------------##

pca_lasso_test2 <- function(i,data) {
  j=i+11
  k=i+12
  y_train=data[i:j,"y"]
  y_test=data[k,"y"]
  x_train=data[i:j,] |> select(!c("month","y","CPI"))
  x_train_cpi=data[i:j,"CPI"] 
  x_test=data[k,] |> select(!c("month","y","CPI"))
  x_test_cpi=data[k,"CPI"]
  
  
  #collect 8 Principal Components and train the lasso regression.
  PC=predict(PCA,newdata = x_train)[,1:dim]
  PCs=cbind(PC,x_train_cpi)
  
  #Calculate scores for testing
  PC_test=predict(PCA,newdata = x_test)[,1:dim]
  PC_test2=append(PC_test,x_test_cpi)
  
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
  return(c(yp,y_test))
  
}


rsquare_test <- function(yt,y_predicted) {
  sst <- sum((yt - mean(yt))^2)
  sse <- sum((y_predicted - yt)^2)
  
  rsq <- 1 - sse/sst
  
  cat("The R-square is:",rsq,"\n") 
  return(rsq)
}



#------------------------------------------#
rep_test2 <- function(data){
  y_predicted=vector()
  yt=vector()
  
  for (x in 1:150)  {
    results=pca_lasso_test2(i=x,data)
    y_test = results[2]
    yp=results[1]
    y_predicted = append(y_predicted,yp)
    yt=append(yt,y_test)
  }
  rsquare_test(yt,y_predicted = y_predicted)
  
}

result=rep_test2(data_4)



