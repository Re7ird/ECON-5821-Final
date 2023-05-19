#Final model for CPI:
#The best model for CPI:"Lag=1, with CPI as independent variable, dim = 8, window size = 60"

#Perequisite
library(tidyverse)
library(glmnet)
library(doParallel)
registerDoParallel(detectCores()-1)
set.seed(10)


windowlength=60
dim=8

load("data/dataset_inf.Rdata")
load("data/data_oos.Rdata")



#Lag=1 data:
data_1=rbind(X,real.X)
data_1=merge(rbind(cpi,real.cpi),data_1,by="month",all = T)
data_1=data_1 |> mutate(y=lead(CPI))

#Train the PCA model
pca_data=X |> select(!c("month"))
PCA=princomp(pca_data)

##------------------------------#Functions:---------------------------##

pca_lasso_test2 <- function(i,data) {
  a=windowlength-1
  b=windowlength
  
  j=i+a
  k=i+b
  y_train=data[i:j,"y"]
  y_test=data[k,"y"]
  x_train=data[i:j,] |> select(!c("month","y","CPI"))
  x_train_cpi=data[i:j,"CPI"] 
  x_test=data[k,] |> select(!c("month","y","CPI"))
  x_test_cpi=data[k,"CPI"]
  
  
  #collect Principal Components and train the lasso regression.
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
  
  result <-foreach (x = 1:(197-windowlength),.combine = "rbind") %dopar% {
    pca_lasso_test2(i = x,data)
  }
  result=data.frame(result)
  colnames(result) <- c("month","CPI_predicted","CPI_true")
  return(result)
  
}

#### This is the final prediction of CPI
result <- rep_test2(data_1)

## Test the prediction out of sample R Square


yp=result[result$month %in% 1:198,]$CPI_predicted
yt=result[result$month %in% 1:198,]$CPI_true

#R-Square of price prediction
rsquare_test(yt,yp)

## calculate inflation rate

result=result |> mutate(inflation_predicted=log(CPI_predicted)-log(lag(CPI_true,n=12)),
                        inflation_true=log(CPI_true)-log(lag(CPI_true,n=12))
                        )
result2=result |> filter(is.na(inflation_predicted)==F)

inf_p=result2[result2$month %in% 13:198,]$inflation_predicted
inf_t=result2[result2$month %in% 13:198,]$inflation_true

#R-Square of inflation
rsquare_test(inf_t,inf_p)

###Result2 is the final answer
result2


