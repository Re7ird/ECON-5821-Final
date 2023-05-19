#Final model for PPI:
#The best model for PPI:"Lag=1, with PPI as independent variable, dim = 8, window size = 60"

#Perequisite
library(tidyverse)
library(glmnet)
library(doParallel)
registerDoParallel(detectCores()-1)
set.seed(10)

dim=8
windowlength=84

load("data/dataset_inf.Rdata")
load("data/data_oos.Rdata")


#Lag=1 data:
data_1=rbind(X,real.X)
data_1=merge(rbind(ppi,real.ppi),data_1,by="month",all = T)
data_1=data_1 |> mutate(y=lead(PPI))

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
  x_train=data[i:j,] |> select(!c("month","y","PPI"))
  x_train_ppi=data[i:j,"PPI"] 
  x_test=data[k,] |> select(!c("month","y","PPI"))
  x_test_ppi=data[k,"PPI"]
  
  
  #collect Principal Components and train the lasso regression.
  PC=predict(PCA,newdata = x_train)[,1:dim]
  PCs=cbind(PC,x_train_ppi)
  
  #Calculate scores for testing
  PC_test=predict(PCA,newdata = x_test)[,1:dim]
  PC_test2=append(PC_test,x_test_ppi)
  
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
  
  result <-foreach (x = 1:113,.combine = "rbind") %dopar% {
    pca_lasso_test2(i = x,data)
  }
  result=data.frame(result)
  colnames(result) <- c("month","PPI_predicted","PPI_true")
  return(result)
  
}

result <- rep_test2(data_1)
yp=result[result$month %in% 169:198,]$PPI_predicted
yt=result[result$month %in% 169:198,]$PPI_true

#R-Square of price prediction
rsquare_test(yt,yp)

result=result |> mutate(inflation_predicted=log(PPI_predicted)-log(lag(PPI_true,n=12)),
                        inflation_true=log(PPI_true)-log(lag(PPI_true,n=12))
                        )
result2=result |> filter(is.na(inflation_predicted)==F)


inf_p=result2[result2$month %in% 169:198,]$inflation_predicted
inf_t=result2[result2$month %in% 169:198,]$inflation_true

#R-Square of inflation
rsquare_test(inf_t,inf_p)

###Result2 is the final answer
result2




