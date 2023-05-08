#This is the thrid model for CPI.
#In this model I use all the data to train the global best lambda of Lasso Regression.
#The result is really bad.

#Perequisite
library(tidyverse)
library(glmnet)
set.seed(10)
n=1


load(url("https://github.com/zhentaoshi/Econ5821/raw/main/data_example/dataset_inf.Rdata"))


#-----------------Start-----------------#
st=Sys.time()

#Lag=1:
data_1=merge(cpi,X,by="month",all = T)
data_1=data_1 |> mutate(y=lead(CPI))  |> filter(is.na(y)==F)

#Lag=2:
data_2=merge(cpi,X,by="month",all = T)
data_2=data_2 |> mutate(y=lead(CPI,n=2)) |> filter(is.na(y)==F)

#Lag=3:
data_3=merge(cpi,X,by="month",all = T)
data_3=data_3 |> mutate(y=lead(CPI,n=3)) |> filter(is.na(y)==F)

#Train the PCA model
pca_data=X |> select(!c("month"))
PCA=princomp(pca_data)

##------------------------------#Functions:---------------------------##

est_lambda1=function(data){
  
  pca_data=data |> select(!c("month","y","CPI"))
  PCA=princomp(pca_data)
  lasso_x=PCA$scores[,1:dim]
  lasso_y=data[,"y"]
  cv_model <- cv.glmnet(lasso_x, lasso_y, alpha = 1)
  best_lambda1 <- cv_model$lambda.min
  return(best_lambda1)
}

est_lambda2=function(data){
  
  pca_data=data |> select(!c("month","y","CPI"))
  PCA=princomp(pca_data)
  lasso_x=cbind(PCA$scores[,1:dim],data$CPI)
  lasso_y=data[,"y"]
  cv_model <- cv.glmnet(lasso_x, lasso_y, alpha = 1)
  best_lambda2 <- cv_model$lambda.min
  return(best_lambda2)
}


pca_lasso_test <- function(i,data,best_lambda) {
  j=i+11
  k=i+12
  y_train=data[i:j,"y"]
  y_test=data[k,"y"]
  x_train=data[i:j,] |> select(!c("month","y","CPI"))
  x_test=data[k,] |> select(!c("month","y","CPI"))
  
  
  #collect 8 Principal Components and train the lasso regression.
  PC=predict(PCA,newdata = x_train)[,1:dim]
  
  #Calculate scores for testin
  PC_test=predict(PCA,newdata = x_test)[,1:dim]
  
  #view coefficients of best model
  pca_lasso <- glmnet(PC, y_train, alpha = 1, lambda =best_lambda)
  coef(pca_lasso)
  summary(pca_lasso)
  
  #make a prediction for the response value of a new observation
  yp=predict(pca_lasso, s = best_lambda, newx = PC_test)[1,1]
  return(c(yp,y_test))
  
}

pca_lasso_test2 <- function(i,data,best_lambda) {
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


#------------------------------
rep_test1 <- function(data){
  y_predicted=vector()
  yt=vector()
  best_lambda=est_lambda1(data)
  
  for (x in 1:150) {
    results=pca_lasso_test(i=x,data,best_lambda)
    y_test = results[2]
    yp=results[1]
    y_predicted = append(y_predicted,yp)
    yt=append(yt,y_test)
  }
  rsquare_test(yt,y_predicted = y_predicted)
  
}

rep_test2 <- function(data){
  y_predicted=vector()
  yt=vector()
  best_lambda=est_lambda2(data)
  
  for (x in 1:150) {
    results=pca_lasso_test2(i=x,data,best_lambda)
    y_test = results[2]
    yp=results[1]
    y_predicted = append(y_predicted,yp)
    yt=append(yt,y_test)
  }
  rsquare_test(yt,y_predicted = y_predicted)
  
}


#---------------------Run test---------------------#

#------------------test 1----------------#
runtest <- function(){
  
    rcpi_1_1 <- rep_test1(data_1)

    #------------------test 2----------------#
    #Lag=2,method=1

    
    rcpi_1_2 <- rep_test1(data_2)



    #------------------test 3----------------#
    #Lag=3,method=1

    
    rcpi_1_3 <- rep_test1(data_3)

    #------------------test 4----------------#
    #Lag=1,method=2
    
    rcpi_2_1 <- rep_test2(data_1)

    #------------------test 5----------------#
    #Lag=2,method=2

    rcpi_2_2 <-rep_test2(data_2)



    #------------------test 6----------------#
    #Lag=3,method=2

    rcpi_2_3 <-rep_test2(data_3)
    
    
    results <- c(rcpi_1_1,rcpi_1_2,rcpi_1_3,rcpi_2_1,rcpi_2_2,rcpi_2_3)
    
    return(results)
}    



##-----------------------------------------------------##
dim=8
res_1 <-runtest()
dim=9
res_2 <-runtest()
dim=10
res_3 <-runtest()

res=rbind(res_1,res_2,res_3)
res2=t(res)
par(new = TRUE)
plot(res2[,1],type = "l")
plot(res2[,2],type = "l")
plot(res2[,3],type = "l")

pl=data.frame(res2)
index=1:6
pl=cbind(pl,index)

ggplot(data = pl)+
  geom_line(color = "green", mapping=aes(y=res_1,x=index))+
  geom_line(color = "red", mapping=aes(y=res_2,x=index))+
  geom_line(color = "blue", mapping=aes(y=res_3,x=index))

ggsave(filename = "ResultsCPI_3.png")

ed=Sys.time()
dr=ed-st

cat("Total duration: ")
dr
