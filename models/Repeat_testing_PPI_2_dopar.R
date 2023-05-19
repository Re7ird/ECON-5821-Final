#Repeated testing

#Perequisite
library(tidyverse)
library(glmnet)
library(doParallel)
registerDoParallel(60)
set.seed(10)
n=5


#load(url("https://github.com/zhentaoshi/Econ5821/raw/main/data_example/dataset_inf.Rdata"))
load("dataset_inf.Rdata")

#-----------------Start-----------------#
st=Sys.time()

#Lag=1:
data_1=merge(ppi,X,by="month",all = T)
data_1=data_1 |> mutate(y=lead(PPI)) |> filter(is.na(y)==F)

#Lag=2:
data_2=merge(ppi,X,by="month",all = T)
data_2=data_2 |> mutate(y=lead(PPI,n=2)) |> filter(is.na(y)==F)

#Lag=3:
data_3=merge(ppi,X,by="month",all = T)
data_3=data_3 |> mutate(y=lead(PPI,n=3)) |> filter(is.na(y)==F)

#Train the PCA model
pca_data=X |> select(!c("month"))
PCA=princomp(pca_data)

##------------------------------#Functions:---------------------------##

pca_lasso_test <- function(i,data) {
  j=i+11
  k=i+12
  y_train=data[i:j,"y"]
  y_test=data[k,"y"]
  x_train=data[i:j,] |> select(!c("month","y","PPI"))
  x_test=data[k,] |> select(!c("month","y","PPI"))
  
  
  #collect 8 Principal Components and train the lasso regression.
  PC=predict(PCA,newdata = x_train)[,1:dim]
  
  #Calculate scores for testin
  PC_test=predict(PCA,newdata = x_test)[,1:dim]
  
  #find lambda
  cv_model <- cv.glmnet(PC, y_train, alpha = 1)
  best_lambda <- cv_model$lambda.min
  
  #display optimal lambda value
  best_lambda
  
  #view plot of test MSE's vs. lambda values
  
  #view coefficients of best model
  pca_lasso <- glmnet(PC, y_train, alpha = 1, lambda =best_lambda)
  coef(pca_lasso)
  summary(pca_lasso)
  
  #make a prediction for the response value of a new observation
  yp=predict(pca_lasso, s = best_lambda, newx = PC_test)[1,1]
  return(c(yp,y_test))
  
}

pca_lasso_test2 <- function(i,data) {
  j=i+11
  k=i+12
  y_train=data[i:j,"y"]
  y_test=data[k,"y"]
  x_train=data[i:j,] |> select(!c("month","y","PPI"))
  x_train_ppi=data[i:j,"PPI"] 
  x_test=data[k,] |> select(!c("month","y","PPI"))
  x_test_ppi=data[k,"PPI"]
  
  
  #collect 8 Principal Components and train the lasso regression.
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
  
  
  ybind <-foreach (x = 1:150,.combine = "rbind") %dopar% {
    results=pca_lasso_test(i = x,data)
    y_test = results[2]
    yp=results[1]
    return(c(yp,y_test))
  }
  yp=ybind[,1]
  yt=ybind[,2]
  
  rsquare_test(yt,y_predicted = yp)
  
}

rep_test2 <- function(data){
  ybind2 <-foreach (x = 1:150,.combine = "rbind") %dopar% {
    results=pca_lasso_test2(i = x,data)
    y_test = results[2]
    yp=results[1]
    return(c(yp,y_test))
  }
  yp=ybind2[,1]
  yt=ybind2[,2]
  
  rsquare_test(yt,y_predicted = yp)
  
}


#---------------------Run test---------------------#

#------------------test 1----------------#
runtest <- function(){
    rsqv=c()
    for (t in 1:n) {

      rsqv=append(rsqv,rep_test1(data_1))

    }
    rppi_1_1 <- rsqv

    #------------------test 2----------------#
    #Lag=2,method=1

    rsqv=c()
    for (t in 1:n){

      rsqv=append(rsqv,rep_test1(data_2))

    }
    rppi_1_2 <- rsqv



    #------------------test 3----------------#
    #Lag=3,method=1

    rsqv=c()
    for (t in 1:n){

      rsqv=append(rsqv,rep_test1(data_3))

    }
    rppi_1_3 <- rsqv

    #------------------test 4----------------#
    #Lag=1,method=2

    rsqv=c()
    for (t in 1:n){

      rsqv=append(rsqv,rep_test2(data_1))

    }

    rppi_2_1 <- rsqv

    #------------------test 5----------------#
    #Lag=2,method=2

    rsqv=c()
    for (t in 1:n){

      rsqv=append(rsqv,rep_test2(data_2))

    }
    rppi_2_2 <- rsqv



    #------------------test 6----------------#
    #Lag=3,method=2

    rsqv=c()
    for (t in 1:n){

      rsqv=append(rsqv,rep_test2(data_3))

    }
    rppi_2_3 <- rsqv
    
    results <- matrix(data=c(mean(rppi_1_1),mean(rppi_1_2),mean(rppi_1_3),
                             mean(rppi_2_1),mean(rppi_2_2),mean(rppi_2_3),
                             sd(rppi_1_1),sd(rppi_1_2),sd(rppi_1_3),
                             sd(rppi_2_1),sd(rppi_2_2),sd(rppi_2_3)),
                      nrow = 2,ncol = 6,byrow = T)

    print("Results:")
    cat("mean: \t",mean(rppi_1_1),"\t",mean(rppi_1_2),"\t",mean(rppi_1_3),"\t",mean(rppi_2_1),
        "\t",mean(rppi_2_2),"\t",mean(rppi_2_3),"\n")
    cat("stdev: \t",sd(rppi_1_1),"\t",sd(rppi_1_2),"\t",sd(rppi_1_3),"\t",sd(rppi_2_1),"\t",sd(rppi_2_2),
        "\t",sd(rppi_2_3),"\n")
    
    return(results)
}    



##-----------------------------------------------------##
dim=8
res_1 <-runtest()[1,]
dim=9
res_2 <-runtest()[1,]
dim=10
res_3 <-runtest()[1,]

res=rbind(res_1,res_2,res_3)
res2=t(res)


pl=data.frame(res2)
index=1:6
pl=cbind(pl,index)

title=paste("PPI Model 2, window size =",12)
ggplot(data = pl)+
  ggtitle(label = title)+
  geom_line(color = "green", mapping=aes(y=res_1,x=index))+
  geom_line(color = "red", mapping=aes(y=res_2,x=index))+
  geom_line(color = "blue", mapping=aes(y=res_3,x=index))

ggsave(filename = "ResultsPPI_2.png")

ed=Sys.time()
dr=ed-st

cat("Total duration: ")
dr
