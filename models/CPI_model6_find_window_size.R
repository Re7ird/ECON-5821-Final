#This model modified the window length of lasso estimation.

#Perequisite
library(tidyverse)
library(glmnet)
library(doParallel)
registerDoParallel(detectCores()-1)
set.seed(10)

n=5


#load(url("https://github.com/zhentaoshi/Econ5821/raw/main/data_example/dataset_inf.Rdata"))


#-----------------Start-----------------#
st=Sys.time()

#Lag=1:
data_1=merge(cpi,X,by="month",all = T)
data_1=data_1 |> mutate(y=lead(CPI)) |> filter(is.na(y)==F)

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



pca_lasso_test2 <- function(i,data,wdl) {
  windowlength=wdl
  a=windowlength-1
  b=windowlength
  z = 160-windowlength
  
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
  return(c(yp,y_test))
  
}


rsquare_test <- function(yt,y_predicted) {
  sst <- sum((yt - mean(yt))^2)
  sse <- sum((y_predicted - yt)^2)
  
  rsq <- 1 - sse/sst
  
  cat("The R-square is:",rsq,"\n") 
  return(rsq)
}



rep_test2 <- function(data,wdl){
  z = 160-wdl
  ybind2 <-foreach (x = 1:z,.combine = "rbind") %dopar% {
    results=pca_lasso_test2(i = x,data,wdl)
    y_test = results[2]
    yp=results[1]
    return(c(yp,y_test))
  }
  yp=ybind2[,1]
  yt=ybind2[,2]
  
  rsquare_test(yt,y_predicted = yp)
  
}



#---------------------Run test---------------------#
runtest <- function(wdl){
  windowlength=wdl


    #------------------test 4----------------#
    #Lag=1,method=2

    rsqv=c()
    for (t in 1:n){

      rsqv=append(rsqv,rep_test2(data_1,windowlength))

    }
    rcpi <- rsqv

    
    results <- c(windowlength,mean(rcpi))

    print("Results:")
    cat("mean: \t",mean(rcpi),"\n")
    cat("stdev: \t",sd(rcpi),"\n")
    
    return(results)
} 



##-----------------------------------------------------##


dim=8
res <-  foreach (l = c(12,24,48,60,72,96,120),.combine="cbind") %do%{
  runtest(wdl = l)
}

rownames(res)<-c("window_size","R_Square")
res[1,] <- round(res[1,])
view(res)


pl=data.frame(t(res))
ggplot(data = pl)+
  ggtitle("CPI: Window Size")+
  geom_point(aes(x=window_size,y=R_Square))

ggsave(filename = "ResultsCPI_6_find_window_size.png")

print("The best model for CPI:")
print("Lag=1, with CPI, dim = 8, window size = 60")
best_rsq=res[2,res[1,]==60]
cat("Best R-Square = ",best_rsq,"\n")



ed=Sys.time()
dr=ed-st

cat("Total duration: ")
print(dr)
