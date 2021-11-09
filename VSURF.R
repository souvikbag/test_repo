install.packages("VSURF")
library(VSURF)
library(tidyverse)
library(caret)
install.packages("pacman")
library(pacman)
pacman::p_load(ggplot2, tidyverse, caret, VSURF)





vsurf_simulation <- function(n,m)
{  

 
  X = matrix(rnorm(n*m,0,1),nrow = n,ncol = m)
  ####X should be n*(m+1) matrix
  
  #####there are 100 regressors, so generate beta values such that 
  #first 10 beta valuesare high and last 90 beta values are very low
  beta <- numeric(m)
  beta[1:5] = c(5,7,7,10,6)
  
  
  ####we get the value of log(p/(1-p)) and errors are taken normal
  logitp = X%*%beta + rnorm(n)
  ######by some manipulation we get the value of p(y=1)
  p = exp(logitp)/(1 +exp(logitp) )
  #Using this value of p, we get the binary response variable
  Y <- rbinom(n,1,p)
  
  #####merge X and Y to create one dataframe
  sim_data <- as.data.frame(cbind(Y,X))
  #logit_model <- glm(Y~.,data=sim_data, family = binomial)
  ###alorithm did not converge
  names(sim_data)[-1] <- paste0('x', 1:(ncol(sim_data)-1))
  ## Train Test split
  training.sample <- sim_data$Y %>% createDataPartition(p = 0.8 , list = FALSE) 
  
  train.data <- sim_data[training.sample,]
  test.data <- sim_data[-training.sample,]
  
  #######Now we fit lasso
  Y.train <- train.data$Y
  Y.test <- test.data$Y
  Y.train <- as.factor(Y.train)
  #model matrix
  # Intercept term must not be included
  X.train <- (model.matrix(Y.train~.,data = train.data))[,-c(1,2)]
  X.test <- model.matrix(Y.test~.,data = test.data)[,-c(1,2)]
  # Fit Sure Variable selecting using random forest
  X.train = data.matrix(X.train)
  vsurf <- VSURF(X.train,Y.train, ncores = 7,verbose = FALSE ,parallel = TRUE)  
 prediction =  predict(vsurf,X.test, step = c("interp", "pred"))
 pred1<-prediction$pred
 cm <- table(Predicted = pred1, Actual = Y.test)
 
 miscla <- 1 - (sum(diag(cm))/sum(cm))
 precision <- cm[1,1]/(cm[1,1]+cm[1,2])
 recall <- cm[1,1]/(cm[1,1]+cm[2,1])
 # Time taken
 time.taken = vsurf$overall.time
 
 #selected
 selected <- length(vsurf$varselect.pred)
 imp = as.matrix(vsurf$varselect.pred)
 selected1 <- sum(ifelse(vsurf$varselect.pred <=5,1,0))
 
 
 
 
 ##error frame
 errors <- data.frame(matrix(ncol = 6, nrow = 1))
 colnames(errors) <- c("selected","selected_Imp","Misclassification_Rate", "Precision", "Recall", "Time")
 errors$selected <- selected
 errors$selected_Imp <- selected1
 #errors$MSE <- round(mse_beta,2)
 #errors$MAE <- round(mae_beta,2)
 errors$Misclassification_Rate <- miscla
 errors$Precision <- round(precision,2)
 errors$Recall <- round(recall,2)
 errors$Time <- time.taken
 
 return(errors)
} 


vsurf_simulation(100,10) 

library(parallel)
detectCores()
