#install required packages

install.packages("SIS")
install.packages("glmnet")
install.packages("ncvreg")

#load required packages

library(SIS)
library(ncvreg)
library(glmnet)

sis_simulation <- function(n,m)
{
  X_pre = matrix(rnorm(n*m,0,1),nrow = n,ncol = m)
  ####X should be n*(m+1) matrix
  X <- cbind(1,X_pre)
  #####there are 100 regressors, so generate beta values such that 
  #first 10 beta valuesare high and last 90 beta values are very low
  beta <- numeric(m+1)
  beta[1:((m/10) + 1)] = runif(((m/10) +1),1,10)
  
  
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
  
  ## Train Test split
  training.sample <- sim_data$Y %>% createDataPartition(p = 0.8 , list = FALSE) 
  
  train.data <- sim_data[training.sample,]
  test.data <- sim_data[-training.sample,]
  
  #######Now we fit lasso
  Y.train <- train.data$Y
  Y.test <- test.data$Y
  #model matrix
  # Intercept term must not be included
  X.train <- (model.matrix(Y.train~.,data = train.data))[,-c(1,2,3)]
  X.test <- model.matrix(Y.test~.,data = test.data)[,-c(1,2,3)]
 # Fit Sure Independence Screening
  X.train = data.matrix(X.train)
  sis_model <- SIS(X.train,Y.train,family = "binomial", penalty = 'SCAD', varISIS='cons',)
  ## Selecting Important variables
  sis_model
  
  W <- as.matrix(sis_model$coef.est)
  keep_X <- rownames(W)[W!=0]
  keep_X <- keep_X[!keep_X == "(Intercept)"]
  ####Selected variables
  selected <- length(keep_X)
  ##select variables from weighted variables
  selected1 <- sum(ifelse(as.numeric(substr(keep_X,2,5)) < (m/10 + 1),1,0))
  
  
  betahat <- coef(best_alasso)[2:(m+1)]
  mse_beta <- (sum((betahat - beta[-1])^2))/n
  mae_beta <- sum(abs(betahat - beta[-1]))/n
  
  ## Prediction and performance of model with test data
  
  prediction <- predict(sis_model$fit, X.test, lambda = sis_model$lambda, type = 'response')
  
  pred1<- ifelse(prediction > 0.5 , 1, 0)
  cm <- table(Predicted = pred1, Actual = Y.test)
  
  miscla <- 1 - (sum(diag(cm))/sum(cm))
  precision <- cm[1,1]/(cm[1,1]+cm[1,2])
  recall <- cm[1,1]/(cm[1,1]+cm[2,1])
  ##error frame
  errors <- data.frame(matrix(ncol = 8, nrow = 1))
  colnames(errors) <- c("selected","selected1","Important","MSE","MAE","Misclassification_Rate", "Precision", "Recall")
  errors$selected <- selected
  errors$selected1 <- selected1
  errors$MSE <- round(mse_beta,2)
  errors$MAE <- round(mae_beta,2)
  errors$Misclassification_Rate <- miscla
  errors$Precision <- round(precision,2)
  errors$Recall <- round(recall,2)
  errors$Important <- (m/10)
  
  return(errors)
}

sis_simulation(200,50)
enet_simulation(200,50)
lasso_simulation(200,50)
alasso_simulation(200,50)




