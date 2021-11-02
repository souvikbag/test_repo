# SCAD - Smoothly Clipped Absolute Devition

# import important libraries
library(tidyverse)
library(caret)
library(ncvreg)


scad_simulation <- function(n,m)
  
{
  
  start.time <- Sys.time()

X_pre = matrix(rnorm(n*m,0,1),nrow = n,ncol = m)
# Include the Intercept
X <- cbind(1,X_pre)
#####there are 100 regressors, so generate beta values such that 
#first 10 beta valuesare high and last 90 beta values are very low
beta <- numeric(m+1)
beta[1:(m/10)+1] = runif((m/10),1,10)

####we get the value of log(p/(1-p)) and errors are taken normal
logitp = X%*%beta + rnorm(n)
######by some manipulation we get the value of p(y=1)
p = exp(logitp)/(1 +exp(logitp) )
#Using this value of p, we get the binary response variable
Y <- rbinom(n,1,p)

#####merge X and Y to create one dataframe
sim_data <- as.data.frame(cbind(Y,X))

## Train Test split
training.sample <- sim_data$Y %>% createDataPartition(p = 0.8 , list = FALSE) 

train.data <- sim_data[training.sample,]
test.data <- sim_data[-training.sample,]

#######Now we fit lasso
Y.train <- train.data$Y
Y.test <- test.data$Y
#model matrix
X.train <- (model.matrix(Y.train~.,data = train.data))[,-c(1,2,3)]
X.test <- model.matrix(Y.test~.,data = test.data)[,-c(1,2,3)]
#minimum value of lambda 
cv.scad <- cv.ncvreg(X.train, Y.train, family = "binomial", penalty = "SCAD")
best_lambda=cv.scad$lambda.min

# Fit the  model on the training data
scad_model <- ncvreg(X.train, Y.train, family = "binomial",
                      lambda = best_lambda, penalty = "SCAD")

##########Important variables from Lasso#######
W <- as.matrix(scad_model$beta)
keep_X <- rownames(W)[W!=0]
keep_X <- keep_X[!keep_X == "(Intercept)"]

####Selected variables
selected <- length(keep_X)
##select variables from weighted variables
selected1 <- sum(ifelse(as.numeric(substr(keep_X,2,5)) < (m/10 + 3),1,0))

betahat <- scad_model$beta[2:(m+1)]
mse_beta <- (sum((betahat - beta[-1])^2))/n
mae_beta <- sum(abs(betahat - beta[-1]))/n

## Prediction and performance of model with test data

prediction <- scad_model %>% predict(X.test, type = 'response')

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

end.time <- Sys.time()

time.taken <- round((end.time - start.time),2)

return(list(errors,time.taken))

}

scad_simulation(200,50)
