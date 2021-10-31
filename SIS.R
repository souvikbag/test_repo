#install required packages

install.packages("SIS")
install.packages("glmnet")
install.packages("ncvreg")

#load required packages

library(SIS)
library(ncvreg)
library(glmnet)

#sis_simulation

sis_simulation <- function(n,m)
{
  X_pre = matrix(rnorm(n*m,0,1),nrow = n,ncol = m)
  ####X should be n*(m+1) matrix
  X <- cbind(1,X_pre)
  #####there are 100 regressors, so generate beta values such that 
  #first 10 beta valuesare high and last 90 beta values are very low
  beta <- numeric(m+1)
  beta[1:((m/10) + 1)] = runif(((m/10) +1),5,10)
  
  
  ####we get the value of log(p/(1-p)) and errors are taken normal
  logitp = X%*%beta + rnorm(n)
  ######by some manipulation we get the value of p(y=1)
  p = exp(logitp)/(1 +exp(logitp) )
  #Using this value of p, we get the binary response variable
  Y <- rbinom(n,1,p)
  
  #####merge X and Y to create one dataframe
  sim_data <- as.data.frame(cbind(X_pre,Y))
  #logit_model <- glm(Y~.,data=sim_data, family = binomial)
  ###alorithm did not converge
  #######Now we fit lasso
  Y <- sim_data$Y
  #model matrix
  X <- model.matrix(Y~.,data = sim_data)[,-1]
  
  #fit sis
  
sis_fit <- SIS(X, Y, family = "binomial", penalty = "SCAD", tune = "cv", perm = TRUE)

## Selecting Important variables
W <- as.matrix(sis_fit$coef.est)
keep_X <- rownames(W)[W!=0]
keep_X <- keep_X[!keep_X == "(Intercept)"]
####Selected variables
selected <- length(keep_X)
##select variables from weighted variables
selected1 <- sum(ifelse(as.numeric(substr(keep_X,2,5)) < (m/10 + 1),1,0))


betahat <- sis_fit$coef.est[2:(m+1)]
mse_beta <- (sum((sis_fit$coef.est - beta[-1])^2))/n
mae_beta <- sum(abs(sis_fit$coef.est - beta[-1]))/n

##error frame
errors <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(errors) <- c("selected","selected1","MSE","MAE","Important")
errors$selected <- selected
errors$selected1 <- selected1
errors$Important <- (m/10)
errors$MSE <- round(mse_beta,2)
errors$MAE <- round(mae_beta,2)
return(list(errors,sis_fit$coef.est))

}

sis_simulation(400,100)
sis_simulation(100,400)   





