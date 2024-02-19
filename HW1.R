#install.packages("fredr")

library(dplyr)
library(stats)

beta = 0.21
T = 840
rho = 0.972
sigma_u = 30.05*10^4
sigma_v = 0.108*10^4
sigma_uv = -1.621*10^4

Sigma <- matrix(c(sigma_u, sigma_uv, sigma_uv, sigma_v),2,2)

library(MASS)
set.seed(seed = 1232020)

S = 1000
model_beta <- 0

uv <- data.frame(mvrnorm(n=S*T,
                            mu=c(0,0),
                            Sigma=Sigma))

for (s in 1:S){

  x_t <- 0 + uv[(s-1)*T+1,2]
  y_t <- 0 + uv[(s-1)*T+1,1]
  
  for(i in 2:T){
    x_t[i] <- rho*x_t[i-1]+uv[(s-1)*T+i,2]
    y_t[i] <- beta*x_t[i-1]+uv[(s-1)*T+i,1]
  }
  
  data <- data.frame(y_t,x_t)
  
  model <- lm(y_t ~ lag(x_t), data=data)
  model_beta[s] <- model$coefficients[2]
  
  rm(x_t, y_t, data)

}



