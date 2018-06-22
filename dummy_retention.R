rm(list = ls())
eval_f <- function(params,t){
  Cinit <- params[1]
  
  Cnat <- params[2]
  
  k <- params[3]
  
  c_fit <- Cnat + (Cinit - Cnat)*exp(-k*t)
  r_fit <- rep(1, length(c_fit))
  for (i in 2:length(r_fit)){
    r_fit[i] <- r_fit[i-1]*(1 - c_fit[i-1])
  }
  return(r_fit)
}


t <- 1:50
param1 <- c(0.65, 0.11, 1.6)
r1 <- eval_f(param1,t)
plot(r1, type = 'l')
param2 <- c(0.59, 0.1, 1.5)
r2 <- eval_f(param2,t)
lines(r2)
param3 <- c(0.57, 0.09, 1.6)
r3 <- eval_f(param3,t)
lines(r3)
param4 <- c(0.55, 0.08, 1.4)
r4 <- eval_f(param4,t)
lines(r4)
param5 <- c(0.49, 0.03, 1.7)
r5 <- eval_f(param5,t)
lines(r5)


param1 <- c(0.65, 0.11, 1)
r1 <- eval_f(param1,t)
plot(r1, type = 'l')
param2 <- c(0.65, 0.11, 2)
r2 <- eval_f(param2,t)
lines(r2)
param3 <- c(0.57, 0.07, 0.6)
r3 <- eval_f(param3,t)
lines(r3)
param4 <- c(0.55, 0.06, 0.6)
r4 <- eval_f(param4,t)
lines(r4)
param5 <- c(0.49, 0.03, 0.5)
r5 <- eval_f(param5,t)
lines(r5)



r_fit_eval <- function(params,t){
  Cinit <- params[1]

  Cnat <- params[2]

  k <- params[3]

  c_fit <- Cnat + (Cinit - Cnat)*exp(-k*t)
  r_fit <- rep(1, length(c_fit))
  for (i in 2:length(r_fit)){
    r_fit[i] <- r_fit[i-1]*(1 - c_fit[i-1])
  }
  r_fit <- r_fit + runif(length(r_fit),min = -1,max = 1) * 0.05*r_fit
  r_fit[1] = 1
  return(r_fit)
}
params <- c(0.4,0.02,0.3)
t <- 1:166
r_fit_eval(params,t)
