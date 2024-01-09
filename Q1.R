################################################################################
#                       Question 1: Short Rate Model Calibration
################################################################################


library(readxl)
#install.packages('vasicek')
library(vasicek)
#install.packages('SMFI5')
library('SMFI5')
#install.packages('rpgm')
#library('rpgm')


#### Initialising data ####


P_market <- read_excel("Data/Euribor Curve UCL Update.xlsm")
P_market <- P_market[7:56,11]
names(P_market) <- "DF"

View(P_market)

Dates = seq(from = as.Date("2023-05-15"), to = as.Date("2072-05-15"), 
            by = 'year')
data = data.frame(Dates, P_market)
P_market <- cbind(Dates, P_market)
Mat <- seq(1,50,1)
P_market <-cbind(P_market, Mat)
View(P_market)
plot(P_market$DF, type= 'l')



##################### 1.1

#### Parameters  #### 

#We initialise the parameters


r0 <- 0.02


kappa <- 0.1


theta <- 0.03


eta <- 0.005



Int <- cbind(r0, kappa, theta, eta)



#### F function  #### 

#We create the fonction that we will optimise to find the right parameters

F_function <-function(Int){
  
  B <- array(dim = c(50))
  A <- array(dim = c(50))
  P <- array(dim = c(50))
  f <-array(dim = c(50))
  for(t in 1:nrow(P_market)){
    B[t] <- (1-exp(-Int[2]*t))/Int[2]
    A[t] <- exp((Int[3]-(Int[4]^2)/(2*Int[2]^2))*(B[t]-t)-(Int[4]^2)/(4*Int[2])*(B[t]^2))
    P[t]<- A[t]*exp(-Int[1]*B[t])
    f[t] <- (P[t]-P_market$DF[t])^2
  }
  P  
  A
  B
  f
  Q <- sum(f)
  Q
  return(Q)
}






#Optimisation method
Int_Opti2 <- optim(Int, F_function, method = "BFGS") #By default optim performs a minimization and uses the neldon-mead method
#we prefer using an other method
Int_Opti2$par 




#### P function  #### 

#returns the value of P at each time t
P_function<- function(Int){
  
  B <- 0
  A <- 0
  P <- 0
  for(t in 1:nrow(P_market)){
    B[t] <- (1-exp(-Int[2]*t))/Int[2]
    A[t] <- exp((Int[3]-Int[4]^2/(2*Int[2]^2))*(B[t]-t)-(Int[4]^2)/(4*Int[2])*(B[t])^2)
    P[t]<- A[t]*exp(-Int[1]*B[t])
  }
  return(P)
}

P_model <- P_function(Int_Opti2$par)

plot(P_market$DF, type="l", main = "Fitting Check", xlab = "Maturity", ylab = "Discount facore/Price")
lines(P_model, col="green")

#We see that the model fittes the curve in a good way




##################### 1.2

View(P_market)
View(P_model)
fit <- -P_market$DF/P_model
P_model_perffit <- -fit*P_model
plot(P_model_perffit, col = "green", type = 'l',main = "Estimated Discount Curve", xlab = "Maturity", ylab = "Discount facore/Price",)

plot(P_market$DF, type="l", main = "Fitting Check", xlab = "Maturity", ylab = "Discount facore/Price", col = 'black')
lines(P_model_perffit, col="green")

plot(P_model_perffit, col = "green", type = 'l',main = "Comparison with previous estimation", xlab = "Maturity", ylab = "Discount facore/Price",)
lines(P_model, col="red")

#Here the curves are perfectly fitted
