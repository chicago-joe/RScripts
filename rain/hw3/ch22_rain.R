library(MASS)
t<-c(1,5,15,20)
Yields<-c(0.49,0.3856,0.3537,0.2863)
B<-c(66.711,19.580,10.645,6.504)
risk<-c(0.0013,0.0011,0.0010,0.0009)
i=1
min.RSS <- function(par) {
  ((1-par[1])^Years[i]+(1-par[2])*(1-(1-par[1])^Years[i]))-(1+risk[i])/((1+Yields[i])^Years[i])}
result <- optim(par, fn = min.RSS,lower=c(0, 0), upper=c(1,1),method = "L-BFGS-B")
result$par
result$value

par<-c(0.1,0.9)
((par[1]^Years[i]+par[2]*(1-par[1]^Years[i])))*1000-Price[i]*(1+Yields[i])^Years[i]