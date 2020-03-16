library(xts)
library(quantmod)
library(Quandl)
library(rugarch)

Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

# For industrial production, compute log-returns (% changes)
indprod <- Quandl("FRED/INDPRO", type="xts")
colnames(indprod) <- c("INDPROD")
indprod.logret <- diff(log(indprod))

exinfl <- Quandl("FRBC/EXIN", type="xts")[,1]
exinfl.diff <- diff(exinfl)
colnames(exinfl.diff) <- c("EXINFL.diff")

cpi <- Quandl("FRBC/USINFL", type="xts")[,1]
colnames(cpi) <- c("CPI")

excpi <- cpi*(1+exinfl)  # expected CPI in twelve months
cpi.surprise <- log(cpi) - log(lag(excpi, 12))  # % CPI surprise
colnames(cpi.surprise) <- c("INFLSURP")

t10y <- Quandl("FRED/DGS10", type="xts")/100
colnames(t10y) <- c("T10Y")

baa <- Quandl("FRED/INDPRO", type="xts")[,1]
colnames(baa) <- c("BAA")

baa.t10y<-baa-t10y
colnames(baa.t10y) <- c("BAA-T10Y")

t30y <- Quandl("FRED/DGS30", type="xts")/100
colnames(t30y) <- c("T30Y")

t3m <- Quandl("FRED/DGS3MO", type="xts")/100
colnames(t3m) <- c("T3M")

t30y.t3m<-t30y-t3m
colnames(t30y.t3m) <- c("T30Y-T3M")

chen <- cbind( t30y.t3m,indprod.logret, exinfl.diff, cpi.surprise, baa.t10y)

chen <- na.locf(chen)["2014/2018-01-01"]


# Get risk-free rate
rf.raw <- Quandl("FRED/DGS3MO", type="xts")/100
colnames(rf.raw) <- c("T3M")


# Get S&P 500, Russell 2000, and stock returns
adj.close <- 6  # 6th field is adjusted close
equity.tickers <- c("^GSPC","^RUT","BA","GD","HON","LMT","NOC","QCOM","RTN","UTX")
prices <- getSymbols(equity.tickers[1], source="yahoo", auto.assign=FALSE,
                     return.class="xts")[,adj.close]
for (i in 2:length(equity.tickers)) {
  prices.tmp <- getSymbols(equity.tickers[i], source="yahoo",
                           auto.assign=FALSE, return.class="xts")[,adj.close]
  prices <- cbind(prices, prices.tmp)
}
equity.names <- c("SPX","RUT","BA","GD","HON","LMT","NOC","QCOM","RTN","UTX")
colnames(prices) <- equity.names
returns <- diff(log(prices))

# Now we join all of the datasets together and trim to recent
alldata <- cbind(rf.raw, returns)["2014/2018-01-01"]


# create excess returns
equity.names.xs <- paste(equity.names, ".xs", sep="")
alldata.xs<-c()
# now in a for loop subtract off a daily risk-free rate, for example:
for (i in 1:length(equity.names)+1) {
  xs.temp <- alldata[,i] - alldata$T3M/250
  alldata.xs <- cbind(alldata.xs, xs.temp)
}
colnames(alldata.xs) <- equity.names.xs
alldata.xs<-na.omit(alldata.xs)["2014/2018-01-01"]

alldata.chen <- cbind( alldata.xs,chen)

alldata.chen <- na.locf(alldata.chen)["2014/2018-01-01"]


model.temp <-lm(alldata.chen[,1]~alldata.chen[,12]+alldata.chen[,13]+alldata.chen[,14]+alldata.chen[,15]+alldata.chen[,11])
model.temp<-summary(model.temp)




allmodel.coefficients<-c()
allmodel.coefficients.name<-c()
for (i in 1:length(equity.names)){
  model.temp <-lm(alldata.chen[,1]~alldata.chen[,12]+alldata.chen[,13]+alldata.chen[,14]+alldata.chen[,15]+alldata.chen[,11])
  model.temp<-summary(model.temp)
  allmodel.coefficients<-cbind(allmodel.coefficients,model.temp$coefficients[,c(1,4)])
  allmodel.coefficients.name<-cbind(allmodel.coefficients.name,paste(colnames(alldata.xs)[i], ".Est", sep=""),paste(colnames(alldata.xs)[i], ".t", sep=""))
}
allmodel.coefficients<-allmodel.coefficients
colnames(allmodel.coefficients)<-allmodel.coefficients.name


# Now do GARCH-in-mean models
gim.spec <- ugarchspec(variance.model=list(model="sGARCH", archm=TRUE, archpow=2),
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
garch.in.mean.spx <- ugarchfit(data=alldata.xs$SPX.xs, spec=gim.spec)
x<-show(garch.in.mean.spx)
x@fit$coef
sum(x@fit$log.likelihoods)
garch.coef<-c()
garch.MLE<-c()
for (i in 1:length(equity.names)){
  x <- ugarchfit(data=alldata.xs[,i], spec=gim.spec)
  garch.coef<-cbind(garch.coef,x@fit$coef)
  garch.MLE<-cbind(garch.MLE,sum(x@fit$log.likelihoods))
}
colnames(garch.coef)<-equity.names
colnames(garch.MLE)<-equity.names
garch.coef
garch.MLE
