## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(Quandl)
library(quantmod)

Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

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
alldata <- cbind(rf.raw, returns)["20141001/20181001"]


# create excess returns
equity.names.xs <- paste(equity.names, ".xs", sep="")
alldata.xs<-c()
# now in a for loop subtract off a daily risk-free rate, for example:
for (i in 1:length(equity.names)+1) {
  xs.temp <- alldata[,i] - alldata$T3M/250
  alldata.xs <- cbind(alldata.xs, xs.temp)
}
colnames(alldata.xs) <- equity.names.xs

# If your ticker were DAL and you wanted to model returns
# (not excess returns) using ESTOX and SMI, you would do like so:
model.temp <- lm(SPX.xs ~  + RUT.xs, data=alldata.xs)
model.temp<-summary(model.temp)


allmodel.coefficients<-c()
allmodel.coefficients.name<-c()
for (i in 3:length(equity.names)){
  model.temp <-lm(alldata.xs[,i]~alldata.xs[,1]+alldata.xs[,2])
  model.temp<-summary(model.temp)
  allmodel.coefficients<-cbind(allmodel.coefficients,model.temp$coefficients[,c(1,2,4)])
  allmodel.coefficients.name<-cbind(allmodel.coefficients.name,paste(colnames(alldata.xs)[i], ".Est", sep=""),paste(colnames(alldata.xs)[i], ".t", sep=""))
}
allmodel.coefficients<-allmodel.coefficients
colnames(allmodel.coefficients)<-allmodel.coefficients.name
allmodel.coefficients
# NOTE that this is not the model you are supposed to do

SPXmodel.coefficients<-c()
SPXmodel.coefficients.name<-c()
for (i in 3:length(equity.names)){
  model.temp <-lm(alldata.xs[,i]~alldata.xs[,1])
  model.temp<-summary(model.temp)
  SPXmodel.coefficients<-cbind(SPXmodel.coefficients,model.temp$coefficients[,c(1,4)])
  SPXmodel.coefficients.name<-cbind(SPXmodel.coefficients.name,paste(colnames(alldata.xs)[i], ".Est", sep=""),paste(colnames(alldata.xs)[i], ".t", sep=""))
}
SPXmodel.coefficients<-SPXmodel.coefficients
colnames(SPXmodel.coefficients)<-SPXmodel.coefficients.name
SPXmodel.coefficients

RUTmodel.coefficients<-c()
RUTmodel.coefficients.name<-c()
for (i in 3:length(equity.names)){
  model.temp <-lm(alldata.xs[,i]~alldata.xs[,2])
  model.temp<-summary(model.temp)
  RUTmodel.coefficients<-cbind(RUTmodel.coefficients,model.temp$coefficients[,c(1,4)])
  RUTmodel.coefficients.name<-cbind(RUTmodel.coefficients.name,paste(colnames(alldata.xs)[i], ".Est", sep=""),paste(colnames(alldata.xs)[i], ".t", sep=""))
}
RUTmodel.coefficients<-RUTmodel.coefficients
colnames(RUTmodel.coefficients)<-RUTmodel.coefficients.name
RUTmodel.coefficients
