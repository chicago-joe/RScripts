## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

library(Quandl)
library(quantmod)
library(xts)

Quandl.api_key('koK7avVzeTYC4mWdtk6q')

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

#tmp <- xts(order.by = index(alldata))
tmp <- alldata$T3M/250
colnames(tmp)<-c("T3M.daily")
alldata<-cbind(tmp,alldata)


for (i in ){
  xs.tmp <- alldata[i]-alldata[,1]
}

for (tkr in equity.names) {
	tmp <- alldata - alldata[1]/250
}
# now in a for loop subtract off a daily risk-free rate, for example:
alldata$SPX.xs <- alldata$SPX - alldata$T3M/250
alldata$RUT.xs <- alldata$RUT - alldata$T3M/250
alldata$BA.xs <- alldata$BA - alldata$T3M/250
alldata$GD.xs <- alldata$GD - alldata$T3M/250
alldata$HON.xs <- alldata$HON - alldata$T3M/250
alldata$LMT.xs <- alldata$LMT - alldata$T3M/250
alldata$NOC.xs <- alldata$NOC - alldata$T3M/250
alldata$RTN.xs <- alldata$RTN - alldata$T3M/250
alldata$UTX.xs <- alldata$UTX - alldata$T3M/250

xsdata <- cbind(alldata$SPX.xs,alldata$RUT.xs,alldata$BA.xs,alldata$GD.xs,alldata$HON.xs,
      alldata$LMT.xs,alldata$NOC.xs,alldata$RTN.xs,alldata$UTX.xs,alldata$T3M)

# If your ticker were DAL and you wanted to model returns
# (not excess returns) using ESTOX and SMI, you would do like so:
# NOTE that this is not the model you are supposed to do
simple.wrong.model <- lm(DAL ~ ESTOX + SMI, data=alldata)
summary(simple.wrong.model)

CAPM.SPX <- lm(xsdata$BA.xs ~ xsdata$SPX.xs, data=xsdata)		

