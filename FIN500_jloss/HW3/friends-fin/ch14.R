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
# now in a for loop subtract off a daily risk-free rate, for example:
alldata1<-alldata
for (i in 2:11)
{
  alldata1[,i] <- alldata[,i] - alldata$T3M/250
}
alldata1.names <- c("T3M","SPX.xs","RUT.xs","BA.xs","GD.xs","HON.xs","LMT.xs","NOC.xs","QCOM.xs","RTN.xs","UTX.xs")
colnames(alldata1) <- alldata1.names  
alldata1_new<-alldata1[,2:11]
alldata<-cbind(alldata,alldata1_new)
alldata<-na.omit(alldata)

# If your ticker were DAL and you wanted to model returns
# (not excess returns) using ESTOX and SMI, you would do like so:
temp1<-{0}
temp2<-{0}
temp3<-{0}
temp4<-{0}
temp5<-{0}
temp6<-{0}
for(i in 14:21)
{
  print(colnames(alldata[,i]))
  model.all <- lm(alldata[,i] ~ alldata$SPX+alldata$RUT)
  print(summary(model.all))
  temp1<-cbind(temp1,model.all$coefficients)
  a <- summary(model.all)
  temp2<-cbind(temp2,coef(a)[, "t value"])
  model.SPX <- lm(alldata[,i] ~ alldata$SPX)
  print(summary(model.SPX))
  temp3<-cbind(temp3,model.SPX$coefficients)
  b <- summary(model.SPX)
  temp4<-cbind(temp4,coef(b)[, "t value"])
  model.RUT <- lm(alldata[,i] ~ alldata$RUT)
  print(summary(model.RUT))
  temp5<-cbind(temp5,model.RUT$coefficients)
  b <- summary(model.RUT)
  temp6<-cbind(temp6,coef(b)[, "t value"])
}
# NOTE that this is not the model you are supposed to do

