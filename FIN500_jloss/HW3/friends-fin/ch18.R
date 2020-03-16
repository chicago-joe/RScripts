## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
## to do
library(xts)
library(Quandl)
library(moments)
Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

# Read in spot fx rates
fxspot.tickers <- c("CURRFX/USDEUR", "CURRFX/USDJPY", "CURRFX/USDCHF")
fxspot.raw <- Quandl(fxspot.tickers[1], type="xts")
for (i in 2:length(fxspot.tickers)) {
  fxspot.temp <- Quandl(fxspot.tickers[i], type="xts")
  fxspot.raw <- merge(fxspot.raw, fxspot.temp)
}
fxspot<-cbind(fxspot.raw$Rate,fxspot.raw$Rate.1,fxspot.raw$Rate.2)
colnames(fxspot) <- c("USDEUR.spot", "USDJPY.spot", "USDCHF.spot")

# Read in future fx rates
fxfut.tickers <- c("CHRIS/CME_EC1", "CHRIS/CME_JY1", "CHRIS/CME_SF1")
fxfut.raw <- Quandl(fxfut.tickers[1], type="xts")
for (i in 2:length(fxfut.tickers)) {
  fxfut.temp <- Quandl(fxfut.tickers[i], type="xts")
  fxfut.raw <- merge(fxfut.raw, fxfut.temp)
}
fxfut<-cbind(fxfut.raw$Settle,fxfut.raw$Settle.1,fxfut.raw$Settle.2)
colnames(fxfut) <- c("USDEUR.fut", "USDJPY.fut", "USDCHF.fut")

# Read in 3M USD deposit futures
usd.3m.raw <- Quandl("CHRIS/CME_ED1", type="xts")[,"Settle"]
eur.3m.raw <- Quandl("CHRIS/LIFFE_I1", type="xts")[,"Settle"]
jpy.3m.raw <- Quandl("CHRIS/TFX_JBA1", type="xts")[,"Settle"]
chf.3m.raw <- Quandl("CHRIS/LIFFE_S1", type="xts")[,"Settle"]
chf.3m.raw["20150122"] <- 101.08  # Fix 1st data error
chf.3m.raw["20150508"] <- 100.83  # Fix 2nd data error
deposits3m.raw <- cbind(usd.3m.raw, eur.3m.raw, jpy.3m.raw, chf.3m.raw)
deposits3m <- (100-deposits3m.raw)/100  # convert futures prices to yield
colnames(deposits3m) <- c("3MUSDLIBOR", "3MCHFLIBOR", "3MJPYTIBOR", "3MEURIBOR")

alldata.full <- cbind(fxspot, fxfut, deposits3m)
alldata <- alldata.full["20141001/20181001"]
alldata<-na.locf(alldata)

# JPY
USDJPY.parity<-(alldata$X3MUSDLIBOR-(alldata$USDJPY.spot*alldata$X3MJPYTIBOR/alldata$USDJPY.fut))
colnames(USDJPY.parity)<-"USDJPY.parity"
mean(USDJPY.parity)
sd(USDJPY.parity)
skewness(USDJPY.parity)
kurtosis(USDJPY.parity)

USDJPY.eurodollar<-alldata$X3MUSDLIBOR
USDJPY.conversion=alldata$USDJPY.spot*alldata$X3MJPYTIBOR/alldata$USDJPY.fut


plot.new()
mfrow(2,1)
plot(USDJPY.parity,main="USD/JPY Interest Rate Parity",ylab="interest rate",col="red",lwd=2,lty=2)
par(lines(USDJPY.eurodollar,col = "blue",lwd=1)+legend("left",c("Parity Diff","Eurodollar"),fill=c("red","blue"),cex=1.1))
lines(USDJPY.eurodollar,col = "blue",lwd=1)
legend(left)
legend("left",c("ParityDiff","Eurodollar"),cex=1.1,fill = c("red","blue"))






