## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(quantmod)
library(Quandl)
library(rugarch)
library(lubridate)
Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

# get CMT USTs: 3M, 2Y, 10Y, 30Y
# Grab constant-maturity US Treasuries
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS5", "FRED/DGS10", "FRED/DGS30")
ust <- Quandl(ust.tickers, type="xts")/100
ust.colnames <- c("T3M", "T2Y", "T5Y", "T10Y", "T30Y")
colnames(ust) <- ust.colnames

## Download Fama-French data
french.base <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp"
factor.file <- "F-F_Research_Data_Factors_daily_CSV.zip"
french.url <- paste(french.base, factor.file, sep="/")
temp.file <- tempfile()
download.file(french.url, destfile=temp.file)
ff.tmp <- read.csv(unz(temp.file, "F-F_Research_Data_Factors_daily.CSV"),
                   header = TRUE, skip = 3)
unlink(temp.file)
# remove obnoxious last line, scale percentages, create xts object
ff.tmp <- ff.tmp[-length(ff.tmp[,1]),]
ff.data <- as.xts(ff.tmp[,c("SMB","HML")],
                  order.by=as.POSIXct(ff.tmp[[1]], format="%Y%m%d"))

## Download Carhart (momentum) data; skip absurd number of comment lines
factor.file <- "F-F_Momentum_Factor_daily_CSV.zip"
french.url <- paste(french.base, factor.file, sep="/")
temp.file <- tempfile()
download.file(french.url, destfile=temp.file)
umd.tmp <- read.csv(unz(temp.file, "F-F_Momentum_Factor_daily.CSV"),
                    header = TRUE, skip = 13)
unlink(temp.file)
# remove obnoxious last line, scale percentages, create xts object
umd.tmp <- umd.tmp[-length(umd.tmp[,1]),]
umd.data <- as.xts(umd.tmp[,c("Mom")], order.by=as.POSIXct(umd.tmp[[1]], format="%Y%m%d"))
colnames(umd.data) <- c("UMD")

### Handle monthly data
# For expected CPI and realized CPI, only get the first column of
# data... like so:
exinfl <- Quandl("FRBC/EXIN", type="xts")[,1]
colnames(exinfl) <- c("EXINFL")
exinfl.diff <- diff(exinfl)
colnames(exinfl.diff) <- c("EXINFL.Diff")

cpi <- Quandl("FRBC/USINFL", type="xts")[,1]
colnames(cpi) <- c("CPI")

excpi <- cpi*(1+exinfl)  # expected CPI in twelve months
cpi.surprise <- log(cpi) - log(lag(excpi, 12))  # % CPI surprise
colnames(cpi.surprise) <- c("INFLSURP")

# For industrial production, compute log-returns (% changes)
indprod <- Quandl("FRED/INDPRO", type="xts")
colnames(indprod) <- c("INDPROD")
indprod.logret <- diff(log(indprod))
colnames(indprod.logret) <- c("INDPROD.LogReturn")

# For credit spread
ltcorpbond<- Quandl("FRED/BAA10Y", type="xts")
colnames(ltcorpbond) <- c("BAAcorbond")
creditspread<-ltcorpbond-ust$T10Y
colnames(creditspread) <- c("CreditSpread")

# For yield curve factor
yieldcurvefactor<-ust$T30Y-ust$T3M
colnames(yieldcurvefactor) <- c("CurveFactor")

# Get index and stock prices; create returns
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
returns <- na.omit(returns)

# create excess returns for indices, stocks
equity.names.xs <- paste(equity.names, ".xs", sep="")
returnexcess<-returns
temp<-cbind(returns,ust$T3M)
for (i in 1:10) {
  returnexcess[,i] <- returns[,i] - temp$T3M/250
}
colnames(returnexcess) <- equity.names.xs

# Now we join all of the datasets together
# alldata.full <- cbind(ust, exinfl, cpi.surprise, ltcorpbond, indprod, ff.data, umd.data, returns)
alldata.full <- cbind(indprod.logret,exinfl.diff,cpi.surprise,creditspread,yieldcurvefactor,returnexcess)
# For monthly data: Last Observation Carried Forward (until new number)
alldata <- na.locf(alldata.full)["20141001/20181001"]


# Date issue
ffdatafortime<-ff.data
ff.datanew<-cbind(ff.data,returnexcess)["20141001/20181001"]
ff.datanew2 <- na.locf(ff.datanew)["20141001/20181001"]
# 打出一个奇数列1,3,5,...到nrow(m)
del <- seq(1, nrow(ff.datanew2), by = 2)
# 删掉用减号
ff.datanew2<-ff.datanew2[-del, ]

# Chen-Roll-Ross
# "SPX","RUT","BA","GD","HON","LMT","NOC","QCOM","RTN","UTX"
temp1<-{0}
temp2<-{0}
temp3<-{0}
for(i in 6:15)  {
  print(colnames(alldata[,i]))
  model.Chen<- lm(alldata[,i]~alldata$INDPROD.LogReturn+alldata$EXINFL.Diff+alldata$INFLSURP+alldata$CreditSpread+alldata$CurveFactor)
  print(summary(model.Chen))
  temp1<-cbind(temp1,model.Chen$coefficients)
  a <- summary(model.Chen)
  temp2<-cbind(temp2,coef(a)[, "t value"])
  temp3<-cbind(temp3,colnames(alldata[,i]))
}

# Now do GARCH-in-mean models
# "SPX","RUT","BA","GD","HON","LMT","NOC","QCOM","RTN","UTX"
gim.spec <- ugarchspec(variance.model=list(model="sGARCH", archm=TRUE, archpow=2),
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE))

garch.in.mean.SPX <- ugarchfit(data=ts(alldata$SPX.xs), spec=gim.spec)
show(garch.in.mean.SPX)

garch.in.mean.RUT <- ugarchfit(data=ts(alldata$RUT.xs), spec=gim.spec)
show(garch.in.mean.RUT)

garch.in.mean.BA <- ugarchfit(data=ts(alldata$BA.xs), spec=gim.spec)
show(garch.in.mean.BA)

garch.in.mean.GD <- ugarchfit(data=ts(alldata$GD.xs), spec=gim.spec)
show(garch.in.mean.GD)

garch.in.mean.HON <- ugarchfit(data=ts(alldata$HON.xs), spec=gim.spec)
show(garch.in.mean.HON)

garch.in.mean.LMT <- ugarchfit(data=ts(alldata$LMT.xs), spec=gim.spec)
show(garch.in.mean.LMT)

garch.in.mean.NOC <- ugarchfit(data=ts(alldata$NOC.xs), spec=gim.spec)
show(garch.in.mean.NOC)

garch.in.mean.QCOM <- ugarchfit(data=ts(alldata$QCOM.xs), spec=gim.spec)
show(garch.in.mean.QCOM)

garch.in.mean.RTN <- ugarchfit(data=ts(alldata$RTN.xs), spec=gim.spec)
show(garch.in.mean.RTN)

garch.in.mean.UTX <- ugarchfit(data=ts(alldata$UTX.xs), spec=gim.spec)
show(garch.in.mean.UTX)
temp4<-cbind(garch.in.mean.SPX@fit[[10]],garch.in.mean.RUT@fit[[10]],garch.in.mean.BA@fit[[10]],garch.in.mean.GD@fit[[10]],
             garch.in.mean.HON@fit[[10]],garch.in.mean.LMT@fit[[10]],garch.in.mean.NOC@fit[[10]],garch.in.mean.QCOM@fit[[10]],
             garch.in.mean.RTN@fit[[10]],garch.in.mean.UTX@fit[[10]])
#Loglikelihood
temp5<-cbind((-sum(garch.in.mean.SPX@fit[[8]])),(-sum(garch.in.mean.RUT@fit[[8]])),(-sum(garch.in.mean.BA@fit[[8]])),(-sum(garch.in.mean.GD@fit[[8]])),
             (-sum(garch.in.mean.HON@fit[[8]])),(-sum(garch.in.mean.LMT@fit[[8]])),(-sum(garch.in.mean.NOC@fit[[8]])),(-sum(garch.in.mean.QCOM@fit[[8]])),
             (-sum(garch.in.mean.RTN@fit[[8]])),(-sum(garch.in.mean.UTX@fit[[8]])))

#  Fama-French Model
BA.French<-lm(BA.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(BA.French)

GD.French<-lm(GD.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(GD.French)

HON.French<-lm(HON.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(HON.French)

LMT.French<-lm(LMT.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(LMT.French)

NOC.French<-lm(NOC.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(NOC.French)

QCOM.French<-lm(QCOM.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(QCOM.French)

RTN.French<-lm(RTN.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(RTN.French)

UTX.French<-lm(UTX.xs~SPX.xs+SMB+HML,data=ff.datanew2)
summary(UTX.French)   # "SPX","RUT","BA","GD","HON","LMT","NOC","QCOM","RTN","UTX"
temp6<-{0}
temp7<-{0}
for(i in 5:12)  {
  print(colnames(ff.datanew2[,i]))
  model.French<- lm(ff.datanew2[,i]~ff.datanew2$SPX.xs+ff.datanew2$SMB+ff.datanew2$HML)
  print(summary(model.French))
  temp6<-cbind(temp6,model.French$coefficients)
  a <- summary(model.French)
  temp7<-cbind(temp7,coef(a)[, "t value"])
}

