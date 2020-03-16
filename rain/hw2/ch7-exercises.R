## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(Quandl)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
Quandl.api_key('iVyBuKymy_j_R7Xxze9t')


# Example of reading in CMTs from Quandl
# Name columns so we know what each holds after joining them together
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS10", "FRED/DGS30")
ust.raw <- Quandl(ust.tickers, type="xts")/100
colnames(ust.raw) <- c("T3M.yld", "T2Y.yld", "T10Y.yld", "T30Y.yld")

# This is a way to get approximate returns for these bonds.
# Later on, you will learn about duration and why we can do this. 
ust.yieldchanges <- diff(ust.raw)
colnames(ust.yieldchanges) <- c("T3M", "T2Y", "T10Y", "T30Y")
ust <- ust.yieldchanges
ust$T3M  <- -0.25*ust.yieldchanges$T3M
ust$T2Y  <- -1.98*ust.yieldchanges$T2Y
ust$T10Y <- -8.72*ust.yieldchanges$T10Y
ust$T30Y <- -19.2*ust.yieldchanges$T30Y

# Get Eurodollar futures (settlement) prices and create log-returns.
ed1.raw <- Quandl("CHRIS/CME_ED1", type="xts")[,"Settle"]
ed1 <- diff(log(ed1.raw))
colnames(ed1) <- c("ED1")
ed8.raw <- Quandl("CHRIS/CME_ED8", type="xts")[,"Settle"]
ed8 <- diff(log(ed8.raw))
colnames(ed8) <- c("ED8")

ted1<-(100-ed1.raw-ust$T3M)
colnames(ted1) <- c("TED1")
ted8<-(100-ed8.raw-ust$T2Y)
colnames(ted8) <- c("TED8")
# Get S&P 500 prices (just adjusted close); then create log-returns.
# Do similarly for the Russell 2000, and other stocks.
adj.close <- 6  # 6th field is adjusted close
spx.raw <- getSymbols("^GSPC", source="yahoo", auto.assign=FALSE, return.class="xts")[,adj.close]
colnames(spx.raw) <- c("SPX.prc")
spx <- diff(log(spx.raw))
colnames(spx) <- c("SPX")

adj.close <- 6  # 6th field is adjusted close
rut.raw <- getSymbols("^RUT", source="yahoo", auto.assign=FALSE, return.class="xts")[,adj.close]
colnames(rut.raw) <- c("RUT.prc")
rut <- diff(log(rut.raw))
colnames(rut) <- c("RUT")

#Group1 and group2
group1.eq.tickers<-c("PG","XOM","IBM","MMM","KO","GS","AXP","WMT","MRK","DIS","HD")
group2.eq.tickers<-c("AAPL","CARB","CBRE",
              "CZR","F","FBC","IMGN","IRDM","SBUX","SVU","SYMC","UPS","VLO")
group1.price <- getSymbols(group1.eq.tickers[1], source="yahoo", auto.assign=FALSE, return.class="xts")[,6]
for(i in 2:length(group1.eq.tickers))
{
  eq.temp <- getSymbols(group1.eq.tickers[i], source="yahoo", auto.assign=FALSE, return.class="xts")[,6]
  group1.price<-merge(group1.price, eq.temp, all=T)
}
start.date<-as.Date("2015/10/30")
print(group1.price[as.Date(start.date)])
start.prices<-as.vector(group1.price[as.Date(start.date)])
group1.sigleprice<-group1.price %*% (1/start.prices)
group1.sigleprice <- xts(x=group1.sigleprice, order.by=index(group1.price))
colnames(group1.sigleprice)<-c("group1")
group1.sigleprice.log <- diff(log(group1.sigleprice))
colnames(group1.sigleprice.log)<-c("group1.log")

group2.price <- getSymbols(group2.eq.tickers[1], source="yahoo", auto.assign=FALSE, return.class="xts")[,6]
for(i in 2:length(group2.eq.tickers))
{
  eq.temp <- getSymbols(group2.eq.tickers[i], source="yahoo", auto.assign=FALSE, return.class="xts")[,6]
  group2.price<-merge(group2.price, eq.temp, all=T)
}
start.date<-as.Date("2015/10/30")
print(group2.price[as.Date(start.date)])
start.prices<-as.vector(group2.price[as.Date(start.date)])
group2.sigleprice<-group2.price %*% (1/start.prices)
group2.sigleprice <- xts(x=group2.sigleprice, order.by=index(group2.price))
colnames(group2.sigleprice)<-c("group2")
group2.sigleprice.log <- diff(log(group2.sigleprice))
colnames(group2.sigleprice.log)<-c("group2.log")

#Commodities

com.tickers<-c("CHRIS/CME_CL1","CHRIS/CME_NG1","CHRIS/CME_HG1","CHRIS/CME_C1")
com.temp <- Quandl(com.tickers[1], return.class="xts")[,7]
com.date <- Quandl(com.tickers[1], return.class="xts")[,1]
yourcom.raw <- xts(x=com.temp, order.by=com.date)
for(i in 2:length(com.tickers))
{
  com.temp <- Quandl(com.tickers[i], return.class="xts")[,7]
  com.date <- Quandl(com.tickers[i], return.class="xts")[,1]
  com.temp <- xts(x=com.temp, order.by=com.date)
  yourcom.raw<-merge(yourcom.raw, com.temp, all=T)
}
colnames(yourcom.raw) <- c("oil.prc","gas.prc","copper.prc","corm.prc")
yourcom <- diff(log(yourcom.raw))
colnames(yourcom) <- c("oil","gas","copper","corm")


# Join all of the datasets together: US Treasuries, Eurodollars,
# S&P 500, Russell 2000, and group 1 and group 2 stocks.
# Then trim them down so the dates are consistent.
alldata_full <- cbind(ust.raw, ust, ed1.raw,ed1,ed8.raw,ed8,ted1,ted8, spx.raw, spx,rut.raw,rut,group1.sigleprice,group1.sigleprice.log,group2.sigleprice,group2.sigleprice.log,yourcom.raw,yourcom )
alldata <- alldata_full["20151030/20181030"]
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++done
# Calculate annual volatilities like so:
apply(alldata,2,mean,na.rm=TRUE)
apply(alldata, 2, sd,na.rm=TRUE)*sqrt(250)
# skewness and kurtosis are independent of time; no need to scale them
skewness(alldata,na.rm=TRUE)
#method="moment")
kurtosis(alldata,na.rm=TRUE)
#method="moment")
SemiDeviation(alldata)




##5 Correlation Heat Map
## use some code from on-line source:
## http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
cordata<-cbind(alldata$T3M,alldata$T2Y,alldata$T10Y,alldata$T30Y,alldata$ED1,alldata$ED8,alldata$SPX,alldata$RUT,
               alldata$group1.log, alldata$group2.log,alldata$oil,alldata$gas,alldata$copper,alldata$corm)
cordata<-na.omit(cordata)
# Cor Matrix
cormat <- round(cor(cordata),2)
head(cormat) 
# Get Upper Cor Matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the Correlation Matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


## from Cornish-Fisher expansion


printcsv<-xts(x=colnames(cordata),order.by=Sys.Date()+1:14)

temp<-c(apply(cordata,2,mean,na.rm=TRUE))
printcsv<-merge(printcsv,temp) 
temp<-c(apply(cordata, 2, sd,na.rm=TRUE)*sqrt(250))
printcsv<-merge(printcsv,temp) 
temp<-c(skewness(cordata,na.rm=TRUE))
printcsv<-merge(printcsv,temp) 
#method="moment")
temp<-c(kurtosis(cordata,na.rm=TRUE))
printcsv<-merge(printcsv,temp) 
#method="moment")
temp<-c(SemiDeviation(cordata))
printcsv<-merge(printcsv,temp) 
temp<-c()
temp2<-c()
for (i in 1:14) {
  #print(colnames(alldata)[i])
  print(VaR.cornfish <- VaR(cordata[,i], p=0.05, method="modified", mu=mean(cordata[,i],na.rm=TRUE), sigma=sd(cordata[,i],na.rm=TRUE),
                            m3=skewness(cordata[,i],na.rm=TRUE), m4=kurtosis(cordata[,i],na.rm=TRUE),na.rm=TRUE))
  print(es.cornfish <- ES(cordata[,i], p=0.05, method="modified", mu=mean(cordata[,i],na.rm=TRUE), sigma=sd(cordata[,i],na.rm=TRUE),
                          m3=skewness(cordata[,i],na.rm=TRUE), m4=kurtosis(cordata[,i],na.rm=TRUE),na.rm=TRUE))
  temp<-c(temp,VaR.cornfish)
  temp2<-c(temp2,es.cornfish)
}
printcsv<-merge(printcsv,temp) 
printcsv<-merge(printcsv,temp2)

data_xts <- as.xts(printcsv) 
tmp <- tempfile() 
write.zoo(data_xts,sep=",",file=tmp)
for (i in 1:14) {
  temp<-c(mean(cordata[,i]),sd(cordata[,i])*sqrt(250),skewness(cordata[,i]),kurtosis(cordata[,i]),SemiDeviation(cordata[,i]),
          VaR(cordata[,i], p=0.05, method="modified", mu=mean(cordata[,i],na.rm=TRUE), sigma=sd(cordata[,i],na.rm=TRUE),
              m3=skewness(cordata[,i],na.rm=TRUE), m4=kurtosis(cordata[,i],na.rm=TRUE),na.rm=TRUE))
}

data_xts <- as.xts(cordata) 
tmp <- tempfile() 
write.zoo(data_xts,sep=",",file=tmp)
