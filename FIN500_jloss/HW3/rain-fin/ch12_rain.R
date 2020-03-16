library(zoo)
library(xts)
library(Quandl)
library(ggplot2)
library(RColorBrewer)
library(Metrics)


Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

# Grab constant-maturity US Treasuries 3-Month Treasury Constant Maturity Rate; 2-Year Treasury Constant Maturity Rate; 10-Year Treasury Constant Maturity Rate; 30-Year Treasury Constant Maturity Rate E(R)
# Name columns so we know what each holds after joining them together
ust.tickers <- c("FRED/DGS3MO","FRED/DGS6MO", "FRED/DGS1",  "FRED/DGS2", "FRED/DGS10", "FRED/DGS20", "FRED/DGS30")
ust.raw <- Quandl(ust.tickers, type="xts")/100
colnames(ust.raw) <- c("T3M","T6M","T1Y", "T2Y", "T10Y","T20Y", "T30Y")

ust.raw<-ust.raw["2014/2018-01-01"]


pca.usd <- prcomp(~ T3M + T6M + T1Y + T2Y+ T10Y+T20Y+T30Y, data=ust.raw,
                  scale=FALSE, center=FALSE)
#pca.usd <- prcomp(yc.usd, scale=FALSE, center=FALSE)
pca.usd$rotation[,1:5]# eigenvectors
head(pca.usd$sdev^2,5)# eigenvalues
