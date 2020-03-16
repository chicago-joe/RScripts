## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(zoo)
library(xts)
library(Quandl)
library(ggplot2)
library(RColorBrewer)
library(Metrics)


Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

# Grab constant-maturity US Treasuries 3-Month Treasury Constant Maturity Rate; 2-Year Treasury Constant Maturity Rate; 10-Year Treasury Constant Maturity Rate; 30-Year Treasury Constant Maturity Rate E(R)
# Name columns so we know what each holds after joining them together
ust.tickers <- c("FRED/DGS3MO","FRED/DGS6MO", "FRED/DGS1",  "FRED/DGS2","FRED/DGS5", "FRED/DGS10", "FRED/DGS20", "FRED/DGS30")
ust.raw <- Quandl(ust.tickers, type="xts")/100
ust<-diff(ust.raw)["20141001/20181001"]
colnames(ust) <- c("T3M","T6M","T1Y", "T2Y", "T5Y","T10Y","T20Y", "T30Y")

ust<-ust["20141001/20181001"]


pca.usd <- prcomp(ust,
                  scale=FALSE, center=FALSE)
#pca.usd <- prcomp(yc.usd, scale=FALSE, center=FALSE)
pca.usd$rotation[,1:5]# eigenvectors
head(pca.usd$sdev^2,5)# eigenvalues
