## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(xts)
library(Quandl)
library(Metrics)
Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

# https://en.wikipedia.org/wiki/Principal_component_analysis 
# Grab constant-maturity US Treasuries
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS6MO", "FRED/DGS1", "FRED/DGS2", "FRED/DGS5", "FRED/DGS10", "FRED/DGS20", "FRED/DGS30")
ust <- Quandl(ust.tickers, type="xts")/100
ust.colnames <- c("T3M", "T6M", "T1Y", "T2Y", "T5Y", "T10Y", "T20Y", "T30Y")
colnames(ust) <- ust.colnames
yc.usd<-diff(ust)["20141001/20181001"]
ust<-ust["20141001/20181001"]
#pca.usd <- prcomp(~ T3M + T6M + T1Y + T2Y + T5Y + T10Y + T20Y + T30Y, data=ust,
                  #scale=FALSE, center=FALSE)
pca.usd <- prcomp(yc.usd, scale=FALSE, center=FALSE)
pca.usd$rotation  # eigenvectors
pca.usd$sdev^2    # eigenvalues
pca.usd
summary(pca.usd)

