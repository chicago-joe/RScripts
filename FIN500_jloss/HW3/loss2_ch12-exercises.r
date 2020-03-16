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

# Get constant-maturity (US) Treasuries
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS6MO", "FRED/DGS1",
	"FRED/DGS2", "FRED/DGS5", "FRED/DGS10", "FRED/DGS20", "FRED/DGS30")
ust.raw <- Quandl(ust.tickers, type="xts")
ust.colnames <- c("UST3M", "UST6M", "UST1Y", "UST2Y", "UST5Y", "UST10Y", "UST20Y", "UST30Y")
colnames(ust.raw) <- ust.colnames
ust <- ust.raw["2014/2018-01-01"]

#pca.usd <- prcomp(ust[,c(1:8)], center=TRUE, scale.=TRUE)
#pca.usd <- prcomp(yc.usd, scale=FALSE, center=FALSE)
#pca.usd$rotation  # eigenvectors
#pca.usd$sdev^2    # eigenvalues


# Why is this not scaled/centered
pca.usd <- prcomp(~ UST3M + UST6M + UST1Y + UST2Y + UST5Y + UST10Y + UST20Y + UST30Y, data=ust, scale=FALSE, center=FALSE)
pca.usd
pca.usd$rotation
pca.usd$sdev^2


