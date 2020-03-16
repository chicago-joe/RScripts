## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

library(MASS)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(Quandl)
library(quantmod)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(xts)

Quandl.api_key('koK7avVzeTYC4mWdtk6q')

## risk-free rate (for example purposes)
rf <- 0.01

etf.symbols <- c("SPY", "IWM", "AGG", "FEZ", "ACWI", "IYR")
adj.close <- 6  # 6th field is adjusted close
etf.prices <- getSymbols(etf.symbols[1], source="yahoo",
                         auto.assign=FALSE, return.class="xts")[,adj.close]
for (i in 2:length(etf.symbols)) {
  etf.tmp <- getSymbols(etf.symbols[i], source="yahoo",
                        auto.assign=FALSE, return.class="xts")[,adj.close]
  etf.prices <- cbind(etf.prices, etf.tmp)
}
colnames(etf.prices) <- etf.symbols
etf.rets <- diff(log(etf.prices))["20141001/20181001"]

#commodity.symbols <- c("WTI", "Natgas", "AU", "CU", "Corn")
commodity.symbols <- c("WTI", "AU", "CU", "Corn")
settle <- "Settle"  # settle field is labeled
#commodity.tickers <- c("CHRIS/CME_CL1", "CHRIS/CME_NG1", "CHRIS/CME_GC1","CHRIS/CME_HG1", "CHRIS/CME_C1")
commodity.tickers <- c("CHRIS/CME_CL1", "CHRIS/CME_GC1","CHRIS/CME_HG1", "CHRIS/CME_C1")
commodity.prices <- Quandl(commodity.tickers[1], type="xts")[,settle]
for (i in 2:length(commodity.symbols)) {
  commodity.tmp <- Quandl(commodity.tickers[i], type="xts")[,settle]
  commodity.prices <- cbind(commodity.prices, commodity.tmp)
}
colnames(commodity.prices) <- commodity.symbols
comm.rets<-diff(log(commodity.prices))["20141001/20181001"]
comm.rets <- na.omit(comm.rets)

all.rets.tmp <- diff(log(cbind(etf.prices,commodity.prices)))["20141001/20181001"]
all.rets <- na.omit(all.rets.tmp)

### 9.1

## set up portfolio with objective and constraints
n.assets <- length(colnames(etf.rets))
port.spec <- portfolio.spec(assets = colnames(etf.rets))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=etf.rets, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print("P etf")
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])


## set up portfolio with objective and constraints
n.assets <- length(colnames(comm.rets))
port.spec <- portfolio.spec(assets = colnames(comm.rets))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=comm.rets, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print("P comm")
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])




## set up portfolio with objective and constraints

n.assets <- length(colnames(all.rets))
port.spec <- portfolio.spec(assets = colnames(all.rets))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=all.rets, portfolio=port.spec,n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print("P all")
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

#9.3

## set up portfolio with objective and constraints
n.assets <- length(colnames(etf.rets))
port.spec <- portfolio.spec(assets = colnames(etf.rets))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=0, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=etf.rets, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print("P etf")
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])


## set up portfolio with objective and constraints
n.assets <- length(colnames(comm.rets))
port.spec <- portfolio.spec(assets = colnames(comm.rets))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=0, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=comm.rets, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print("P comm")
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])




## set up portfolio with objective and constraints

n.assets <- length(colnames(all.rets))
port.spec <- portfolio.spec(assets = colnames(all.rets))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=0, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=all.rets, portfolio=port.spec,n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print("P all")
print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])
