## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
library(MASS)
library(xts)
library(quantmod)
library(Quandl)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

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
etf.rets<-na.omit(etf.rets)

commodity.symbols <- c("WTI", "AU", "CU", "Corn")
settle <- "Settle"  # settle field is labeled
commodity.tickers <- c("CHRIS/CME_CL1", "CHRIS/CME_GC1",
                       "CHRIS/CME_HG1", "CHRIS/CME_C1")
## Worth noticing the difference between here and instructions on the book
commodity.prices <- Quandl(commodity.tickers[1], type="xts")[,settle]
for (i in 2:length(commodity.symbols)) {
  commodity.tmp <- Quandl(commodity.tickers[i], type="xts")[,settle]
  commodity.prices <- cbind(commodity.prices, commodity.tmp)
}
colnames(commodity.prices) <- commodity.symbols
commodity.rets <- diff(log(commodity.prices))["20141001/20181001"]
commodity.rets<-na.omit(commodity.rets)

all.returns.tmp <- diff(log(cbind(etf.prices,commodity.prices)))["20141001/20181001"]
all.returns <- na.omit(all.returns.tmp)
# a)
# All
## set up portfolio with objective and constraints
n.assets <- length(colnames(all.returns))
port.spec <- portfolio.spec(assets = colnames(all.returns))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=all.returns, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("(All)Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("(All)Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("(All)Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("(All)Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

# ETFs
## set up portfolio with objective and constraints
n.assets <- length(colnames(etf.rets))
etf_port.spec <- portfolio.spec(assets = colnames(etf.rets))
etf_port.spec <- add.objective(portfolio=etf_port.spec, type="risk", name="StdDev")
etf_port.spec <- add.objective(portfolio=etf_port.spec, type="return", name="mean")
etf_port.spec <- add.constraint(portfolio=etf_port.spec, type="full_investment")
etf_port.spec <- add.constraint(portfolio=etf_port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
etf_eff.frontier <- create.EfficientFrontier(R=etf.rets, portfolio=etf_port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
etf_sharpe.ratios <- (etf_eff.frontier$frontier[,"mean"]-rf/250)/etf_eff.frontier$frontier[,"StdDev"]
etf_max.sharpe.ratio <- etf_sharpe.ratios[etf_sharpe.ratios == max(etf_sharpe.ratios)]
etf_optimal.port.name <- names(etf_max.sharpe.ratio)
etf_optimal.mean <- etf_eff.frontier$frontier[optimal.port.name,"mean"]
etf_optimal.sd <- etf_eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("(ETFs)Optimal Sharpe Ratio: %f", etf_max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("(ETFs)Optimal E(port return): %f", etf_optimal.mean*n.trading.days.per.year))
print(sprintf("(ETFs)Optimal sd(port return): %f", etf_optimal.sd*sqrt(n.trading.days.per.year)))
print("(ETFs)Optimal weights")
print(etf_eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

# Commodity
## set up portfolio with objective and constraints
n.assets <- length(colnames(commodity.rets))
commodity_port.spec <- portfolio.spec(assets = colnames(commodity.rets))
commodity_port.spec <- add.objective(portfolio=commodity_port.spec, type="risk", name="StdDev")
commodity_port.spec <- add.objective(portfolio=commodity_port.spec, type="return", name="mean")
commodity_port.spec <- add.constraint(portfolio=commodity_port.spec, type="full_investment")
commodity_port.spec <- add.constraint(portfolio=commodity_port.spec, type="box", min=-1, max=1)

## map out the efficient frontier (for variance risk)
commodity_eff.frontier <- create.EfficientFrontier(R=commodity.rets, portfolio=commodity_port.spec,
                                             n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
commodity_sharpe.ratios <- (commodity_eff.frontier$frontier[,"mean"]-rf/250)/commodity_eff.frontier$frontier[,"StdDev"]
commodity_max.sharpe.ratio <- commodity_sharpe.ratios[commodity_sharpe.ratios == max(commodity_sharpe.ratios)]
commodity_optimal.port.name <- names(commodity_max.sharpe.ratio)
commodity_optimal.mean <- commodity_eff.frontier$frontier[optimal.port.name,"mean"]
commodity_optimal.sd <- commodity_eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("(Commodity)Optimal Sharpe Ratio: %f", commodity_max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("(Commodity)Optimal E(port return): %f", commodity_optimal.mean*n.trading.days.per.year))
print(sprintf("(Commodity)Optimal sd(port return): %f", commodity_optimal.sd*sqrt(n.trading.days.per.year)))
print("(Commodity)Optimal weights")
print(commodity_eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

# c)
# All
## set up portfolio with objective and constraints
n.assets <- length(colnames(all.returns))
port.spec <- portfolio.spec(assets = colnames(all.returns))
port.spec <- add.objective(portfolio=port.spec, type="risk", name="StdDev")
port.spec <- add.objective(portfolio=port.spec, type="return", name="mean")
port.spec <- add.constraint(portfolio=port.spec, type="full_investment")
port.spec <- add.constraint(portfolio=port.spec, type="box", min=0, max=1)

## map out the efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R=all.returns, portfolio=port.spec,
                                         n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf/250)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios == max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("(All)Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("(All)Optimal E(port return): %f", optimal.mean*n.trading.days.per.year))
print(sprintf("(All)Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
print("(All)Optimal weights")
print(eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

# ETFs
## set up portfolio with objective and constraints
n.assets <- length(colnames(etf.rets))
etf_port.spec <- portfolio.spec(assets = colnames(etf.rets))
etf_port.spec <- add.objective(portfolio=etf_port.spec, type="risk", name="StdDev")
etf_port.spec <- add.objective(portfolio=etf_port.spec, type="return", name="mean")
etf_port.spec <- add.constraint(portfolio=etf_port.spec, type="full_investment")
etf_port.spec <- add.constraint(portfolio=etf_port.spec, type="box", min=0, max=1)

## map out the efficient frontier (for variance risk)
etf_eff.frontier <- create.EfficientFrontier(R=etf.rets, portfolio=etf_port.spec,
                                             n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
etf_sharpe.ratios <- (etf_eff.frontier$frontier[,"mean"]-rf/250)/etf_eff.frontier$frontier[,"StdDev"]
etf_max.sharpe.ratio <- etf_sharpe.ratios[etf_sharpe.ratios == max(etf_sharpe.ratios)]
etf_optimal.port.name <- names(etf_max.sharpe.ratio)
etf_optimal.mean <- etf_eff.frontier$frontier[optimal.port.name,"mean"]
etf_optimal.sd <- etf_eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("(ETFs)Optimal Sharpe Ratio: %f", etf_max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("(ETFs)Optimal E(port return): %f", etf_optimal.mean*n.trading.days.per.year))
print(sprintf("(ETFs)Optimal sd(port return): %f", etf_optimal.sd*sqrt(n.trading.days.per.year)))
print("(ETFs)Optimal weights")
print(etf_eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

# Commodity
## set up portfolio with objective and constraints
n.assets <- length(colnames(commodity.rets))
commodity_port.spec <- portfolio.spec(assets = colnames(commodity.rets))
commodity_port.spec <- add.objective(portfolio=commodity_port.spec, type="risk", name="StdDev")
commodity_port.spec <- add.objective(portfolio=commodity_port.spec, type="return", name="mean")
commodity_port.spec <- add.constraint(portfolio=commodity_port.spec, type="full_investment")
commodity_port.spec <- add.constraint(portfolio=commodity_port.spec, type="box", min=0, max=1)

## map out the efficient frontier (for variance risk)
commodity_eff.frontier <- create.EfficientFrontier(R=commodity.rets, portfolio=commodity_port.spec,
                                                   n.portfolios=100, type="mean-StdDev")

## daily Sharpe ratio
commodity_sharpe.ratios <- (commodity_eff.frontier$frontier[,"mean"]-rf/250)/commodity_eff.frontier$frontier[,"StdDev"]
commodity_max.sharpe.ratio <- commodity_sharpe.ratios[commodity_sharpe.ratios == max(commodity_sharpe.ratios)]
commodity_optimal.port.name <- names(commodity_max.sharpe.ratio)
commodity_optimal.mean <- commodity_eff.frontier$frontier[optimal.port.name,"mean"]
commodity_optimal.sd <- commodity_eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 252
print(sprintf("(Commodity)Optimal Sharpe Ratio: %f", commodity_max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("(Commodity)Optimal E(port return): %f", commodity_optimal.mean*n.trading.days.per.year))
print(sprintf("(Commodity)Optimal sd(port return): %f", commodity_optimal.sd*sqrt(n.trading.days.per.year)))
print("(Commodity)Optimal weights")
print(commodity_eff.frontier$frontier[optimal.port.name,(1:n.assets)+3])

