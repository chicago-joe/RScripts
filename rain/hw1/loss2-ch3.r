

library(xts)
library(Quandl)
library(quantmod)
library(ggplot2)
library(RColorBrewer)
Quandl.api_key("koK7avVzeTYC4mWdtk6q")


# Get Fixed Income
fi.tickers <- c("CHRIS/MX_CGF1.1", "CHRIS/MX_CGF1.2", "CHRIS/MX_CGB1.1", "CHRIS/MX_CGB1.2")
fi.raw <- Quandl(fi.tickers, type="raw")
fi.colnames <- c("Date", "CGF1 Bid", "CGF1 Ask", "CGB1 Bid", "CGB1 Ask")
colnames(fi.raw) <- fi.colnames

# Filter out missing bid/ask prices
newdata <-subset(fi.raw, `CGF1 Bid`>0&`CGF1 Ask`>0&`CGB1 Bid`>0&`CGB1 Ask`>0,
                 select = c(`Date`, `CGF1 Bid`, `CGF1 Ask`, `CGB1 Bid`, `CGB1 Ask`))
# Create spreads
newdata$CGF1.spread <- newdata$`CGF1 Ask`- newdata$`CGF1 Bid`
newdata$CGB1.spread <- newdata$`CGB1 Ask`- newdata$`CGB1 Bid`
# Create fractional spreads
newdata$CGF1.fracspread <- log(newdata$`CGF1 Ask`) - log(newdata$`CGF1 Bid`)
newdata$CGB1.fracspread <- log(newdata$`CGB1 Ask`) - log(newdata$`CGB1 Bid`)
# reorder data columns
fi.all <- newdata[c(1,2,3,6,8,4,5,7,9)]
summary(fi.all$CGB1.spread)
summary(fi.all$CGB1.fracspread)
summary(fi.all$CGF1.spread)
summary(fi.all$CGF1.fracspread)
boxplot(as.matrix(data.frame(fi.all$CGB1.spread,fi.all$CGF1.spread)), col = "orange")
boxplot(as.matrix(data.frame(fi.all$CGB1.fracspread,fi.all$CGF1.fracspread)), col = "orange")

# Commodities
commodities.tickers <- c("LME/PR_CU.1", "LME/PR_CU.2", "LME/PR_AL.1", "LME/PR_AL.2", "LME/PR_TN.1", "LME/PR_TN.2")
commodities.raw <- Quandl(commodities.tickers, type="raw")
commodities.colnames <- c("Date", "CU.buy", "CU.sell", "AL.buy", "AL.sell", "TN.buy", "TN.sell")
colnames(commodities.raw) <- commodities.colnames

# Filter out missing bid/ask prices
newcom <- subset(commodities.raw, `CU.buy`>0&`CU.sell`>0&`AL.buy`>0&`AL.sell`>0&`TN.buy`>0&`TN.sell`>0,
                 select = c(`Date`, `CU.buy`, `CU.sell`, `AL.buy`, `AL.sell`, `TN.buy`, `TN.sell`))
# Spreads
# Fucked up on this. need to switch to = SELL - BUY. My bad.
newcom$CU.spread <- newcom$`CU.sell`- newcom$`CU.buy`
newcom$AL.spread <- newcom$`AL.sell`- newcom$`AL.buy`
newcom$TN.spread <- newcom$`TN.sell`- newcom$`TN.buy`
newcom$CU.fracspread <- log(newcom$`CU.sell`) - log(newcom$`CU.buy`)
newcom$AL.fracspread <- log(newcom$`AL.sell`) - log(newcom$`AL.buy`)
newcom$TN.fracspread <- log(newcom$`TN.sell`) - log(newcom$`TN.buy`)
commodities.all <- newcom[c(1,2,3,8,11,4,5,9,12,6,7,10,13)]

summary(commodities.all$CU.spread)
summary(commodities.all$CU.fracspread)
summary(commodities.all$AL.spread)
summary(commodities.all$AL.fracspread)
summary(commodities.all$TN.spread)
summary(commodities.all$TN.fracspread)
boxplot(as.matrix(data.frame(commodities.all$CU.spread,commodities.all$AL.spread,commodities.all$TN.spread)), col = "orange")
boxplot(as.matrix(data.frame(commodities.all$CU.fracspread,commodities.all$AL.fracspread,commodities.all$TN.fracspread)), col = "orange")


# Equity Indices
idx.tickers <- c("CHRIS/HKEX_HSI1.2", "CHRIS/HKEX_HSI1.3", "CHRIS/MX_SXM1.1", "CHRIS/MX_SXM1.2")
idx.raw <- Quandl(idx.tickers, type="raw")
idx.colnames <- c("Date", "HSI.bid", "HSI.ask", "TSX.bid", "TSX.ask")
colnames(idx.raw) <- idx.colnames

# Filter out missing bid/ask prices
idx.new <- subset(idx.raw, `HSI.bid`>0&`HSI.ask`>0&`TSX.bid`>0&`TSX.ask`>0,
                  select = c(`Date`, `HSI.bid`, `HSI.ask`, `TSX.bid`, `TSX.ask`))
# Spreads
idx.new$HSI.spread <- idx.new$`HSI.ask`- idx.new$`HSI.bid`
idx.new$TSX.spread <- idx.new$`TSX.ask`- idx.new$`TSX.bid`
idx.new$HSI.fracspread <- log(idx.new$`HSI.ask`) - log(idx.new$`HSI.bid`)
idx.new$TSX.fracspread <- log(idx.new$`TSX.ask`) - log(idx.new$`TSX.bid`)

# TODO - CHECK COLUMN ORDERING
idx.all <- idx.new[c(1,2,3,6,8,4,5,7,9)]

summary(idx.new$HSI.spread)
summary(idx.new$HSI.fracspread)
summary(idx.new$TSX.spread)
summary(idx.new$TSX.fracspread)
boxplot(as.matrix(data.frame(idx.new$HSI.spread,idx.new$TSX.spread)), col = "orange")
boxplot(as.matrix(data.frame(idx.new$HSI.fracspread,idx.new$TSX.fracspread)), col = "orange")


# FX Rates
fx.tickers <- c("BITFINEX/BTCUSD.5","BITFINEX/BTCUSD.6")
fx.raw <- Quandl(fx.tickers, type="raw")
fx.colnames <- c("Date", "Bit.bid", "Bit.ask")
colnames(fx.raw) <- fx.colnames

# Filter out missing bid/ask prices
fx.new <- subset(fx.raw, `Bit.bid`>0&`Bit.ask`>0,
                 select = c(`Date`, `Bit.bid`, `Bit.ask`))
# Spreads
fx.new$Bit.spread <- fx.new$`Bit.ask`- fx.new$`Bit.bid`
fx.new$Bit.fracspread <- log(fx.new$`Bit.ask`) - log(fx.new$`Bit.bid`)
fx.all <- fx.new

summary(fx.all$Bit.spread)
summary(fx.all$Bit.fracspread)
boxplot(as.matrix(data.frame(fx.all$Bit.spread)), col = "orange")
boxplot(as.matrix(data.frame(fx.all$Bit.fracspread)), col = "orange")


# Equity

shortvol.tickers <- c("FINRA/FNSQ_AMD.1", "FINRA/FNSQ_RGR.1", "FINRA/FNSQ_TSLA.1")  
shortvol.raw <- Quandl(shortvol.tickers, type="xts")
colnames(shortvol.raw) <- c("AMD.shortvol", "RGR.shortvol", "TSLA.shortvol")

eq.tickers <- c("AMD", "RGR", "TSLA")
tmp <- getSymbols(eq.tickers[1], src="yahoo", env=NULL)
adj.col <- last(colnames(tmp))
eq.raw <- tmp[,adj.col]                       # What does this line and the for-loop do? I think this line adds the adj close from yahoo to eq.raw?
for (j in 2:length(eq.tickers))               # Not sure about this for-loop, how is this different than lines 50-52?
{
  tmp <- getSymbols(eq.tickers[j], src="yahoo", env=NULL)
  adj.col <- last(colnames(tmp))
  eq.raw <- cbind(eq.raw, tmp[,adj.col])
}
mix.raw <- cbind(eq.raw, shortvol.raw)
mix.new <- subset(mix.raw, `AMD.shortvol`>0&`RGR.shortvol`>0&`TSLA.shortvol`>0&`TSLA.Adjusted`>0&`AMD.Adjusted`>0&`RGR.Adjusted`>0,
                 select = c(`AMD.shortvol`,`AMD.Adjusted`,`RGR.shortvol`,`RGR.Adjusted`,`TSLA.shortvol`,`TSLA.Adjusted`))


fi.all <- subset(fi.all, Date > as.Date("2015-01-01") )
sp <- c()
ye <- c()
supp<-c()

for (v in fi.all[,1]) {
  ye <- as.Date( c(ye,v))
}
for (v in fi.all[,1]) {
   ye <-as.Date( c(ye,v))
}
for (v in fi.all[,4]) {
  sp <- c(sp,v)
}
for (v in fi.all[,8]) {
  sp <- c(sp,v)
}
for (v in fi.all[,4]) {
  supp <- c(supp,"CGF1")
}
for (v in fi.all[,4]) {
  supp <- c(supp,"CGB1")
}

df <- data.frame(spread=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=spread, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

sp <- c()
for (v in fi.all[,5]) {
  sp <- c(sp,v)
}
for (v in fi.all[,9]) {
  sp <- c(sp,v)
}
df <- data.frame(frac=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=frac, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")






commodities.all <- subset(commodities.all, Date > as.Date("2015-01-01") )
sp <- c()
ye <- c()
supp<-c()

for (v in commodities.all[,1]) {
  ye <- as.Date( c(ye,v))
}
for (v in commodities.all[,1]) {
  ye <-as.Date( c(ye,v))
}
for (v in commodities.all[,1]) {
  ye <-as.Date( c(ye,v))
}
for (v in commodities.all[,4]) {
  sp <- c(sp,v)
}
for (v in commodities.all[,8]) {
  sp <- c(sp,v)
}
for (v in commodities.all[,12]) {
  sp <- c(sp,v)
}
for (v in commodities.all[,4]) {
  supp <- c(supp,"CU")
}
for (v in commodities.all[,4]) {
  supp <- c(supp,"AL")
}
for (v in commodities.all[,4]) {
  supp <- c(supp,"TN")
}

df <- data.frame(spread=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=spread, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

sp <- c()
for (v in commodities.all[,5]) {
  sp <- c(sp,v)
}
for (v in commodities.all[,9]) {
  sp <- c(sp,v)
}
for (v in commodities.all[,13]) {
  sp <- c(sp,v)
}
df <- data.frame(frac=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=frac, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

#+ scale_color_gradientn(colours = rainbow(3))
  # once dataframes are loaded, use the cbind function to combine/bind this with shortvol.raw 
  # (maybe create a new array entirely?)
  # Syntax: eq.new1 <- cbind(shortvol.raw, adj.close[, ????????])
  




idx.all <- subset(idx.all, Date > as.Date("2015-01-01") )
sp <- c()
ye <- c()
supp<-c()

for (v in idx.all[,1]) {
  ye <- as.Date( c(ye,v))
}
for (v in idx.all[,1]) {
  ye <-as.Date( c(ye,v))
}
for (v in idx.all[,4]) {
  sp <- c(sp,v)
}
for (v in idx.all[,8]) {
  sp <- c(sp,v)
}
for (v in idx.all[,4]) {
  supp <- c(supp,"HSI")
}
for (v in idx.all[,4]) {
  supp <- c(supp,"TSX 60")
}

df <- data.frame(spread=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=spread, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

sp <- c()
for (v in idx.all[,5]) {
  sp <- c(sp,v)
}
for (v in idx.all[,9]) {
  sp <- c(sp,v)
}
df <- data.frame(frac=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=frac, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")







fx.all <- subset(fx.all, Date > as.Date("2015-01-01") )
sp <- c()
ye <- c()
supp<-c()

for (v in fx.all[,1]) {
  ye <- as.Date( c(ye,v))
}
for (v in fx.all[,4]) {
  sp <- c(sp,v)
}
for (v in fx.all[,4]) {
  supp <- c(supp,"Bit")
}

df <- data.frame(spread=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=spread, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

sp <- c()
for (v in fx.all[,5]) {
  sp <- c(sp,v)
}

df <- data.frame(frac=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=frac, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")






mix.new <- mix.new["2015/"]
#mix.new <- subset(mix.new, Date > as.Date("2015-01-01") )
sp <- c()
ye <- c()
supp<-c()
for (v in index(mix.new)) {
  ye <- as.Date( c(ye,v))
  supp <- c(supp,"AMD")
}
for (v in index(mix.new)) {
  ye <- as.Date( c(ye,v))
  supp <- c(supp,"RGR")
}
for (v in index(mix.new)) {
  ye <- as.Date( c(ye,v))
  supp <- c(supp,"TSLA")
  
}
for (v in mix.new[,2]) {
  sp <- c(sp,v)
}
for (v in mix.new[,4]) {
  sp <- c(sp,v)
}
for (v in mix.new[,6]) {
  sp <- c(sp,v)
}


df <- data.frame(Adjusted=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Adjusted, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

sp <- c()
for (v in mix.new[,1]) {
  sp <- c(sp,v)
}
for (v in mix.new[,3]) {
  sp <- c(sp,v)
}
for (v in mix.new[,5]) {
  sp <- c(sp,v)
}

df <- data.frame(shortv=sp,
                 date=ye,type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=shortv, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1")

sp <- c()
ye <- c()
supp<-c()
for (v in index(mix.new)) {
  ye <- as.Date(c(ye,v))
  supp <- c(supp,"Adj")
}
for (v in index(mix.new)) {
  ye <-as.Date( c(ye,v))
  supp <- c(supp,"short")
}
for (v in mix.new[,2]) {
  sp <- c(sp,v)
}
for (v in mix.new[,1]) {
  sp <- c(sp,v/1000000)
}


df <- data.frame(Adjusted=sp,
                 year=ye,type=supp)

# Change the color
p<-ggplot(df, aes(y=Adjusted, x=year, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1") 

sp <- c()
ye <- c()
supp<-c()
for (v in index(mix.new)) {
  ye <- as.Date(c(ye,v))
  supp <- c(supp,"Adj")
}
for (v in index(mix.new)) {
  ye <-as.Date( c(ye,v))
  supp <- c(supp,"short")
}
for (v in mix.new[,4]) {
  sp <- c(sp,v)
}
for (v in mix.new[,3]) {
  sp <- c(sp,v/1000)
}


df <- data.frame(Adjusted=sp,
                 year=ye,type=supp)

# Change the color
p<-ggplot(df, aes(y=Adjusted, x=year, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1") 


sp <- c()
ye <- c()
supp<-c()
for (v in index(mix.new)) {
  ye <- as.Date(c(ye,v))
  supp <- c(supp,"Adj")
}
for (v in index(mix.new)) {
  ye <-as.Date( c(ye,v))
  supp <- c(supp,"short")
}
for (v in mix.new[,6]) {
  sp <- c(sp,v)
}
for (v in mix.new[,5]) {
  sp <- c(sp,v/5000)
}


df <- data.frame(Adjusted=sp,
                 year=ye,type=supp)

# Change the color
p<-ggplot(df, aes(y=Adjusted, x=year, color=type,group=type)) +
  geom_line()
p + scale_color_brewer(palette = "Set1") 
  # alldata.full <- cbind(fi.raw, commodities.raw, eqidx.raw, fx.raw, eq.raw)
  # alldata <- alldata.full["20140101/20180515"]
  # alldata <- alldata.full["20170101/20180101"]
  # summary(alldata)

