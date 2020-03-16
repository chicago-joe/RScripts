## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.

#install.packages("xts")
library(zoo)
library(xts)
library(Quandl)
library(ggplot2)
library(RColorBrewer)
library(Metrics)

Quandl.api_key('iVyBuKymy_j_R7Xxze9t')

####ex1

# Grab constant-maturity US Treasuries 3-Month Treasury Constant Maturity Rate; 2-Year Treasury Constant Maturity Rate; 10-Year Treasury Constant Maturity Rate; 30-Year Treasury Constant Maturity Rate E(R)
ust.tickers <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS5", "FRED/DGS10", "FRED/DGS30")
ust <- Quandl(ust.tickers, type="xts")/100
ust.colnames <- c("T3M", "T2Y", "T5Y", "T10Y", "T30Y")
colnames(ust) <- ust.colnames

# Grab inflation-indexed US Treasuries TIPS Yield Curve and Inflation Compensation E(r)
tips.yields <- c("TIPSY02", "TIPSY05", "TIPSY10")
tips <- Quandl("FED/TIPSY", type="xts")[,tips.yields]/100

minerror<-(1.1)
# expected inflation and CPI are only available monthly...
# For expected inflation and CPI, only get the first column of data... like so:
exinfl <- Quandl("FRBC/EXIN", type="xts")[,1]#Expected Inflation E(i)
colnames(exinfl) <- c("EXINFL")
cpi <- Quandl("FRBC/USINFL", type="xts")[,1]
colnames(cpi) <- c("CPI")

# Calculate inflation and surprise by projecting CPI for one year ahead
# The surprise is how much the CPI differed from what was
# projected a year earlier
infl.yoy <- log(cpi) - log(lag(cpi, 12))
colnames(infl.yoy) <- c("INFL.YOY")
infl.mom <- (log(cpi) - log(lag(cpi)))*12
colnames(infl.mom) <- c("INFL.MOM")
excpi <- cpi*(1+exinfl)  # expected CPI in twelve months
colnames(excpi) <- c("excpi")
cpi.surprise <- log(cpi) - log(lag(excpi, 12))  # % CPI surprise
colnames(cpi.surprise) <- c("INFLSURP")

# combine the data and carry monthly observations forward
inflation.tmp <- cbind(ust, tips, infl.yoy, infl.mom, exinfl, cpi, excpi, cpi.surprise)["1999/"]
inflation.data <- na.locf(inflation.tmp)
lambda02best<-((inflation.data[,2]-inflation.data[,6]-inflation.data[,9])/inflation.data[,14]/inflation.data[,14]*minerror)
lambdai02<-((mean(inflation.data[,2],na.rm=T)-mean(inflation.data[,6],na.rm=T)-mean(inflation.data[,9],na.rm=T))/(sd(inflation.data[,11],na.rm=T)*sd(inflation.data[,11],na.rm=T)))
lambda05best<-((inflation.data[,3]-inflation.data[,7]-inflation.data[,9])/inflation.data[,14]/inflation.data[,14]*minerror)
lambdai05<-((mean(inflation.data[,3],na.rm=T)-mean(inflation.data[,7],na.rm=T)-mean(inflation.data[,9],na.rm=T))/(sd(inflation.data[,11],na.rm=T)*sd(inflation.data[,11],na.rm=T)))
lambda10best<-((inflation.data[,4]-inflation.data[,8]-inflation.data[,9])/inflation.data[,14]/inflation.data[,14]*minerror)
lambdai10<-((mean(inflation.data[,4],na.rm=T)-mean(inflation.data[,8],na.rm=T)-mean(inflation.data[,9],na.rm=T))/(sd(inflation.data[,11],na.rm=T)*sd(inflation.data[,11],na.rm=T)))

#lm(inflation.data[,2]~sd(inflation.data[,11],na.rm=T)*sd(inflation.data[,11],na.rm=T))

lambdar02<-((mean(inflation.data[,2],na.rm=T)-mean(inflation.data[,6],na.rm=T)-mean(inflation.data[,9],na.rm=T))/(sd(inflation.data[,14],na.rm=T)*sd(inflation.data[,14],na.rm=T)))
lambdar05<-((mean(inflation.data[,3],na.rm=T)-mean(inflation.data[,7],na.rm=T)-mean(inflation.data[,9],na.rm=T))/(sd(inflation.data[,14],na.rm=T)*sd(inflation.data[,14],na.rm=T)))
lambdar10<-((mean(inflation.data[,4],na.rm=T)-mean(inflation.data[,8],na.rm=T)-mean(inflation.data[,9],na.rm=T))/(sd(inflation.data[,14],na.rm=T)*sd(inflation.data[,14],na.rm=T)))

different<-cbind(
  inflation.data[,2]-inflation.data[,6]-inflation.data[,10],
  inflation.data[,3]-inflation.data[,7]-inflation.data[,10],
  inflation.data[,4]-inflation.data[,8]-inflation.data[,10],
  inflation.data[,2]-inflation.data[,6]-inflation.data[,9],
  inflation.data[,3]-inflation.data[,7]-inflation.data[,9],
  inflation.data[,4]-inflation.data[,8]-inflation.data[,9],
  inflation.data[,2]-inflation.data[,6]-inflation.data[,9]-lambdar02*inflation.data[,14]*inflation.data[,14],
  inflation.data[,3]-inflation.data[,7]-inflation.data[,9]-lambdar05*inflation.data[,14]*inflation.data[,14],
  inflation.data[,4]-inflation.data[,8]-inflation.data[,9]-lambdar10*inflation.data[,14]*inflation.data[,14],
  inflation.data[,2]-inflation.data[,6]-inflation.data[,9]-lambda02best*inflation.data[,14]*inflation.data[,14],
  inflation.data[,3]-inflation.data[,7]-inflation.data[,9]-lambda05best*inflation.data[,14]*inflation.data[,14],
  inflation.data[,4]-inflation.data[,8]-inflation.data[,9]-lambda10best*inflation.data[,14]*inflation.data[,14])

model<-cbind(
             inflation.data[,6]+inflation.data[,10],
             inflation.data[,7]+inflation.data[,10],
             inflation.data[,8]+inflation.data[,10],
             inflation.data[,6]+inflation.data[,9],
             inflation.data[,7]+inflation.data[,9],
             inflation.data[,8]+inflation.data[,9],
             inflation.data[,6]+inflation.data[,9]+lambdar02*inflation.data[,14]*inflation.data[,14],
             inflation.data[,7]+inflation.data[,9]+lambdar05*inflation.data[,14]*inflation.data[,14],
             inflation.data[,8]+inflation.data[,9]+lambdar10*inflation.data[,14]*inflation.data[,14],
             inflation.data[,6]+inflation.data[,9]+lambda02best*inflation.data[,14]*inflation.data[,14],
             inflation.data[,7]+inflation.data[,9]+lambda05best*inflation.data[,14]*inflation.data[,14],
             inflation.data[,8]+inflation.data[,9]+lambda10best*inflation.data[,14]*inflation.data[,14]
             )

colnames(different) <- c("Y02EiEr","Y05EiEr","Y10EiEr","Y02wMOM","Y05wMOM","Y10wMOM","Y02wSP","Y05wSP","Y10wSP","Y02best","Y05best","Y10best")
colnames(model) <- c("Y02EiEr","Y05EiEr","Y10EiEr","Y02wMOM","Y05wMOM","Y10wMOM","Y02wSP","Y05wSP","Y10wSP","Y02best","Y05best","Y10best")
different_2<-different**2
num<-1
for (i in 1:12) {
  print(colnames(different_2)[i])
  #print(i)
  print(mean(different_2[,i],na.rm=T))
  num<-num+1
}
for (i in 2:4) {
  print(colnames(inflation.data)[i])
  print(sd(inflation.data[,i],na.rm=T))
}
print(mean(different_2[,3],na.rm=T))
mean(different_2,na.rm=T)
##graph 02 begin
v <- c()
date <- c()
supp<-c()
num<-1
modeltype<-c("wYOY","wv","wSP")
for (i in index(model)) {
  date<-c(date,as.Date(i))
  v <- c(v,inflation.data[num,2])
  supp<-c(supp,"T2Y")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,1])
  supp<-c(supp,"EiEr")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,4])
  supp<-c(supp,"wMOM")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,7])
  supp<-c(supp,"wSP")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,10])
  supp<-c(supp,"best")
  num<-num+1
}


df <- data.frame(Value=v,
                 date=as.Date(date),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")

##graph 02 end
##graph 05 begin

v <- c()
date <- c()
supp<-c()
num<-1
modeltype<-c("wYOY","wv","wSP")
for (i in index(model)) {
  date<-c(date,as.Date(i))
  v <- c(v,inflation.data[num,3])
  supp<-c(supp,"T5Y")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,2])
  supp<-c(supp,"EiEr")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,5])
  supp<-c(supp,"wMOM")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,8])
  supp<-c(supp,"wSP")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,11])
  supp<-c(supp,"best")
  num<-num+1
}


df <- data.frame(Value=v,
                 date=as.Date(date),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")

##graph 05 end
##graph 10 begin

v <- c()
date <- c()
supp<-c()
num<-1
modeltype<-c("wYOY","wv","wSP")
for (i in index(model)) {
  date<-c(date,as.Date(i))
  v <- c(v,inflation.data[num,4])
  supp<-c(supp,"T10Y")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,3])
  supp<-c(supp,"EiEr")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,6])
  supp<-c(supp,"wMOM")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,9])
  supp<-c(supp,"wSP")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,12])
  supp<-c(supp,"best")
  num<-num+1
}


df <- data.frame(Value=v,
                 date=as.Date(date),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")

##graph 10 end


v <- c()
date <- c()
supp<-c()
num<-1
modeltype<-c("wYOY","wv","wSP")
for (i in index(model)) {
  date<-c(date,as.Date(i))
  v <- c(v,inflation.data[num,2])
  supp<-c(supp,"T2Y")
  #date<-c(date,as.Date(i))
  #v <- c(v,model[num,1])
  #supp<-c(supp,"wYOY")
  #date<-c(date,as.Date(i))
  #v <- c(v,model[num,4])
  #supp<-c(supp,"wv")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,7])
  supp<-c(supp,"wSP")
  num<-num+1
}


df <- data.frame(Value=v,
                 date=as.Date(date),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")


v <- c()
date <- c()
supp<-c()
num<-1
modeltype<-c("wYOY","wv","wSP")
for (i in index(model)) {
  date<-c(date,as.Date(i))
  v <- c(v,inflation.data[num,3])
  supp<-c(supp,"T5Y")
  #date<-c(date,as.Date(i))
  #v <- c(v,model[num,2])
  #supp<-c(supp,"wYOY")
  #date<-c(date,as.Date(i))
  #v <- c(v,model[num,5])
  #supp<-c(supp,"wv")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,8])
  supp<-c(supp,"wSP")
  num<-num+1
}


df <- data.frame(Value=v,
                 date=as.Date(date),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")


v <- c()
date <- c()
supp<-c()
num<-1
modeltype<-c("wYOY","wv","wSP")
for (i in index(model)) {
  date<-c(date,as.Date(i))
  v <- c(v,inflation.data[num,4])
  supp<-c(supp,"T10Y")
  #date<-c(date,as.Date(i))
  #v <- c(v,model[num,3])
  #supp<-c(supp,"wYOY")
  #date<-c(date,as.Date(i))
  #v <- c(v,model[num,6])
  #supp<-c(supp,"wv")
  date<-c(date,as.Date(i))
  v <- c(v,model[num,9])
  supp<-c(supp,"wSP")
  num<-num+1
}


df <- data.frame(Value=v,
                 date=as.Date(date),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")


####ex2

# backward Hodrick-Prescott filter function
hpbackfilter <- function(y, lambda) {
    n <- length(y)
    I <- diag(1, nrow = n)
    # build the curvature matrix
    K <- matrix(0, nrow=n-2, ncol=n)
    for (i in 1:(n-2)) {
        K[i,i:(i+2)] = c(1,-2,1)
    }
    # now invert and multiply by the data
    hat.matrix <- solve(I+2*lambda*t(K)%*%K)
    hat.matrix %*% y
}
lambda.monthly <- 129600 # for monthly data
tau <- hpbackfilter(cpi, lambda.monthly)
tauchange<-tau
for(i in 2:length(tau)){
  tauchange[i]<-(tau[i]-tau[i-1])/tau[i-1]
}
tauchange[1]=0
taulog<-log(diff(tau))
v <- c()
date <- c()
supp<-c()
error<-c()
num<-1
for (i in index(cpi)) {
  #print(index(cpi)[1])
  #date<-c(date,as.Date(index(cpi)[1],"%b%Y"))
  v <- c(v,tau[num])
  supp<-c(supp,"tau")
  #date<-c(date,as.Date(i,"%Y.%d%m"))
  #v <- c(v,cpi[num])
  #supp<-c(supp,"cpi")
  error<-c(error,tau[num]-cpi[num])
  num<-num+1
}
dates <- seq(as.Date("1947-01-01"),length=860,by="months")
date<-c(date,dates)
num<-1
for (i in index(cpi)) {
  v <- c(v,cpi[num])
  supp<-c(supp,"cpi")
  num<-num+1
}
dates<-c(dates,dates)


df <- data.frame(Value=v,
                 date=as.Date(dates,"%Y-%m-%d"),type=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=Value, color=type,group=type)) +
  geom_line()
#p+ scale_color_gradientn(colours = rainbow(3))
p + scale_color_brewer(palette = "Set1")

sqerror<-error**2
mean(error)
mean(sqerror)
