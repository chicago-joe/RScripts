## Dale W.R. Rosenthal, 2018
## You are free to distribute and use this code so long as you attribute
## it to me or cite the text.
## The legal disclaimer in _A Quantitative Primer on Investments with R_
## applies to this code.  Use or distribution without these comment lines
## is forbidden.
# The fields in an 11Ac1-5 execution quality report
library(ggplot2)
library(RColorBrewer)
field.names <- c("Participant", "MktCenter", "CCYYMM", "Symbol", "Immediacy",
                 "Size", "NumCoveredOrds", "ShrsCoveredOrds", "CxldShrsCoveredOrds",
                 "FillShrsCoveredOrds", "AwayShrsCoveredOrds", "Shrs0to9s",
                 "Shrs10to29s", "Shrs30to59s", "Shrs60to299s", "Shrs5to30m",
                 "AvgRealizedSpread", "AvgEffectiveSpread", "ShrsPriceImprove",
                 "ShrWtdPriceImproved", "ShrWtdPriceImproveTime",
                 "ShrsFilledAtQuote", "ShrWtdAtQuoteTime", "ShrsFilledOutsideQuote",
                 "ShrWtdOutsideQuotePriceDiff", "ShrWtdOutsideQuoteTime")

# These give the meaning of coded fields in the report
field.participants <- list(A="Amex", B="BSE", M="CHX", C="CSE", T="NASD",
                           N="NYSE", P="PCX", X="Phlx")
field.sizes <- list("21"="100-499", "22"="500-1999", "23"="2000-4999", "24"="5000+")
field.immediacy <- list("11"="Market", "12"="Marketable limit", "13"="Inside limit",
                        "14"="At-quote limit", "15"="Near-quote limit")



files2grab <- c("M200204.dat",
                "M200304.dat",
                "M200404.dat",
                "M200504.dat",
                "M200604.dat",
                "M200704.dat",
                "M200804.dat",
                "M200904.dat",
                "M201004.dat",
                "M201104.dat",
                "M201204.dat",
                "M201304.dat",
                "M201404.dat",
                "M201504.dat",
                "M201604.dat",
                "M201704.dat",
                "M201804.dat")

sp <- c()
ye <- c()
supp<-c()
for (zipfile in files2grab) {
  
  exdata<-c()
  exdata <- read.csv(zipfile, header=FALSE, sep="|")
  
  colnames(exdata) <- field.names
  newdata <-subset(exdata, !is.na(`AvgRealizedSpread`))
  #unlink(temp)
  a<-min(newdata$Size)
  b<-max(newdata$Size)
  # do some calculations across subgroups and store results
  # You can subset the data like so:
  #marketableorders.idx <- exdata$Immediacy == 11 | exdata$Immediacy == 12
  #mean(exdata$AvgRealizedSpread[marketableorders.idx])
  
  
  for (i in a:b) {
    sp<-c(sp,mean(newdata$AvgRealizedSpread[newdata$Size == i]))
    supp <-c(supp,i)
    ye<-c(ye,mean(newdata$CCYYMM))
  }
}

#exdata <- read.csv("M200204.dat", header=FALSE, sep="|")
#colnames(exdata) <- field.names
#unlink(temp)
# do some calculations across subgroups and store results
# You can subset the data like so:


df <- data.frame(AvgRealizedSpread=sp,
                 date=ye,size=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=AvgRealizedSpread, color=size,group=size)) +
  geom_line()+
  geom_point()
p+ scale_color_gradientn(colours = rainbow(3))
#p+scale_color_manual(values=c("green"))
# x.axis: Months (in order)...(Jan - Feb - Mar - Apr.)
# y.axis: Metric (avg.spread, avg.effspread, share.w_avg.time - 1 per plot!!)
# Create your groups of size (group 1, 2, 3, 4, 5) and use ggplot2-----
# Use ggplot2 "Legends" function to plot the different groups for each metric plot
#p <- ggplot(newdata, aes(`Size`, `AvgRealizedSpread`)) + geom_point(aes(colour = color))p + guides(col = guide_legend(nrow = 8))


sp <- c()
ye <- c()
supp<-c()
for (zipfile in files2grab) {

  
  exdata <- read.csv(zipfile, header=FALSE, sep="|")
 
  colnames(exdata) <- field.names
  newdata <-subset(exdata, !is.na(`AvgEffectiveSpread`))
  #unlink(temp)
  a<-min(newdata$Size)
  b<-max(newdata$Size)
  # do some calculations across subgroups and store results
  # You can subset the data like so:
  #marketableorders.idx <- exdata$Immediacy == 11 | exdata$Immediacy == 12
  #mean(exdata$AvgRealizedSpread[marketableorders.idx])
  
  
  for (i in a:b) {
    sp<-c(sp,mean(newdata$AvgEffectiveSpread[newdata$Size == i]))
    supp <-c(supp,i)
    ye<-c(ye,mean(newdata$CCYYMM))
  }
}

#exdata <- read.csv("M200204.dat", header=FALSE, sep="|")
#colnames(exdata) <- field.names
#unlink(temp)
# do some calculations across subgroups and store results
# You can subset the data like so:


df <- data.frame(AvgEffectiveSpread=sp,
                 date=ye,size=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=AvgEffectiveSpread, color=size,group=size)) +
  geom_line()+
  geom_point()
p+ scale_color_gradientn(colours = rainbow(3))
#p+scale_color_manual(values=c("green"))
# x.axis: Months (in order)...(Jan - Feb - Mar - Apr.)
# y.axis: Metric (avg.spread, avg.effspread, share.w_avg.time - 1 per plot!!)
# Create your groups of size (group 1, 2, 3, 4, 5) and use ggplot2-----
# Use ggplot2 "Legends" function to plot the different groups for each metric plot
#p <- ggplot(newdata, aes(`Size`, `AvgRealizedSpread`)) + geom_point(aes(colour = color))p + guides(col = guide_legend(nrow = 8))

sp <- c()
ye <- c()
supp<-c()
for (zipfile in files2grab) {
  
  
  exdata <- read.csv(zipfile, header=FALSE, sep="|")
  
  colnames(exdata) <- field.names
  newdata <-subset(exdata, !is.na(`ShrWtdAtQuoteTime`))
  #unlink(temp)
  a<-min(newdata$Size)
  b<-max(newdata$Size)
  # do some calculations across subgroups and store results
  # You can subset the data like so:
  #marketableorders.idx <- exdata$Immediacy == 11 | exdata$Immediacy == 12
  #mean(exdata$AvgRealizedSpread[marketableorders.idx])
  
  
  for (i in a:b) {
    sp<-c(sp,mean(newdata$ShrWtdAtQuoteTime[newdata$Size == i]))
    supp <-c(supp,i)
    ye<-c(ye,mean(newdata$CCYYMM))
  }
}

#exdata <- read.csv("M200204.dat", header=FALSE, sep="|")
#colnames(exdata) <- field.names
#unlink(temp)
# do some calculations across subgroups and store results
# You can subset the data like so:


df <- data.frame(ShrWtdAtQuoteTime=sp,
                 date=ye,size=supp)

# Change the color
p<-ggplot(df, aes(x=date, y=ShrWtdAtQuoteTime, color=size,group=size)) +
  geom_line()+
  geom_point()
p+ scale_color_gradientn(colours = rainbow(3))
#p+scale_color_manual(values=c("green"))
# x.axis: Months (in order)...(Jan - Feb - Mar - Apr.)
# y.axis: Metric (avg.spread, avg.effspread, share.w_avg.time - 1 per plot!!)
# Create your groups of size (group 1, 2, 3, 4, 5) and use ggplot2-----
# Use ggplot2 "Legends" function to plot the different groups for each metric plot
#p <- ggplot(newdata, aes(`Size`, `AvgRealizedSpread`)) + geom_point(aes(colour = color))p + guides(col = guide_legend(nrow = 8))

