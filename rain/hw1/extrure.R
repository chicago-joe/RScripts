
r1<- rnorm(10000,0,0.04)
r2<- rnorm(10000,0,0.04)
r3<- rnorm(10000,0,0.04)
r4<- rnorm(10000,0,0.04)
r5<- rnorm(10000,0,0.04)
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
x<- ((1+r1)*(1+r2)*(1+r3)*(1+r4)*(1+r5))^1/5
y<- 1/5*((1+r1)+(1+r2)+(1+r3)+(1+r4)+(1+r5))

summary(x)
summary(y)
for (j in 1:10000) {
  if(x[j]<y[j]){
    
  }else{
    print("geometric > equally-weighted")
  }
}



count<-0
for (i in 1:5000) {
  r1<- rnorm(10000,0,0.04)
  r2<- rnorm(10000,0,0.04)
  r3<- rnorm(10000,0,0.04)
  r4<- rnorm(10000,0,0.04)
  r5<- rnorm(10000,0,0.04)
  
  r1<-mean(r1)
  r2<-mean(r2)
  r3<-mean(r3)
  r4<-mean(r4)
  r5<-mean(r5)
  
  x<- ((1+r1)*(1+r2)*(1+r3)*(1+r4)*(1+r5))^1/5
  
  y<- 1/5*((1+r1)+(1+r2)+(1+r3)+(1+r4)+(1+r5))
  if(x<y){
    
    #print("geometric < equally-weighted")
  }else{
    count<-count+1
    #print("geometric > equally-weighted")
  }
}
print(count/5000)

testdata1 <-read.table("M201804.dat", header=TRUE)
testdata2 <-read.table("M201804.dat", header=TRUE)
testdata3 <-read.table("M201804.dat", header=TRUE)
testdata4 <-read.table("M201804.dat", header=TRUE)
testdata5 <-read.table("M201804.dat", header=TRUE)

set <- c(0 ,0.5 ,5.25,1.5,10.5,2.5,15.75,3.5)
mean(set)
var(set)
sqrt(var(set))
boxplot(set)

# 如果是均匀分布，则没有明显差异 。这里组其实已经分好了，直接用 。H0：人数服从均匀分布
x <- c(21,4,21,4,21,4,21,4)
chisq.test(x)

x <- c(21,4,21,4,21,4,21,4)
n <- sum(x); m <- length(x)
p <- rep(1/m,m)
K <- sum((x-n*p)^2/(n*p)); K #计算出K值
# [1] 136.49

p <- 1-pchisq(K,m-1); p #计算出p值
# [1] 0 #拒绝原假设。
x<-c()
for (v in 0:78251) {
  x<-c(x,0)
  x<-c(x,2/8)
  x<-c(x,4/8)
  x<-c(x,6/8)
}
for (v in 0:14905) {
  x<-c(x,1/8)
  x<-c(x,3/8)
  x<-c(x,5/8)
  x<-c(x,7/8)
}
x<-c(372625*0.21,372625*0.21,372625*0.21,372625*0.21,372625*0.04,372625*0.04,372625*0.04,372625*0.04)
chisq.test(x)
n <- sum(x); m <- length(x)
p <- rep(1/m,m)
K <- sum((x-n*p)^2/(n*p)); K #计算出K值
# [1] 136.49

p <- 1-pchisq(K,m-1); p #计算出p值





