# Ch22
df = read.table(file="C:\\Users\\jloss\\Desktop\\hw3.csv", header = TRUE,sep=",")
# par[1]is p, par[2]is L
min.RSS <- function(data, par) {
  with(df, sum(((((1-par[1])^T+(1-par[2]))/(1+Rf)^T)-B/1000))^2)
}
result <- optim(par = c(0, 0), fn = min.RSS,lower=c(0, 0), upper=c(1, 1), data = df,method="L-BFGS-B")
result$par
result$value