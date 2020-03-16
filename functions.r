> head(prices[1:(n-1), 1])
           USDEUR.spot
2014-01-01     0.72754
2014-01-02     0.72639
2014-01-03     0.73171
2014-01-05     0.73551
2014-01-06     0.73551
2014-01-07     0.73357
> head(prices[1:(n-1), 1]+1)
           USDEUR.spot
2014-01-01     1.72754
2014-01-02     1.72639
2014-01-03     1.73171
2014-01-05     1.73551
2014-01-06     1.73551
2014-01-07     1.73357
> head(prices[1:(n-1), 1]+1)*head(prices[2:n, ])
Error in `*.default`(head(prices[1:(n - 1), 1] + 1), head(prices[2:n,  : 
  non-conformable arrays
> head(prices[1:(n-1), 1]+1)*head(prices[2:n, 1])
           USDEUR.spot
2014-01-02    1.254032
2014-01-03    1.267110
2014-01-05    1.276485
2014-01-06    1.276485
2014-01-07    1.271695
> 




write.table((commodity_eff.frontier$frontier[optimal.port.name,(1:n.assets)+3]),file="clipboard",sep=",")
