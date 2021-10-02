x1=rnorm(100);x2=runif(100);x3=rnorm(100)
library(tibble)
datas=tibble(X1=x1,X2=x2,X3=x3)
#with(datas, `X1`)[1]
