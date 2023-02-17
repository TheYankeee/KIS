install.packages("psych")
library(psych)

x<-runif(1000,1,1000000)
y<-c(1:500)
median(x)
mean(y)-harmonic.mean(y)
x[(x>=5)&(x<7000)]
y[y%%2==0]
save(x,y,file="xy")
