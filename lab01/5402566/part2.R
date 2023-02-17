n<-1000000
#1
m<-0
p<-0
for (i in 1:n) 
{
  x<-sample(c(1:10),5,replace=TRUE)
  if (length(x[1:4][x[1:4]<=9]) + length(x[5][x[5]>5])>=3) 
    m<- m+1
  j<-sample(1:5,1)
  if ((j==5 & x[j]>5)|(j<5&x[j]<=9))
    p<-p+1
}
print(m/n)
print(p/n)

#2
m<-0
for (i in 1:n)
{
  x<-sample(c(1:100),3,replace=TRUE)
  if ((length(x[1][x[1]<=90]) + length(x[2][x[2]<=80]) + length(x[3][x[3]<=85])) == 2)
    m<-m+1
}
print(m/n)

#3
m<-0
for (i in 1:n)
{
  x<-sample(c(1:100),10,replace=TRUE)
  if (length(x[x<=33]) == 3)
    m<-m+1
}
print(m/n)

#4
m<-0
for (i in 1:n)
{
  x<-sample(c(1:100),5,replace=TRUE)
  if (length(x[1:5][x[1:5]<=51]) == 3)
    m<-m+1
}
print(m/n)

#5
m<-0
for (i in 1:n)
{
  x<-sample(c(1:10),2,replace=TRUE)
  if ((x[1]<=7 & x[2]<=5)|(x[1]>7 & x[2] <=4))
    m<-m+1
}
print(m/n)