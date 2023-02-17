install.packages("zoo")
library(zoo)
install.packages("readxl")
library(readxl)
d<-read_excel("data.xlsx")
View(d)
f<-read.csv("https://raw.githubusercontent.com/junaart/ForStudents/master/R/Lesson_4/HairEyeColor.csv")
View(f)
save(d,f,file="xxx")
load("xxx")
d$`Работающие активы`
y<-c(4,5,8,9,7,67,1,4)
y[(y>7) & 1,2,3]
mean(y)
median(y)
y[y>mean(y)]
x<-c(2,3)
prod(x)
q<-c(1:1000)
r<-sample(c(1:1000),1000)
q+r
q*r
q^r
sqrt(sum((r-mean(r))^2)/(length(r)-1))
sd(r)

u<-runif(500,4,90)
u

p<-rnorm(50,6,2)
p
