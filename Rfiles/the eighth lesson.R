myvars <- c("mpg", "hp", "wt")
summary(mtcars[myvars])

#mean(), sd(), var(), min(), max(), median(), length(), range(), quantile()

mydata <- data.frame(x=rnorm(20,2,1),y=rnorm(20,3,2))
apply(mydata, 2, sd)
sapply(mydata, sd)#sapply automatically execute by coloumns

#other useful functions:
#describte(); stat.desc(); describe()

myvars <- c("mpg", "hp", "wt")
aggregate(mtcars[myvars], by= list(am = mtcars$am), mean)
aggregate(mtcars[myvars], by = list(am = mtcars$am), mean)
aggregate(mtcars[myvars], by = list(am = mtcars$am), sd)

#tables--- r provides several methods for creating frequency and contingency tables.

#table(var1, car2,...)
library(vcd)
mytable <- with(Arthritis, table(Improved))#针对improved做一个表

mytable

margin.table(mytable, 1)
Treatment
Placebo Tested
#the index 1 refers to the first variable in the table() statements

#mydata <- table(A,B) 建列联表
#mytable <- xtabs(~A+B, data = mydata) 
mydata <-xtabs(-Treatment+Improved, data = Arthristis)

margin.table(mytable, 1)
Treatment
Placebo Treated

#求比例。针对每一行求比例。
prop.table(mytable, 1)

addmargins(prop.table(mytable))


###############practice from books
myvar <- c("mpg", "hp", "wt")
head(mtcars[myvars])#head function return the first part of the statistics

summary(mtcars[myvars])
#apply function
m<-matrix(1:6,2,3)
apply(m,2,sum) 
sapply(m,sum) 

#sapply function---sapply(x, FUN, options) 
sapply(mtcars[myvars], mean)
sapply(mtcars[myvars], sd)
sapply(mtcars[myvars], median)
sapply(mtcars[myvars], max)
sapply(mtcars[myvars], min)
sapply(mtcars[myvars], var)
sapply(mtcars[myvars], quantile)
sapply(mtcars[myvars], range)
sapply(mtcars[myvars], length)
sapply(mtcars[myvars], fivenum)#returns the Tukey's five-number summary

#return statistics data(skewness and kurtosis, etc)
mystats <- function(x, na.omit = FALSE){
  if(na.omit)#na.omit omits objects that has missing values 
      x<- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^3/s^3)/n-3
  return(c(n=n, mean =m, stdev = s, skew = skew, kurtosis = kurt))
}
  
# if you want to omiss missing value, change to na.omiss=TRUE
  
  
  
mystats <- function(x, na.omit = TRUE){
  #na.omit omits objects that has missing values 
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^3/s^3)/n-3
  return(c(n=n, mean =m, stdev = s, skew = skew, kurtosis = kurt))
}
 myvars <- c("mpg","hp","wt")
 sapply(mtcars[myvars], mystats)
 
#introducing Hmisc to demonstrate statistics
 library(lattice)
 library(survival)
 library(Formula)
 library(ggplot2)
library(Hmisc)
 myvars <- c("mpg", "hp", "wt")
 describe(mtcars[myvars])
 
# introducing pastecs to demonstrate statistics  
 #stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
 library(pastecs)
 myvars <- c("mpg", "hp", "wt")
 stat.desc(mtcars[myvars])
 
 #introducing physh to demonstrate statistics
 library(psych)
 myvars <- c("mpg", "hp", "wt")
 describe(mtcars[myvars])
 
 #how to demonstrate statistics by groups
 myvars <- c("mpg", "hp", "wt")
 aggregate(mtcars[myvars], by = list(am = mtcars$am), sd)
  
 
 


