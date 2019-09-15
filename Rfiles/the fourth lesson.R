#advanced data management
trunc(9.22)
round(9.24, digit=1)
round(3.54545, digit = 2)
sqrt(23)
celling(20.23)
floor(232.5)
acos(0.433)
log(3,base = 3)
log(8)
exp(23)
abs(-23)

y <- quantile(0.2, c(.3,.84)) 
x <- c(1,2,3,4)
sum(c(1,2,3,4))
x<- c(1, 5, 23, 29) 
diff(x)
min(c(1,2,3,4))
sd(x)
median(x)
mean(x)
var(x)

#use the funtion pretty(c(begining, end), number of generated number)
x <- pretty(c(-3,3),30)
y <- dnorm(x)
plot(x,y,
     type = "l",
     xlab = "Normal Deviate",
     ylab = "Density",
     yaxs = 'i'
     )
z <- t(x)
z

#generate distributions
x <- rnorm(100, mean = 5, sd = 2)

#introduction of dnorm, rnorm, qnorm and rnorm
#http://seankross.com/notes/dpqr/
#rnorm is used to generate normally distributed numbers;dnorm is used to attain the f(x); 
#pnorm is to return the number correspondent to the dnorm
test <- dnorm(0, mean = 0, sd = 1)
pnorm(test)
dnorm(0, mean = 0 ,sd = 1)
rnorm(5)

