library(datasets)
head(iris)
summary(iris)
plot(iris)

detach('package:datasets',unload=TRUE)

dev.off()

cat('\014')

library('pacman')

pacman::p_load(pacman, dplyr)

p_unload(dplyr, tidyr, stringr)
cat('\014')



library(datasets)
head(iris)

plot(iris$Species)
plot(iris$Petal.Length)
plot(iris$Petal.Length)
plot(iris$Species, iris$Sepal.Width)
plot(iris$Petal.Length,iris$Sepal.Width)
plot(iris)

plot(iris$Petal.Length, iris$Petal.Width,
     col = '#cc0000',
     pch=19, 
     main='title')

plot(cos, 0,2*pi)
plot(exp,1,5)
plot(dnorm,-3,+3)
plot(sin,0,2*pi)
plot(tan, 0, pi/2)
plot(dpois(1:10, lambda = .1))

plot(dnorm, -3,3,
     lwd=10,
     col='red')

######Bar charts---most basic graphic, used to constrast variables; used for categorical variables
library(datasets)

?mtcars
head(mtcars)

barplot(mtcars$cyl)
cylinders <- table(mtcars$cyl)
plot(cylinders)

#####histogram----used to show the distribution of values; used for numerical variables
library(datasets)
head(iris)

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

######histogram by groups######

# put the graph in 3 rows and 1 column


hist(iris$Petal.Width[iris$Species=='setosa'],
     xlim=c(0,3),
     breaks = 9,
     main='Petal Width for Setosa',
     col='green')

par(mfrow=c(3,1))

hist(iris$Petal.Length[iris$Species=='setosa'],
     breaks = 9,
     col='black')

hist(iris$Petal.Length[iris$Species=='versicolor'],
     breaks = 9,
     col='blue')

hist(iris$Petal.Length[iris$Species=='virginica'],
     breaks = 9,
     col='purple')


#########Scatterplot---visualizing the association between two numerical variables
library(datasets)

head(mtcars)
# good to first check univariate distribution
hist(mtcars$wt)
hist(mtcars$mpg)

# basic x-y plot for 2 quantative variables
plot(mtcars$wt, mtcars$mpg,
     pch=19,
     cex=1.5,
     col='#cc0000')

######### overlay information---increased information density
?lynx
head(lynx)

hist(lynx,
     breaks = 14,
     freq=FALSE,##show density
     col='pink')

# add a normal distribution
curve(dnorm(x,mean=mean(lynx),sd=sd(lynx)),
            col= 'black',
            lwd = 2,
            add = TRUE)

# add more 
lines(density(lynx), col='blue',lwd=2)

lines(density(lynx, adjust=3), col='red')
      
# Add a rug plot
rug(lynx, col='grey')
?rug

# summary()

head(iris)
summary(iris)
summary(iris$Species) # for categorical variables
summary(iris$Sepal.Length) # for numerical variables

# describe()---more detailed information of data

p_load(psyche)

p_help(psych)
p_help(psych, web=F)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, ramarkdown, shiny,
               stringr, tidyr)
describe(iris$Sepal.Length)
describe(iris)
