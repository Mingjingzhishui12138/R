library(grid)
library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

attach(mtcars)
boxplot(mpg, main = 'Box plot', xlab = "Cars", ylab = "Miles per Gallon")

mpg <- mtcars$mpg
mpg[1,1] = 50
mpg[2,1] = 45
mpg[32,1] = 0


#groupplot 为什么有三个箱子
mpg <- mtcars$mpg
cyl <- mtcars$cyl
boxplot(mpg~cyl, data = mtcars, main = "Car Mileage Data",
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon",
        col = c("white",'red','yellow'))


hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 12, col = "red",
     xlab = "Miles Per Gallon",
     main = "Colored histogram with 12 bins")

#density plot
plot(density(mtcars$mpg))

d <- density(mtcars$mpg)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col = 'red', border = "blue")
rug(mtcars$mpg, col = "brown")


barplot(counts,
       main = "Stacked Bar Plot",
       xlab = "Treatment",ylab = "Frequency",
       col = c('red','yellow','green'))
barplot(counts,
        main = "Grouped Bar Plot",
        xlab= "Treatment",ylab = "Frequency",
        col = c("red",'yellow','green'),
        legend = rownames(counts),beside = TRUE
        )

boxplot(mtcars$mpg,main = "Box plot",ylab = "miles per Gallon")

#box plot by group 
boxplot(mpg ~ cyl, data = mtcars,
        notch = TRUE,
        col = 'red',
        main = "Car Mileage Data",
        xlab = "Number pf Cylinders",
        ylab = "Miles Per Gallon")

par(mfrow = c(2,2))
hist(mtcars$mpg)

hist(mtcars$mpg,
     breaks =12,
     col = 'red',
     xlab = 'MIles Per Gallon',
     main = "Colored histogram with 12 bins"
)
#jitter 的意思是抖动。rug那一行效果是下面的图多了一行东西
hist(mtcars$mpg,
     freq = FALSE,
     breaks = 12,
     col = "red",
     xlab = "Miles per Gallon",
     main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg),col = "Blue", lwd = 2)

#bar plot
cyl<- mtcars$cyl
cyl
table(cyl)
barplot(cyl,
        main = "This is a main",
        xlab = "This is an xlab",
        ylab = "This is a ylab",
        #rug(jitter(cyl))
        )

# how to plot horizonal barplot
barplot(table(cyl),horiz = TRUE,
        main = "Simple bar plot",
        xlab = 'number of cylinders',
        ylab = 'Frequency')

x <- mtcars$mpg
h<- hist(x,
         breaks =12,
         col = 'red',
         xlab = "Miles per Gallon",
         main = "Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2]*length(x))
lines(xfit, yfit, col = 'blue',lwd = 2)
box()

plot(density(mtcars$mpg))         

lines(density(mtcars$mpg))
 
par(mfrow=c(2,1))
d <- density(mtcars$mpg)

plot(d)

#plot density function
d <- density(mtcars$mpg)
plot(d,main="Kernal Density of Miles Per Gallon")
polygon(d, col="red",border = 'brown')


#add density plot to histogram
#jitter: Add a small amount of noise to a numeric vector.
hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red",
     xlab = "Miles per gallon",
     main = "HIstogram, rug, plot, density curve")
     rug(jitter(mtcars$mpg))
     lines(density(mtcars$mpg), col = "blue",lwd =2)
