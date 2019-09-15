# Part 1
# 1.	An IT company located in Shenzhen consists of employees from different provinces: 80 from Guangdong, 45 from Hunan, 35 from Hubei, 20 from Zhejiang, 18 from Fujian, and 2 from Xinjiang. Draw a pie chart to show the percentage of the employees in these provinces. 
# 2.	Let X denote the ages of the above employees and Y be their salaries. The salary generally depends on the employee¡¯s age. For those young employees at the age between 22 and 30, their salaries follow the normal distribution N(8000, 10002); for those at the age between 31 and 45, their salaries follow N(15000, 20002); and for those at the age between 46 to 60, their salaries follow N(20000, 15002). The group sizes are 90, 70 and 40, respectively. In each group, the ages of the employees are random samples uniformly drawn from the age range. Draw a scatter plot to show the association relationship between age and salary, with different colors representing different groups. 
# 3.	Generate three samples of 100,000 observations from t distribution with degrees of freedom 5, 10 and 30, respectively. Compare the (estimated) density plots of these samples with the standard normal density function in one plot. 
# Make sure your plots have appropriate titles, axes labels, legends, and other annotations that you think are necessary.
# 
# Part 2
# 1.	Use the ¡°mtcars¡± data set to conduct the hypothesis testing that the mean of mpg (miles per gallon) is larger than 15.
# 2.	Use the ¡°mtcars¡± data set to test whether the means of wt (weight) of the cars are different in the automatic and manual (am, 0 = automatic, 1 = manual) cases.
# 3.	The ¡°sleep¡± data set shows the effect of two soporific drugs (increase in hours of sleep compared to control) on 10 patients. Use hypothesis testing to show whether these two drugs have significant difference.

# 1. employee distribution

slices <- c(45, 35, 20, 18, 2)  
lbls <- c("Hunan", "Hubei", "Zhejiang", "Fujian", "Xinjiang") 
pie(slices, labels = lbls,     
    main="Employee distributions") 

pct <- round(slices/sum(slices)*100) 
lbls2 <- paste(lbls, " ", pct, "%", sep="") 
pie(slices, labels=lbls2, col=rainbow(length(lbls2)),      
    main="Employee Distributions")

# 2. Relationships between ages and wages

ages1 <- runif(90,22,30)
ages2 <- runif(70,31,45 )
ages3 <- runif(40,46,60)
Ages <- c(ages1,ages2,ages3)

wages1 <- rnorm(90,mean=8000,sd=1000)
wages2 <- rnorm(70,mean=15000,sd=2000)
wages3 <- rnorm(40,mean=20000,sd=1500)
Wages <- c(wages1,wages2, wages3)

marker1 <- rep('22~30 years old',times = 90)
marker2 <- rep('21~45 years old',times = 70)
marker3 <- rep('46~60 years old', times = 40)
Groups <- c(marker1, marker2, marker3)

  
mydataframe <- data.frame(z, x,y, stringsAsFactors = FALSE)
mydataframe
mydataframe$z <- factor(mydataframe$z)

library(ggplot2)
ggplot(data=mydataframe, aes(x=Ages, y=Wages,color=Groups, main = "Relationships Between Ages and Wages")) + geom_point(size=3)


plot(ages1, wages1,      
     main="Relationship Between Ages And Wages ",      
     xlab="Ages",      
     ylab="Wages ", pch=19) 
#abline(lm(mpg~wt), col="red", lwd=2, lty=1) 
#lines(lowess(wt,mpg), col="blue", lwd=2, lty=2)

# generating t-distributed samples and plotting density functions
sample1 <- rt(n = 10000, df = 5)
sample2 <- rt(n = 10000, df = 10)
sample3 <- rt(n = 10000, df = 30)
x <- seq(1,10000)
mydataframe1 <- data.frame(x, sample1,sample2,sample3)

sample <- c(sample1, sample2, sample3)
sample <- factor(sample, order=TRUE,levels = c('sample1','sample2','sample3'))
sample.f <- factor(sample, levels <- c(1,2,3), labels = "Sample1", "Sample2","Sample3")
mydataframe1.density.compare(x,sample.f,xlab = 'df')
title(main = 'df')