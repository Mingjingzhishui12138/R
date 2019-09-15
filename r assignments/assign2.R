# Part 1
# 1.	An IT company located in Shenzhen consists of employees from different provinces: 80 from Guangdong, 45 from Hunan, 35 from Hubei, 20 from Zhejiang, 18 from Fujian, and 2 from Xinjiang. Draw a pie chart to show the percentage of the employees in these provinces. 
# 2.	Let X denote the ages of the above employees and Y be their salaries. The salary generally depends on the employee’s age. For those young employees at the age between 22 and 30, their salaries follow the normal distribution N(8000, 10002); for those at the age between 31 and 45, their salaries follow N(15000, 20002); and for those at the age between 46 to 60, their salaries follow N(20000, 15002). The group sizes are 90, 70 and 40, respectively. In each group, the ages of the employees are random samples uniformly drawn from the age range. Draw a scatter plot to show the association relationship between age and salary, with different colors representing different groups. 
# 3.	Generate three samples of 100,000 observations from t distribution with degrees of freedom 5, 10 and 30, respectively. Compare the (estimated) density plots of these samples with the standard normal density function in one plot. 
# Make sure your plots have appropriate titles, axes labels, legends, and other annotations that you think are necessary.
# 
# Part 2
# 1.	Use the “mtcars” data set to conduct the hypothesis testing that the mean of mpg (miles per gallon) is larger than 15.
# 2.	Use the “mtcars” data set to test whether the means of wt (weight) of the cars are different in the automatic and manual (am, 0 = automatic, 1 = manual) cases.
# 3.	The “sleep” data set shows the effect of two soporific drugs (increase in hours of sleep compared to control) on 10 patients. Use hypothesis testing to show whether these two drugs have significant difference.

# 1. employee distribution

slices <- c(45, 35, 20, 18, 2)  
lbls <- c("Hunan", "Hubei", "Zhejiang", "Fujian", "Xinjiang") 
pie(slices, labels = lbls,     
    main="Employees distributions") 

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
marker2 <- rep('31~45 years old',times = 70)
marker3 <- rep('46~60 years old', times = 40)
Groups <- c(marker1, marker2, marker3)

  
mydataframe <- data.frame(Groups, Ages, Wages,  stringsAsFactors = FALSE)
mydataframe
mydataframe$Groups <- factor(mydataframe$Groups)

library(ggplot2)
ggplot(data=mydataframe, aes(x=Ages, y=Wages,color=Groups, main = "Relationships Between Ages and Wages")) + 
  geom_point(size=3) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Relationships Between Ages and Wages")




# 3. generating t-distributed samples and plotting density functions
library("sm")
sample1 <- rt(n = 10000, df = 5)
sample2 <- rt(n = 10000, df = 10)
sample3 <- rt(n = 10000, df = 30)
sample4 <- rnorm(n = 10000, mean = 0, sd = 1)
sample <- c(sample1, sample2, sample3, sample4)

marker1 <- rep("sample1 s.t. t Distribution" , 10000)
marker2 <- rep("sample2 s.t. t Distribution", 10000)
marker3 <- rep("sample3 s.t. t Distribution", 10000)
marker4 <- rep("Sample4 s.t. Normal Distribution",10000)
SampleGropus <- c(marker1,marker2,marker3,marker4)

mydataframe1 <- data.frame(SampleGropus, sample)
library(ggplot2)
ggplot(data = mydataframe1, aes(x = sample, fill=SampleGropus))+ geom_density(alpha=.3)  +
labs(title="Density Plot",             
     x="Values of Different Samples", 
     y="Density") +
  theme(plot.title = element_text(hjust = 0.5)) #####ggbest标题居中专用


# 4. t test for mean of mpg
t.test(mtcars$mpg, mu = 15, alternative = "greater")

# 5. t test for means of wt of automatic and manual

am_column_manual <- subset(mtcars, am == 1)
am_column_automatic <- subset(mtcars,am == 0)
subset_of_am_column_manual <- am_column_manual["wt"]
subset_of_am_column_automatic <- am_column_automatic["wt"]
t.test(am_column_automatic,am_column_manual,alternative = c("two.sided"))

# 6. t test for two sets of drugs

set1 <- subset(sleep, group == 1)
set2 <- subset(sleep, group == 2)
subset1 <- set1["extra"]
subset2 <- set2["extra"]
t.test(subset1,subset2,alternative = c("two.sided"))
