#Assignment 3
#By Chen Ming
##Part 1

###1.1.	Find descriptive statistics of the data and summarize them into a table.
library("readxl")
######### mydata<-read.csv(file.choose())
######### mydata
newdata <- read.csv(file = "alumni_giving_rate.csv", header = TRUE)
newdata


###1.2.	Use graphical analysis (such as scatterplot) to investigate the relationship between Alumni Giving Rate and each of the other variables.
par(mfrow=c(2,2))
fit1 <- lm(Alumni.Giving.Rate~Graduation.Rate,data = newdataframe)
plot1 = plot( newdataframe$Graduation.Rate,newdataframe$Alumni.Giving.Rate,
             main = "Relationships Between Alumni Giving Rates and Graduation Rate",
             xlab = "Graduation Rate",
             ylab = "Alumni Giving Rate",
             pch = 4
)
lines(newdataframe$Graduation.Rate, fitted(fit1), lty = 1, col = "green")

fit2 <- lm(Alumni.Giving.Rate~Number.of.Classes.Under.20,data = newdataframe)
plot2 = plot(newdataframe$Number.of.Classes.Under.20,newdataframe$Alumni.Giving.Rate, 
             main = "Relationships Between Alumni Giving Rates and Number.of.Classes.Under.20",
             xlab = "Number.of.Classes.Under.20",
             ylab = "Alumni Giving Rate",
             pch = 4
)
lines(newdataframe$Number.of.Classes.Under.20, fitted(fit2), lty = 1, col = "green")

fit3 <- lm(Alumni.Giving.Rate~Student.Faculty.Ratio,data = newdataframe)
plot3 = plot(newdataframe$Student.Faculty.Ratio,newdataframe$Alumni.Giving.Rate, 
             main = "Relationships Between Alumni Giving Rates and Student.Faculty.Ratio",
             xlab = "Student.Faculty.Ratio",
             ylab = "Alumni Giving Rate",
             pch = 4
)
lines(newdataframe$Student.Faculty.Ratio, fitted(fit3), lty = 1, col = "green")
#### From  
###1.3.	Develop a multiple linear regression model that could be used to predict the Alumni Giving Rate using the data provided. This may include model specification and estimation. Summarize your findings with evidence and reasoning (possibly from the previous question).
####method 1
multilinearregression1 = lm(Alumni.Giving.Rate~Graduation.Rate+Number.of.Classes.Under.20+ Student.Faculty.Ratio,data = newdataframe)
summary(multilinearregression1)

multilinearregression2 = lm(Alumni.Giving.Rate~Graduation.Rate+ Student.Faculty.Ratio,data = newdataframe)
summary(multilinearregression2)

anova(multilinearregression1,multilinearregression2)
anova(multilinearregression2,multilinearregression1)
AIC(multilinearregression2,multilinearregression1)

####method 2
library(MASS)
multilinearregression1 = lm(Alumni.Giving.Rate~Graduation.Rate+Number.of.Classes.Under.20+ Student.Faculty.Ratio,data = newdataframe)
stepAIC(multilinearregression1, direction = "backward")
newmodel = lm(Alumni.Giving.Rate ~ Graduation.Rate + Student.Faculty.Ratio, 
              data = newdataframe)
newmodel
summary(newmodel)
#######sum of Sq? RSS?
###1.4.	Check the model assumptions.
par(mfrow = c(2,2))
chechmodel = plot(newmodel)


##Part 2

###2.1.	Calculate the mean of Fertility, and then partition the provinces into two groups, with group 1 including the provinces having above average Fertility measure, and group 2 including the remaining provinces. Use variable y to denote this group information.

FertilityCol = swiss["Fertility"]
mean_fertility = mean(as.matrix(FertilityCol))
mean_fertility

# subset1 <- subset(swiss, Fertility >= mean_fertility)
# group1 <- rownames(subset1)
# group1
# 
# subset2 <- subset(swiss, Fertility < mean_fertility)
# group2 <- rownames(subset2)
# y = group2
# y

swiss$ynprovinces[swiss$Fertility > mean_fertility]<- 1
swiss$ynprovinces[swiss$Fertility < mean_fertility] <- 0
group1 <- subset(swiss,ynprovinces == 1)
group1
group2 <- subset(swiss,ynprovinces == 0)
group2
y = as.matrix(swiss$ynprovinces)
y
###2.2.	Set the group 2 as the baseline group, and use logistic regression to show the relationship between y and the other variables except Fertility. Interpret the regression results.
lgfit <- glm(formula = y~Agriculture+Examination+Education+Catholic+Infant.Mortality, family = binomial(),data = swiss)
summary(lgfit)
#***********significance level is one-sided*********** 
###2.3.	Choose a model selection criterion, for instances, AIC, BIC, adjusted R square or Cp, and use it to select a reasonable model. 
library(MASS)
multilinearregression4 <- lm(y~Agriculture+Examination+Education+Catholic+Infant.Mortality, data = swiss)
stepAIC(multilinearregression4, direction = "backward")
newmodel2 <-  lm(y ~ Agriculture + Examination + Infant.Mortality,  data = swiss)
newmodel2                
                
