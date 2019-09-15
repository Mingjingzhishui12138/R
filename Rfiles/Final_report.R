# Part 1 
##a)Report descriptive statistics of the data set obtained in (a).
library(MASS)
newdataset = na.omit(survey)
summary(newdataset)

##b)Use boxplot to show the distributions of the height of male and female students.
male=subset(newdataset,Sex=='Male')
male_height=male['Height']

female=subset(newdataset,Sex=='Female')
female_height=female['Height']
par(mfrow=c(1,2))
boxplot(male_height,
        main='Distribution of Male Height')
boxplot(female_height,
        main='Distribution of Female Height')

##c)Which numerical variables might have an influence on the student's pulse?
W.Hnd<- as.matrix(newdataset$W.Hnd)
W.Hnd[which(W.Hnd=='Right')]<- 0
W.Hnd[which(W.Hnd=='Left')]<- 1
W.Hnd<- as.numeric(W.Hnd)
W.Hnd

Smoke<- as.matrix(newdataset$Smoke)
Smoke[which(Smoke=='Never')]<- 0
Smoke[which(Smoke=='Regul')]<- 1
Smoke[which(Smoke=='Occas')]<- 2
Smoke[which(Smoke=='Heavy')]<- 3
Smoke<- as.numeric(Smoke)
Smoke

Clap<- as.matrix(newdataset$Clap)
Clap[which(Clap=='Right')]<- 0
Clap[which(Clap=='Left')]<- 1
Clap[which(Clap=='Neither')]<- 2
Clap<- as.numeric(Clap)
Clap

Exer<- as.matrix(newdataset$Exer)
Exer[which(Exer=='Some')]<- 0
Exer[which(Exer=='Freq')]<- 1
Exer[which(Exer=='None')]<- 2
Exer<- as.numeric(Exer)
Exer

Fold<- as.matrix(newdataset$Fold)
Fold[which(Fold=='R on L')]<- 0
Fold[which(Fold=='L on R')]<- 1
Fold[which(Fold=='Neither')]<- 2
Fold<- as.numeric(Fold)
Fold

Sex<- as.matrix(newdataset$Sex)
Sex[which(Sex=='Male')]<- 0
Sex[which(Sex=='Female')]<- 1
Sex<- as.numeric(Sex)
Sex

newdataset1<- subset(newdataset,select = c(Age, Height, Wr.Hnd, Pulse))
newdataset1['Sex']<- Sex
newdataset1['Fold']<- Fold
newdataset1['Exer']<- Exer
newdataset1['Clap']<- Clap
newdataset1['W.Hnd']<- W.Hnd
newdataset1['Smoke']<- Smoke

cor(newdataset1)
cor.test(newdataset1$Pulse,newdataset1$Height)
cor.test(newdataset1$Pulse,newdataset1$Age)
cor.test(newdataset1$Pulse,newdataset1$Wr.Hnd)
cor.test(newdataset1$Pulse,newdataset1$Sex)
cor.test(newdataset1$Pulse,newdataset1$Fold)
cor.test(newdataset1$Pulse,newdataset1$Exer)
cor.test(newdataset1$Pulse,newdataset1$Clap)
cor.test(newdataset1$Pulse,newdataset1$W.Hnd)
cor.test(newdataset1$Pulse,newdataset1$Smoke)

##d)Is the probability of a student clapping his/her left hand on top less than 0.2?
a <- length(newdataset$Clap)
b <- sum(newdataset$Clap=='Left')
prop.test(b,a,alternative = 'less', p=.2)

##e)Is the span of the writing hand in general larger than the span of the non-writing hand?
t.test(newdataset$Wr.Hnd,newdataset$NW.Hnd,alternative = 'greater')


# Part 2
##a)According to CLT, what is the approximated distribution of the sample means?
newmatrix=matrix(0,nrow=200, ncol=1)
count <- 1
while(count < 201){
  a = rexp(10,5)
  meanvalue=mean(a)
  newmatrix[count,]<- meanvalue
  count <- count+1
}
newmatrix
##b)Draw the density plots of the sample means and its approximated distribution on one graph.

library(sm)
dist <- c(1:400)
dist[1:200] <- newmatrix
dist[201:400]<- rexp(200,5)
tag <- c(rep('1',times=200),rep('2',times=200))
anewdataframe <- data.frame(tag,dist)
tag.f<-factor(tag,levels = c('1','2'), labels=c('mean_value_dist','expo_dist'))
sm.density.compare(anewdataframe$dist, anewdataframe$tag)
title(main="Density Plot") 
legend("topright",c("sample means distribution", "approximate distribution"),
       lty=c(1, 2), col=c("red", "green"))
##c)Show the qq plot of the distribution of the sample means.
qqnorm(newmatrix)

#Part 3
##a)Draw 5000 random samples of ????.Show the density plot of the samples.
matrix1<- matrix(0,5000,1)
count1 <-1
while(count1 < 5000){
  if (runif(1)<0.75){
    matrix1[count1,]=rt(1,df=4);
    count1=count1+1
  }else{if(runif(1)<0.6){
    matrix1[count1,]<- rexp(1,2)}else{
    matrix1[count1,]<- runif(1,-3,3)};
    count1=count1+1}
}
matrix1[which(matrix1>10.3)]<- 10.3
plot(density(matrix1),main='Density Plot')

##b)find the VaR of the samples obtained in a).
a=sort(matrix1)
VaR=quantile(a,.05)
VaR

##c)Find the CVaR of the samples obtained in a).
mean(a[1:250])


#Part 4

newdata <- read.csv(file='customer_analysis.csv',stringsAsFactors = FALSE)
##data exploration
summary(newdata)
plot(density(newdata$Purchase),main = 'Distribution of Purchase')

par(mfrow=c(1,2))
num1<- sum(newdata=='F')
num2<- sum(newdata=='M')
slices<- c(num1,num2)
lbls <- c('Female','Male')
pct <- round(slices/sum(slices)*100)
lbls2<-paste(lbls,' ', pct,'%',sep = '')
pie(slices, labels=lbls2, col=rainbow(length(lbls2)), main='Pie Chart for Gender')

num1<- sum(newdata$Marital_Status==1)
num2<- sum(newdata$Marital_Status==0)
slices <- c(num1,num2)
lbls <- c('Married','Unmarried')
pct <- round(slices/sum(slices)*100)
lbls2<-paste(lbls,' ', pct,'%',sep = '')
pie(slices, labels = lbls2,col=rainbow(length(lbls2)), main='Pie Chart for Marital Status')

num1<- sum(newdata=='18-25')
num2<- sum(newdata=='26-35')
num3<- sum(newdata=='36-45')
num4<- sum(newdata=='46-50')
num5<- sum(newdata=='51-55')
num6<- sum(newdata=='55+')
slices<- c(num1,num2,num3,num4,num5,num6)
lbls <- c('18-25','26-35','36-45','46-50','51-55','55+')
pct <- round(slices/sum(slices)*100)
lbls2<-paste(lbls,' ', pct,'%',sep = '')
pie(slices,lbls2,col=rainbow(length(lbls2)), main='Pie Chart for Age')

num1<- sum(newdata=='A')
num2<- sum(newdata=='B')
num3<- sum(newdata=='C')
slices <-c(num1,num2,num3)
lbls<- c('city A','city B','city C')
pct <- round(slices/sum(slices)*100)
lbls2<-paste(lbls,' ', pct,'%',sep = '')
pie(slices,lbls2,col=rainbow(length(lbls2)), main='Pie Chart for City')

num1<- sum(newdata$Stay_In_Current_City_Years==0)
num2<- sum(newdata$Stay_In_Current_City_Years==1)
num3<- sum(newdata$Stay_In_Current_City_Years==2)
num4<- sum(newdata$Stay_In_Current_City_Years==3)
num5<- sum(newdata$Stay_In_Current_City_Years=='4+')
slices<- c(num1,num2,num3,num4,num5)
lbls<- c('0 year','1 year','2 years','3years','4+ years')
pct <- round(slices/sum(slices)*100)
lbls2<-paste(lbls,' ', pct,'%',sep = '')
pie(slices, labels = lbls2, col=rainbow(length(lbls2)), main = 'Pie Chart for Stay-in-current-city Years')

##conducting correlation analysis
par(mfrow=c(1:2))
x <- newdata$Age
x[which(x=='0-17')]<- 1
x[which(x=='18-25')]<- 2 
x[which(x=='26-35')]<- 3
x[which(x=='36-45')]<- 4
x[which(x=='46-50')]<- 5
x[which(x=='51-55')]<- 6
x[which(x=='55+')]<- 7
x<- as.numeric(x)
y<- newdata$Purchase
testdata <- data.frame(x,y, stringsAsFactors = FALSE)
cor(testdata$x, testdata$y, method = 'kendall')

x1 <- newdata$City_Category
x1[which(x1=='A')]<- 1
x1[which(x1=='B')]<- 2
x1[which(x1=='C')]<- 3
x1<- as.numeric(x1)
testdata <- data.frame(x1,y,stringsAsFactors = FALSE)
cor(testdata$x1,testdata$y, method='kendall')

x2<- newdata$Gender
x2[which(x2=='M')]<- 1
x2[which(x2=='F')]<- 2
x2<- as.numeric(x2)
testdata <- data.frame(x2,y,stringsAsFactors = FALSE)
cor(testdata$x2,testdata$y, method='kendall')

x3<- newdata$Stay_In_Current_City_Years
x3[which(x3==0)]<- 1
x3[which(x3==1)]<- 2
x3[which(x3==2)]<- 3
x3[which(x3==3)]<- 4
x3[which(x3=='4+')]<- 5
x3<- as.numeric(x3)
testdata <- data.frame(x3,y,stringsAsFactors = FALSE)
cor(testdata$x3,testdata$y, method='kendall')

x4<- newdata$Marital_Status
testdata <- data.frame(x4,y,stringsAsFactors = FALSE)
cor(testdata$x4,testdata$y,method='kendall')

par(mfrow=(1:2))
boxplot(newdata$Purchase~newdata$Age,main='Age')
boxplot(newdata$Purchase~newdata$Gender,main='Gender')
boxplot(newdata$Purchase~newdata$City_Category,main='City Category')
boxplot(newdata$Purchase~newdata$Stay_In_Current_City_Years,main='Stay-in-Current-City Years')
boxplot(newdata$Purchase~newdata$Marital_Status,main='Marital Status')

##modeling
fit1 <- lm(newdata$Purchase~newdata$City_Category)
summary(fit1)

fit2<- lm(newdata$Purchase~newdata$City_Category+newdata$Gender)
summary(fit2)

fit3<- lm(newdata$Purchase~newdata$City_Category+newdata$Gender+newdata$Age)
summary(fit3)

fit4<- lm(newdata$Purchase~newdata$City_Category+newdata$Gender+newdata$Age+newdata$Stay_In_Current_City_Years)
summary(fit4)

fit5<- lm(newdata$Purchase~newdata$City_Category+newdata$Gender+newdata$Age+newdata$Stay_In_Current_City_Years+newdata$Marital_Status)
summary(fit5)

fitted_model <- fit4
summary(fitted_model)
fitted_model

library(MASS)
stepAIC(fit5,direction = 'backward')


par(mfrow=c(2,2))
plot(fit5)


