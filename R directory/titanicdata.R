train <- read.csv('train.csv',header=TRUE)
test <- read.csv('test.csv',header = TRUE)
View(test)
View(train)


test.survived <- data.frame(Survived = rep('None', nrow(test)), test[,])
test.survived <- test.survived[,c(2,1,3,4,5,6,7,8,9,10,11,12)]
data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Survived<- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)

table(data.combined$Pclass)





library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill = factor(Survived)))+
  geom_bar(width=.5)+
  xlab('Pclass')+
  ylab('Total Count')+
  labs(fill='Survived')

View(train$Name)
head(as.character(train$Name))
tail(as.character(train$Name))
head(train$Name)

dup.name <- as.character(data.combined[which(duplicated(data.combined$Name)),'Name'])

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, 'Miss.')),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, 'Mrs.')),]
mrses[1:5,]

males<- data.combined[which(train$Sex=='male'),]
males <- data.combined[which(str_detect(train$Sex,'male')),]
males[,]

a<- grep('Miss.',train$Name)
length(a)

#create a utility function to detect the relationship between survived and pclass
extractTitle <- function(Name){
  Name <- as.character(Name)
  if(length(grep('Miss.', Name))>0){
    return('Miss.')}
  else if(length(grep('Mr.',Name))>0){
    return('Mr.')}
  else if(length(grep('Mrs.', Name))>0){
    return('Mrs.')}
  else if(length(grep('Master.', Name))>0){
    return('Master.')}
  else{
    return('Other')}
}

titles <- NULL

for (i in 1:nrow(data.combined)){
  titles<- c(titles, extractTitle(data.combined[i,'Name']))
}



data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$titles <- as.factor(titles)
ggplot(data.combined[1:891,],aes(x=titles, fill=Survived))+
         geom_bar()+
         facet_wrap(~Pclass)+
         ggtitle('Pclass')+
         xlab('Titles')+
         ylab('Total Count')+
       labs(fill='Survived')

table(data.combined$Sex)       

ggplot(data.combined[1:891,],aes(x=Sex, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle('Pclass')

summary(data.combined$Age)
ggplot(data.combined[1:891,],aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Pclass+Sex)+
  ggtitle('Pclass')
  
summary(data.combined[1:891,'Age'])
#because too many blank in data of age, we are gonna use its proxy to test whether it is predicative

#validate that 'Master' is a good proxy for male children
boys <- data.combined[which(data.combined$titles=='Master.'),]
summary(boys$Age)

misses <- data.combined[which(data.combined$titles=='Miss.'),]


ggplot(misses[misses$Survived != 'None',],aes(x=Age, fill=Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)+
  ggtitle('Pclass')
         
misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone)
length(which(misses.alone$Age <= 14.5))
which(misses.alone$Age <= 14.5)

summary(data.combined$SibSp)
# we are curious about whether the SibSp can be converted to factor, hence:
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived))+
  geom_histogram(stat='count')+
  ggtitle('Pclass, Title')+
  facet_wrap(~Pclass+titles)+
  xlab('SibSp')+
  ylab('Total Count')
  
data.combined$Parch <- data.combined$Parch
ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived))+
  geom_histogram(stat='count')+
  ggtitle('Pclass, Title')+
  facet_wrap(~Pclass+titles)+
  xlab('Parch')+
  ylab('Total Count')

#try some feature engineering. 
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$FamilySize <- as.factor(temp.parch+temp.sibsp+1)
View(data.combined)

ggplot(data.combined[1:891,], aes(x=FamilySize, fill=Survived))+
  geom_histogram(stat='count')+
  facet_wrap(~Pclass+ titles)+
  ggtitle('Pclass, Title')+
  xlab('Family Size')+
  ylab("Total Count")
#*******************the third video*********************
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket=='', ' ', substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

data.combined$ticket.first.char <- as.factor(ticket.first.char)
ggplot(data.combined[1:891,],aes(x=ticket.first.char, fill=Survived))+
  geom_histogram(stat='count')+
  facet_wrap(~Pclass+titles)
  ggtitle("Survivability by ticket.first.char")+
  ylim(0,350)+
  labs(fill='Survived')

length(unique(data.combined$Fare))  
summary(data.combined$Fare)
#cannot make fare a factor, treat as numeric&visualize with histogram
ggplot(data.combined[1:891,],aes(x=Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle('Combined Fare Distribution')+
  xlab('Fare')+
  ylab('Total Count')+
  ylim(0,200)
####note that only continuous value can use 'binwidth'  
  
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)  
data.combined$Cabin[1:100]#to make sense the data

####replace blank with'U' for 'Unknown'
data.combined[which(data.combined$Cabin==""),'Cabin']<- 'U'
data.combined$Cabin


cabin.first.char<- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ylim(0,750)

#what about folks with multiple cabins?
#####take close look at the following sentence
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), 'Y', 'N'))
data.combined$cabin.multiple

ggplot(data.combined[1:891,],aes(x=cabin.multiple, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+titles)+
  ylim(0,350)+
  labs(fill='Survived')


#############understanding: first take a look at the structure of the data, deciding whether to take it as factor or character; We can also summarize different ,
#############we can also summarize character into several factors by extracting certain patterns; after that, we can use ggplot to see whether there are something interesting 
#############happen if we put the known significant variable with the current variable(such as using facet_wrap), and then determine wether to add it to the model about to fit.(in order to prevent overfitting)

str(data.combined$Embarked)
levels(data.combined$Embarked)
ggplot(data.combined[1:891,],aes(x=Embarked, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+titles)+
  ylim(0,350)+
  labs(fill='Survived')
###if all the orange is in "Q", that is gonna be interesting. But currently we are not going to add 'Embarkment' to the model


#***********Exploratory Modeling**********
rf.train.1 <- data.combined[1:891,c('Pclass','titles')]
rf.label <- as.factor(train$Survived)

set.seed(1234)
library('randomForest')
rf.1 <- randomForest(x=rf.train.1, y=rf.label, importance=TRUE, ntree=1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c('Pclass', 'titles','SibSp')]
  
               
set.seed(1234)
rf.2 <- randomForest(x=rf.train.2, y=rf.label, ntree=1000, importance=TRUE)
rf.2
varImpPlot(rf.2)

rf.train.3<- data.combined[1:891, c('Pclass','titles','SibSp','Parch')]
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3, y=rf.label, ntree=1000, importance=TRUE)
rf.3
varImpPlot(rf.3)

rf.train.4<- data.combined[1:891, c('Pclass','titles','FamilySize')]
set.seed(1234)
rf.4 <- randomForest(x=rf.train.4, y=rf.label, ntree=1000, importance=TRUE)
rf.4
varImpPlot(rf.4)


#####Summary: we can start with the modelling of the dicision tree with the variables that apprently useful;
####and then we can construct the basic model with more varibles to see whether the error rate goes up; At last
####we need to choose the most indicative model with the help of error rate(and varImpPlot)

#***************cross validataion****************
# cross validation is for maximizing the value of the data; estimate how good your model is for unseen data
# cross validation is the most important one while you are trying using machine into data analysis
test.submit.df <- data.combined[892:1309, c('Pclass','titles','FamilySize')]
rf.4.preds <- predict(rf.4, test.submit.df)
table(rf.4.preds)

submit.df <- data.frame(PassangerID=rep(892:1309), Survived=rf.4.preds)
View(submit.df)

write.csv(submit.df, file='RF_SUB_20160215_1.CSV',row.names = FALSE)

library(lattice)
library(ggplot2)
library(caret)
help(package='caret')
library(foreach)
library(iterators)
library(snow)
library(doSNOW)
set.seed(2348)
cv.10.fold <- createMultiFolds(rf.label, k=10, times=10)
