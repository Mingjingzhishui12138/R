library(kernlab)
data(spam)
head(spam)

plot(density(spam$your[spam$type=='nonspam']),col='red',main='',xlab='Frequency of "your"',ylab='Density')
lines(density(spam$your[spam$type=='spam']),col='blue')
abline(v=.5,col='black')

prediction <- ifelse(spam$your>.5, 'spam', 'nonspam')
table(prediction,spam$type)
table(prediction,spam$type)/length(spam$type)


library(kernlab)
data(spam)
set.seed(233)
samplespam <- spam[sample(dim(spam)[1],size = 10),]
sampLabel <- (samplespam$type == 'spam')*1+1
plot(samplespam$capitalAve,col = sampLabel)

# My practice
data(iris)
head(iris)
sampleIris <- iris[sample(dim(iris)[1],size = 20),]
sampleLabel <- (sampleIris$Species=='setosa')+1
plot(sampleIris$Sepal.Length,col = sampleLabel)
library(ggplot2)
ggplot(data=sampleIris,aes(y=sampleIris$Sepal.Length,x=sampleIris$Sepal.Width,colour = factor(sampleIris$Species)))+geom_point()+geom_line()+theme(legend.position = 'none')
