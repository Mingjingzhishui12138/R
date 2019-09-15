#Assignmengt 2#

#Question 1: Pie chart
slices <- c(80, 45 ,35, 20, 18, 2)
lbls <- c("Guangdong", "Hunan", "Hubei", "Zhejiang", "Fujian", "Xinjiang")
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", sep="")
pie(slices, labels=lbls2, col=rainbow(length(lbls2)), main="Pie chart with Percentages")

#Question 2:
#establish group data
X1 <- runif(90, 22, 30) #sample命令，作出三组dataframe,然后将这三组数据合成一组，再进行plot)
X2 <- runif(70, 31, 45)
X3 <- runif(40, 46, 60)
Y1 <- rnorm(90, 8000, 1000)
Y2 <- rnorm(70, 15000, 2000)
Y3 <- rnorm(40, 20000, 1500)
#scatter plot
plot(X1, Y1, xlim=c(20,65), ylim=c(0,35000),col="blue", main="Association relationship of GroupX", xlab="Ages", ylab="Salaries", pch=20)
points(X2, Y2, col="red", pch=20)
points(X3, Y3, col="green", pch=20)
legend("topleft", title="Group", c("22~30", "31~45", "46~60"), pch=c(20, 20, 20), col=c("blue", "red", "green"))

#Question3:
t_distA <- rt(100000,5)
t_distB <- rt(100000, 10)
t_distC <- rt(100000, 30)
n_norm <- rnorm(100000, 0, 1)
plot(density(t_distA), xlim=c(-8,8), col="blue", lty=4, ylim=c(0,0.45),
main="Density distribution")

lines(density(t_distB), col="yellow", lty=3)
lines(density(t_distC), col="green", lty=1)
lines(density(n_norm), col="red", lty=2)
legend("topright", title="Dist.Type", c("norm", "t.dist df=5", "t.dist df=10", "t.dist df=30"),
lty=c(4, 3, 1, 2), col=c("blue", "yellow", "green", "red"))


#Part2--Question1:Hypothesis
t.test(mtcars$mpg, alternative=("greater"), mu=15)

#Part2--Question2:
#the samples in mtcars are independent
t.test(wt ~ am, data=mtcars, alternative="two.sided")

#Part2--Question3:
t.test(extra ~ group, data=sleep, conf.level=0.95)