#question1
vector1 <- c(1,2,3,4)
vector2 <- c('a','b','c','d')
vector3 <- c(FALSE, FALSE, TRUE, TRUE)
length(vector3)
vector1[1]
vector1[3]
vector1[4]

#question2
rowname <- paste("R", 1:2, sep = "")
columnname <- paste("C", 1:5, sep = "")
a = matrix(1:10, nrow = 2, ncol = 5, byrow=FALSE, dimnames = list(rowname, columnname))
a
a[6]
a[2,5]
a[1,]
a[,2]

#question3
dim1 <- c(1,2,3)
dim2 <- c(4,5,6)
dim3 <- c(7,8,9)
dim4 <- c(10,11,12)
z = array(1:81, c(3,3,3,3), dimnames = list(dim1, dim2, dim3,dim4))
z
refer <- z[81]
refer

#question4
names <- c("niko","jack","tommy","franklin","trevor","name6","name7","name8","name9","name10","name11","name12","name13","name14","name15")
birthday <- c("1950-1-1", "1960-1-1", "1970-1-1", "1980-1-1", "1990-1-1","1990-1-2","1990-1-3","1990-1-4","1990-1-5","1990-1-6","1990-1-7","1990-1-8","1990-1-9","1990-1-10","1990-1-11")
transformedbirthday <- as.Date(birthday, "%Y-%m-%d")
gender <- c( "male","male","female","female","female","female","female","female","female", "male", "male","female","female","female", "male")
age <- c(69,59,49,39,29,29,29,29,29,29,29,29,29,29,29)
salary <- c(100, 110, 120, 130, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140)
mydataframe <- data.frame(names, birthday, gender,age,salary)
mydataframe
summary(mydataframe)
subset <- mydataframe$names[age < 50 & age > 30 & gender == 'male'] 
subset
#question5
newlist <- c('a', '12','$10000','c','d')
newlist1 <-newlist
names(newlist)[2] <- c("age")
names(newlist)[3] <- c("salary")
newlist
newlist[5]

#question6
list <- c("A","B","C","D","E")
withorder <- factor(list, ordered = TRUE, levels = c("A","B","C","D","E"))
withoutorder <- list
withorder
withoutorder


#question7
dataset1 = runif(100,0,100)
dataset2 = runif(100,0,100)
dataset1
dataset2
mean(dataset1)
var(dataset1)
sd(dataset1)
mean(dataset2)
var(dataset2)
sd(dataset2)
range1 <- quantile(dataset1, c(0.75))-quantile(dataset1,c(0.25))
range2 <- quantile(dataset2, c(0.75))-quantile(dataset2,c(0.25))
range1
range2
cov(dataset1,dataset2)
cor(dataset1, dataset2)

#question8

set.seed(1)
x <- seq(1,10000,length.out = 10000)
y1 <- dt(x, 3, 0)
y1
mean(y1)
var(y1)
y2 <- dt(x, 10, 0)
y2
mean(y2)
var(y2)
y3 <- dt(x, 20, 0)
y3
mean(y3)
var(y3)
z <- rnorm(10000)
plot(x,z)


#question9
b = matrix(rnorm(500, mean = -2, sd = sqrt(3)),nrow = 100, ncol = 50, byrow = FALSE )
c = sum(b[b < 1 & b >-1])
c
b[abs(b) > 1] = 0
b


#question 10
x1 <- c(2,4) 
y1 <- c(6,8)
x2 <- c(1,2,3)
y2 <- c(1,2,3,4)
myfunction <- function(arg1, arg2)
  if(length(arg1) == length(arg2)){
    a1 = mean(arg1);
    a2 = mean(arg2);
    b = cov(arg1,arg2);
    c = var(arg1,arg2);
    print(a1);
    print(a2);
    print(b);
    print(c)}else{
      print("The lengths of the two arguments are not equal")
      }
myfunction(x1,y1)
myfunction(x2,y2)


  