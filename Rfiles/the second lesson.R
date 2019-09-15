x <- seq(1, 10, 2)
x <- 1:7
x <- -c(1,2,3,4,5,6)

x <- c(2,4,5)
b <- c(2,5,5)
b[c(1,2)]
b <-c("ih","we","df")
b[-c(1,2)]
b <- c("one", "two", "three") 
y <- matrix(1:20,4,5)
y
x <- matrix(1:10, nrow = 2)
x
x <- matrix(1:10, 2,5,byrow = TRUE)
x
dim1 <- c("a1","a2")
dim2 <- c("b1","b2","b3")
dim3 <- c("c1","c2","c3","c4")
z <- array(1:24, c(2,3,4),dimnames = list(dim1, dim2, dim3))
z
patientID <- c(1,2,3,4)
age <- c(23,54,23,54)
diabetes <- c("fdf","wsd","we","dfda")
patientdata <- data.frame(patientID, age, diabetes)
patientdata[1:2]
patientdata[c("age","patientID")]
table(patientdata$age, patientdata$patientID)
str(patientdata)


attach(mtcars)
summary(mtcars)
plot(mpg, disp)
plot(mpg, wt)
detach(mtcars)

# be aware that you shuould not use comma between different orders in the following structures
#and that you should use different line to seperate orders instead.
with(mtcars, {print(summary(mpg)) 
  plot(mpg, disp)
  plot(mpg, wt)})

with(mtcars, {
  keepstats <<- summary(mpg)
})
keepstats

# It is said that the row.name order is used to lable the variables 
patientdata <- data.frame(patientID, age, diabetes, row.names = "patientID")
patientdata

# notice that factor are used to store variables as integer vectors
# how to turn nominal variables into ordinary variables by using factor function.
status <- c("poor", "improved", "excellent")
status <- factor(status, ordered = TRUE)
status
# how to reset the order.
status <- factor(status, ordered = TRUE, levels = c("poor","improve","excellent"))
#sex <- factor(sex, levels = c(1,2), labels = c("male", "female"))
status

# practice of using factor
patientID <- c(1, 2, 3, 4) 
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1") 
status <- c("Poor", "Improved", "Excellent", "Poor") 
diabetes <- factor(diabetes) 
status <- factor(status, order=TRUE) 
patientdata <- data.frame(patientID, age, diabetes, status) 
str(patientdata)

# crearing lists
d <- "This is a list"
e <- c("one","two","three","four")
f <- c(2,4,5,6)
h <- matrix(1:10, 2,5, byrow = TRUE)
z <- list(title = d, summary = e,f,h)
z

# how to import csv files to R
grades <- read.table("students.csv", header =TRUE, sep = ",")


grades

